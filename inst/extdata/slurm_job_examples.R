# Example scripts for run_slurm_job() and run_singularity_job()
# These demonstrate the "fire and track" pattern for running containerized tools
# outside the rbioc singularity environment, tracked via targets.
#
# IMPORTANT: working_dir must be on a shared filesystem (e.g., /scratch, /labs)
# NOT /tmp which is node-local and not visible across compute nodes.

library(hprcc)

# =============================================================================
# Container paths (Gemini cluster)
# =============================================================================
containers <- list(
    cellranger = "/packages/singularity/shared_cache/cellranger_latest.sif",
    star = "/packages/singularity/shared_cache/star_2.7.11b.sif",
    cellbender = "/packages/singularity/shared_cache/cellbender_latest.sif"
)

# =============================================================================
# Example 1: Get container versions (simple test)
# =============================================================================
# These can be run directly to verify containers are working
#
# Usage:
#   get_cellranger_version("/scratch/user/test")
#   # Wait for job to complete, then:
#   readLines("/scratch/user/test/version.txt")

get_cellranger_version <- function(working_dir) {
    run_singularity_job(
        name = "cellranger_version",
        container = containers$cellranger,
        command = "cellranger --version > version.txt",
        working_dir = working_dir,
        completion_files = "version.txt",
        slurm_options = list(time = "00:05:00", mem = "4G", cpus_per_task = 1L)
    )
}

get_star_version <- function(working_dir) {
    run_singularity_job(
        name = "star_version",
        container = containers$star,
        command = "STAR --version > version.txt",
        working_dir = working_dir,
        completion_files = "version.txt",
        slurm_options = list(time = "00:05:00", mem = "4G", cpus_per_task = 1L)
    )
}

get_cellbender_version <- function(working_dir) {
    run_singularity_job(
        name = "cellbender_version",
        container = containers$cellbender,
        command = "cellbender --version > version.txt",
        working_dir = working_dir,
        completion_files = "version.txt",
        slurm_options = list(time = "00:05:00", mem = "4G", cpus_per_task = 1L),
        gpu = FALSE # Version check doesn't need GPU
    )
}

# Test GPU partition submission (checks nvidia-smi in container)
test_gpu_container <- function(working_dir) {
    run_singularity_job(
        name = "gpu_test",
        container = containers$cellbender,
        command = "nvidia-smi > gpu_info.txt 2>&1",
        working_dir = working_dir,
        completion_files = "gpu_info.txt",
        slurm_options = list(time = "00:10:00", mem = "8G", cpus_per_task = 2L),
        gpu = TRUE # Sets partition=gpu-a100, gres=gpu:1, --nv flag
    )
}

# =============================================================================
# Example 2: Cell Ranger count (10X Genomics)
# =============================================================================
# For use in targets pipelines with pattern = map(sample_id)

#' Run Cell Ranger count for a single sample
#'
#' @param sample_id Sample identifier
#' @param fastq_dir Directory containing FASTQ files
#' @param transcriptome Path to Cell Ranger reference
#' @param output_dir Base output directory
#' @param threads Number of threads (default 8)
#' @param memory_gb Memory in GB (default 64)
#' @return Path to completion file or script
run_cellranger_count <- function(
    sample_id,
    fastq_dir,
    transcriptome,
    output_dir,
    threads = 8L,
    memory_gb = 64L
) {
    sample_dir <- file.path(output_dir, sample_id)

    # Build cellranger command
    cmd <- glue::glue(
        "cellranger count",
        " --id={sample_id}",
        " --fastqs={fastq_dir}",
        " --transcriptome={transcriptome}",
        " --localcores=$SLURM_CPUS_PER_TASK",
        " --localmem={as.integer(memory_gb * 0.9)}" # Leave headroom
    )

    run_singularity_job(
        name = glue::glue("cellranger_{sample_id}"),
        container = containers$cellranger,
        command = cmd,
        working_dir = sample_dir,
        completion_files = c(
            "outs/filtered_feature_bc_matrix.h5",
            "outs/raw_feature_bc_matrix.h5"
        ),
        slurm_options = list(
            time = "24:00:00",
            mem = paste0(memory_gb, "G"),
            cpus_per_task = threads
        )
    )
}

# =============================================================================
# Example 3: STAR alignment (inDrops/bulk RNA-seq)
# =============================================================================

#' Build STAR index
#'
#' @param genome_fa Path to genome FASTA
#' @param gtf Path to GTF annotation
#' @param output_dir Output directory for index
#' @param threads Number of threads (default 8)
#' @return Path to completion file or script
build_star_index <- function(
    genome_fa,
    gtf,
    output_dir,
    threads = 8L
) {
    cmd <- glue::glue(
        "STAR --runMode genomeGenerate",
        " --genomeDir {output_dir}",
        " --genomeFastaFiles {genome_fa}",
        " --sjdbGTFfile {gtf}",
        " --runThreadN $SLURM_CPUS_PER_TASK",
        " --sjdbOverhang 100"
    )

    run_singularity_job(
        name = "star_index",
        container = containers$star,
        command = cmd,
        working_dir = output_dir,
        completion_files = "SA", # STAR index file
        slurm_options = list(
            time = "04:00:00",
            mem = "40G",
            cpus_per_task = threads
        )
    )
}

#' Run STAR alignment for a sample
#'
#' @param sample_id Sample identifier
#' @param r1_fastq Path to R1 FASTQ (can be comma-separated for multiple lanes)
#' @param r2_fastq Path to R2 FASTQ
#' @param star_index Path to STAR index directory
#' @param output_dir Output directory
#' @param threads Number of threads (default 4)
#' @return Path to completion file or script
run_star_alignment <- function(
    sample_id,
    r1_fastq,
    r2_fastq,
    star_index,
    output_dir,
    threads = 4L
) {
    sample_dir <- file.path(output_dir, sample_id)

    cmd <- glue::glue(
        "STAR --genomeDir {star_index}",
        " --readFilesIn {r1_fastq} {r2_fastq}",
        " --readFilesCommand zcat",
        " --runThreadN $SLURM_CPUS_PER_TASK",
        " --outFileNamePrefix {sample_dir}/",
        " --outSAMtype BAM SortedByCoordinate",
        " --outBAMsortingThreadN $SLURM_CPUS_PER_TASK",
        " --quantMode GeneCounts"
    )

    run_singularity_job(
        name = glue::glue("star_{sample_id}"),
        container = containers$star,
        command = cmd,
        working_dir = sample_dir,
        completion_files = "Aligned.sortedByCoord.out.bam",
        slurm_options = list(
            time = "04:00:00",
            mem = "40G",
            cpus_per_task = threads
        )
    )
}

# =============================================================================
# Example 4: CellBender (GPU job)
# =============================================================================

#' Run CellBender remove-background
#'
#' @param sample_id Sample identifier
#' @param input_h5 Path to Cell Ranger raw_feature_bc_matrix.h5
#' @param output_dir Output directory
#' @param expected_cells Expected number of cells
#' @param total_droplets Total droplets to consider (default 25000)
#' @param epochs Training epochs (default 150)
#' @return Path to completion file or script
run_cellbender <- function(
    sample_id,
    input_h5,
    output_dir,
    expected_cells,
    total_droplets = 25000L,
    epochs = 150L
) {
    sample_dir <- file.path(output_dir, sample_id)
    output_file <- file.path(sample_dir, "cellbender_output.h5")

    cmd <- glue::glue(
        "cellbender remove-background",
        " --input {input_h5}",
        " --output {output_file}",
        " --expected-cells {expected_cells}",
        " --total-droplets-included {total_droplets}",
        " --epochs {epochs}",
        " --cuda"
    )

    run_singularity_job(
        name = glue::glue("cellbender_{sample_id}"),
        container = containers$cellbender,
        command = cmd,
        working_dir = sample_dir,
        completion_files = "cellbender_output.h5",
        gpu = TRUE, # Enables --nv flag and sets partition=gpu-a100, gres=gpu:1
        slurm_options = list(
            time = "04:00:00",
            mem = "60G",
            cpus_per_task = 4L
        )
    )
}

# =============================================================================
# Example 5: Usage in targets pipeline
# =============================================================================
# Example _targets.R snippet showing dynamic branching

# library(targets)
# library(hprcc)
#
# list(
#     tar_target(samples, c("sample1", "sample2", "sample3")),
#
#     tar_target(
#         cellranger_result,
#         run_cellranger_count(
#             sample_id = samples,
#             fastq_dir = "/path/to/fastqs",
#             transcriptome = "/path/to/refdata-gex-GRCh38-2024-A",
#             output_dir = "/scratch/user/project/cellranger"
#         ),
#         pattern = map(samples),
#         cue = tar_cue(mode = "always"), # Always check completion
#         deployment = "main"             # Don't send to workers
#     ),
#
#     tar_target(
#         cellbender_result,
#         run_cellbender(
#             sample_id = samples,
#             input_h5 = file.path(cellranger_result, "outs/raw_feature_bc_matrix.h5"),
#             output_dir = "/scratch/user/project/cellbender",
#             expected_cells = 5000
#         ),
#         pattern = map(samples, cellranger_result),
#         cue = tar_cue(mode = "always"),
#         deployment = "main"
#     )
# )
