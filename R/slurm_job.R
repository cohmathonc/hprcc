# SLURM Job Submission for External Tools
# Fire-and-track pattern for running containerized tools via SLURM
# Designed for tools like cellranger, STAR that run outside rbioc

#' Submit a SLURM job with completion tracking
#'
#' Submits a shell command as a SLURM job and tracks completion via output files.
#' Implements a "fire and track" pattern for targets integration:
#' 1. Check if completion files exist (job already done) -> return completion path
#' 2. Check if script exists (job already submitted) -> return script path
#' 3. Otherwise, create script and submit via sbatch -> return script path
#'
#' Use with `tar_cue(mode = "always")` and `deployment = "main"` in targets.
#'
#' @param name Character. Job name used for script filename and SLURM job name.
#' @param command Character. The shell command to execute.
#' @param working_dir Character. Directory for script creation and job execution.
#' @param completion_files Character vector. Relative paths from working_dir
#'   that indicate successful completion. First found file is returned.
#' @param slurm_options Named list. SLURM parameters:
#'   - time: Wall time (e.g., "24:00:00")
#'   - mem: Total memory (e.g., "64G")
#'   - cpus_per_task: Number of CPUs (integer)
#'   - partition: SLURM partition (default from cluster)
#'   - gres: Generic resources (e.g., "gpu:1")
#'   - Additional options passed as --name=value
#' @param modules_to_load Character vector. Modules to load (e.g., "singularity").
#' @param env_vars Named list. Environment variables to export.
#' @param force_resubmit Logical. If TRUE, resubmit even if script exists.
#'
#' @return Path to completion file (if exists) or script path.
#'
#' @export
#' @importFrom cli cli_alert_success cli_alert_info cli_abort
#'
#' @examples
#' \dontrun{
#' # In _targets.R
#' tar_target(
#'   my_job,
#'   run_slurm_job(
#'     name = "my_analysis",
#'     command = "python script.py --input data.csv",
#'     working_dir = "/scratch/user/analysis",
#'     completion_files = "results/output.csv",
#'     slurm_options = list(time = "4:00:00", mem = "32G", cpus_per_task = 4L),
#'     modules_to_load = "python/3.10"
#'   ),
#'   cue = tar_cue(mode = "always"),
#'   deployment = "main"
#' )
#' }
run_slurm_job <- function(
    name,
    command,
    working_dir,
    completion_files,
    slurm_options = list(),
    modules_to_load = NULL,
    env_vars = NULL,
    force_resubmit = FALSE
) {
    # Input validation
    stopifnot(
        is.character(name), length(name) == 1L, nzchar(name),
        is.character(command), length(command) == 1L,
        is.character(working_dir), length(working_dir) == 1L,
        is.character(completion_files), length(completion_files) >= 1L,
        is.list(slurm_options),
        is.null(modules_to_load) || is.character(modules_to_load),
        is.null(env_vars) || is.list(env_vars),
        is.logical(force_resubmit), length(force_resubmit) == 1L
    )

    # Normalize paths
    working_dir <- normalizePath(working_dir, mustWork = FALSE)
    script_path <- file.path(working_dir, paste0(name, ".sh"))

    # 1. Check completion files
    if (!force_resubmit) {
        for (f in completion_files) {
            full_path <- file.path(working_dir, f)
            if (file.exists(full_path)) {
                cli::cli_alert_success("Job complete: {name}")
                return(full_path)
            }
        }
    }

    # 2. Check if script already exists (already submitted)
    if (file.exists(script_path) && !force_resubmit) {
        cli::cli_alert_info("Job already submitted: {name}")
        return(script_path)
    }

    # 3. Create working directory
    if (!dir.exists(working_dir)) {
        dir.create(working_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # 4. Generate and write script
    script_content <- generate_slurm_script(
        name = name,
        command = command,
        working_dir = working_dir,
        slurm_options = slurm_options,
        modules_to_load = modules_to_load,
        env_vars = env_vars
    )

    writeLines(script_content, script_path)
    Sys.chmod(script_path, mode = "0755")

    # 5. Submit job
    submit_result <- system2("sbatch", script_path, stdout = TRUE, stderr = TRUE)
    exit_status <- attr(submit_result, "status")

    if (!is.null(exit_status) && exit_status != 0) {
        cli::cli_abort(c(
            "sbatch failed with exit code {exit_status}",
            "x" = paste(submit_result, collapse = "\n")
        ))
    }

    cli::cli_alert_success("Submitted SLURM job: {name}")
    return(script_path)
}


#' Submit a Singularity container job via SLURM
#'
#' Convenience wrapper around [run_slurm_job()] for running commands inside
#' Singularity containers. Automatically handles bind paths and GPU settings.
#'
#' @inheritParams run_slurm_job
#' @param container Character. Path to the Singularity .sif file.
#' @param command Character. Command to run inside the container.
#' @param bind_paths Character vector. Additional paths to bind into container.
#'   Merged with cluster defaults (Apollo: /labs,/opt,/ref_genome,/run;
#'   Gemini: /packages,/run,/ref_genomes,/scratch).
#' @param gpu Logical. If TRUE, adds --nv flag, sets gres=gpu:1,
#'   and defaults partition to "gpu-a100".
#'
#' @return Path to completion file (if exists) or script path.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Cell Ranger count
#' tar_target(
#'   cellranger_result,
#'   run_singularity_job(
#'     name = paste0("cellranger_", sample_id),
#'     container = "/packages/singularity/shared_cache/cellranger.sif",
#'     command = paste(
#'       "cellranger count",
#'       "--id=", sample_id,
#'       "--fastqs=", fastq_dir,
#'       "--transcriptome=", ref_dir,
#'       "--localcores=$SLURM_CPUS_PER_TASK",
#'       "--localmem=58"
#'     ),
#'     working_dir = file.path(output_dir, sample_id),
#'     completion_files = "outs/filtered_feature_bc_matrix.h5",
#'     slurm_options = list(time = "24:00:00", cpus_per_task = 8L, mem = "64G")
#'   ),
#'   pattern = map(sample_id),
#'   cue = tar_cue(mode = "always"),
#'   deployment = "main"
#' )
#'
#' # GPU job (CellBender)
#' tar_target(
#'   cellbender_result,
#'   run_singularity_job(
#'     name = paste0("cellbender_", sample_id),
#'     container = "/packages/singularity/shared_cache/cellbender.sif",
#'     command = "cellbender remove-background --cuda ...",
#'     working_dir = output_dir,
#'     completion_files = "output.h5",
#'     gpu = TRUE,
#'     slurm_options = list(time = "4:00:00", cpus_per_task = 4L, mem = "60G")
#'   ),
#'   cue = tar_cue(mode = "always"),
#'   deployment = "main"
#' )
#' }
run_singularity_job <- function(
    name,
    container,
    command,
    working_dir,
    completion_files,
    bind_paths = NULL,
    gpu = FALSE,
    slurm_options = list(),
    env_vars = NULL,
    force_resubmit = FALSE
) {
    # Validate container path
    stopifnot(
        is.character(container), length(container) == 1L,
        is.logical(gpu), length(gpu) == 1L
    )

    if (!file.exists(container)) {
        cli::cli_abort("Container not found: {.path {container}}")
    }

    # Build bind paths: cluster defaults + user-specified + working_dir
    default_binds <- singularity_bind_dirs()
    if (is.null(default_binds)) {
        cli::cli_abort("Cannot determine singularity bind paths. Set SINGULARITY_BIND env var or hprcc.singularity_bind_dirs option.")
    }
    all_binds <- strsplit(default_binds, ",")[[1]]

    if (!is.null(bind_paths)) {
        all_binds <- unique(c(all_binds, bind_paths))
    }

    # Ensure working_dir is bound
    working_dir_norm <- normalizePath(working_dir, mustWork = FALSE)
    working_dir_root <- strsplit(working_dir_norm, "/")[[1]][2]
    if (!is.na(working_dir_root) && nzchar(working_dir_root)) {
        root_path <- paste0("/", working_dir_root)
        if (!root_path %in% all_binds) {
            all_binds <- c(all_binds, root_path)
        }
    }

    bind_string <- paste(all_binds, collapse = ",")

    # Build singularity command
    sing_bin <- singularity_bin()
    if (is.null(sing_bin)) {
        cli::cli_abort("Cannot determine singularity binary path. Set SINGULARITY_BIN env var or hprcc.singularity_bin option.")
    }
    nv_flag <- if (gpu) "--nv " else ""

    full_command <- sprintf(
        "%s exec %s-B %s %s %s",
        sing_bin,
        nv_flag,
        bind_string,
        container,
        command
    )

    # GPU-specific SLURM options
    if (gpu) {
        slurm_options$gres <- slurm_options$gres %||% "gpu:1"
        slurm_options$partition <- slurm_options$partition %||% "gpu-a100"
    }

    # Ensure singularity module is loaded (prepend to any user-provided modules)
    modules <- "singularity"

    run_slurm_job(
        name = name,
        command = full_command,
        working_dir = working_dir,
        completion_files = completion_files,
        slurm_options = slurm_options,
        modules_to_load = modules,
        env_vars = env_vars,
        force_resubmit = force_resubmit
    )
}


#' Generate SLURM script content
#'
#' @param name Job name
#' @param command Shell command to run
#' @param working_dir Working directory
#' @param slurm_options Named list of SLURM options
#' @param modules_to_load Character vector of modules
#' @param env_vars Named list of environment variables
#'
#' @return Character string with full script content
#' @keywords internal
generate_slurm_script <- function(
    name,
    command,
    working_dir,
    slurm_options,
    modules_to_load,
    env_vars
) {
    # Header
    lines <- c(
        "#!/bin/bash",
        paste0("#SBATCH --job-name=", name),
        "#SBATCH --export=NONE",
        "#SBATCH --get-user-env=L"
    )

    # SLURM options
    option_map <- list(
        time = "time",
        mem = "mem",
        cpus_per_task = "cpus-per-task",
        partition = "partition",
        gres = "gres",
        account = "account",
        qos = "qos"
    )

    for (opt_name in names(slurm_options)) {
        if (opt_name %in% names(option_map)) {
            sbatch_name <- option_map[[opt_name]]
        } else {
            sbatch_name <- gsub("_", "-", opt_name)
        }
        lines <- c(lines, sprintf("#SBATCH --%s=%s", sbatch_name, slurm_options[[opt_name]]))
    }

    # Log files
    lines <- c(
        lines,
        sprintf("#SBATCH --output=%s/slurm-%%j.out", working_dir),
        sprintf("#SBATCH --error=%s/slurm-%%j.err", working_dir)
    )

    # Setup section
    lines <- c(
        lines,
        "",
        "# --- Setup ---",
        'echo "Job started on $(hostname) at $(date)"',
        sprintf("cd %s || exit 1", working_dir)
    )

    # Modules
    if (!is.null(modules_to_load) && length(modules_to_load) > 0) {
        lines <- c(lines, "", "# --- Load Modules ---")
        for (mod in modules_to_load) {
            lines <- c(lines, sprintf("module load %s", mod))
        }
    }

    # Environment variables
    if (!is.null(env_vars) && length(env_vars) > 0) {
        lines <- c(lines, "", "# --- Environment ---")
        for (var_name in names(env_vars)) {
            lines <- c(lines, sprintf("export %s=%s", var_name, env_vars[[var_name]]))
        }
    }

    # Command execution with signal handling
    lines <- c(
        lines,
        "",
        "# --- Execute Command ---",
        paste0(command, " &"),
        "cmd_pid=$!",
        'trap "kill -TERM $cmd_pid 2>/dev/null" SIGTERM SIGINT',
        "wait $cmd_pid",
        "exit_code=$?",
        "",
        "# --- Completion ---",
        'echo "Job finished at $(date) with exit code $exit_code"',
        "exit $exit_code"
    )

    paste(lines, collapse = "\n")
}


# Null-coalescing operator (if not already defined)
`%||%` <- function(x, y) if (is.null(x)) y else x
