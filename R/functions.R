# Configurations for {crew.cluster} controllers
# On the COH HPRCC

#-----------------------------------------------------------------------------

#' Package Options for hprcc
#'
#' The **hprcc** package has a number of settings that can be configured
#' via [options()][base::options] or environment variables, providing
#' the flexibility to use it with any containerized environment supporting
#' R and [targets][targets::targets-package] (>=1.9.1).
#'
#' Options can be set by calling [options()][base::options] _before_ loading the **hprcc** package in
#' `_targets.R`. Option settings take precedence over environment variables, where
#' indicated below. If no `options` are set, the default configuration
#' runs the [RStudio for Bioconductor](http://hprcc.coh.org/user-guide/rbioc/) container.
#'
#' @section Options:
#' \describe{
#'   \item{hprcc.slurm_logs}{logical. Enable SLURM job & [autometric](https://wlandau.github.io/autometric/index.html)
#'         logging. If `TRUE`, logs are saved to `logs/` in [targets::tar_path_store()]. Logs capture the `stderr`
#'         and `stdout` of each SLURM job, and can be parsed by [autometric::log_read()]. \cr
#'         Default: `FALSE`.}
#'   \item{hprcc.slurm_verbose}{logical. Show SLURM messages in the console. \cr
#'         Default: `FALSE`}
#'   \item{hprcc.slurm_jobs}{logical. Write SLURM submission scripts to `jobs/ in `[targets::tar_path_store()]; use the
#'         default of `$TMPDIR` if `FALSE`. \cr
#'         Default: `FALSE`}
#'   \item{hprcc.slurm_account}{character. SLURM account for job submission. \cr
#'         Default: `$USER`}
#'   \item{hprcc.r_libs_user}{Path to user R libraries. \cr
#'         Environment: `$R_LIBS_USER` \cr
#'         Default: `"~/R/x86_64-pc-linux-gnu-library/%V"`}
#'   \item{hprcc.r_libs_site}{Site-specific library path. \cr
#'         Environment: `$R_LIBS_SITE` \cr
#'         Apollo default: `"/opt/singularity-images/rbioc/rlibs/bioc-$BIOCONDUCTOR_VERSION"` \cr
#'         Gemini default: `"/packages/singularity/shared_cache/rbioc/rlibs/bioc-$BIOCONDUCTOR_VERSION"`}
#'   \item{hprcc.singularity_bin}{Path to the Singularity binary. \cr
#'         Environment: `$SINGULARITY_BIN` \cr
#'         Apollo default: `"/opt/singularity/3.7.0/bin/singularity"` \cr
#'         Gemini default: `"/packages/easy-build/software/singularity/3.7.0/bin/singularity"`}
#'   \item{hprcc.singularity_container}{Path to the Singularity image. \cr
#'         Environment: `$SINGULARITY_CONTAINER` \cr
#'         Apollo default: `"/opt/singularity-images/rbioc/vscode-rbioc_$BIOCONDUCTOR_VERSION.sif"` \cr
#'         Gemini default: `"/packages/singularity/shared_cache/rbioc/vscode-rbioc_$BIOCONDUCTOR_VERSION.sif"`}
#'   \item{hprcc.bind_dirs}{Directories to bind in the Singularity container. \cr
#'         Environment: `$SINGULARITY_BIND` \cr
#'         Apollo default: `"/labs,/opt,/ref_genome,/run"` \cr
#'         Gemini default: `"/packages,/run,/ref_genomes,/scratch"`}
#'   \item{hprcc.default_partition}{Default SLURM partition. Automatically detected using \code{"scontrol show partition"}. \cr
#'         Default: Dynamically retrieved default partition from SLURM configuration.}
#' }
#'
#' @keywords package
#' @seealso \code{\link{create_controller}} for creating SLURM job controllers
#' @name package-options
#' @aliases hprcc-package
NULL

# Env for storing package settings
HPRCC <- new.env(parent = environment())

#' Determine Cluster Based on Hostname
#'
#' Retrieves the name of the COH HPRCC cluster by matching the system's hostname
#' against known patterns. It supports 'apollo' and 'gemini' clusters.
#'
#' @return A character string: either "apollo" or "gemini".
#' @export
#' @examples
#' if (interactive()) {
#'     get_cluster()
#' }
#' @note This function will throw an error if the hostname does not match any known cluster pattern.
get_cluster <- function() {
    hostname <- as.character(Sys.info()["nodename"])
    if (grepl("ppxhpc", hostname)) {
        return("apollo")
    } else if (grepl("^g-[a-z]-[0-9]-[0-9]-[0-9]{2}|^gemini", hostname)) {
        return("gemini")
    } else {
        warning("Unknown cluster")
    }
}

#' Set Up a Controller for SLURM Jobs on COH Clusters
#'
#' Configures and initializes a [controller][crew.cluster::crew_controller_slurm]
#' for managing SLURM jobs on City of Hope clusters using the [crew.cluster][crew.cluster::crew.cluster-package]
#' package to facilitate job execution, managing resources such as CPU, memory, walltime, and
#' writing SLURM logs and scripts.
#'
#' @param name A unique identifier for the controller.
#' @param slurm_cpus Number of CPU cores allocated to each task.
#' @param slurm_mem_gigabytes Memory allocated to each task, in gigabytes.
#' @param slurm_walltime_minutes Maximum allowed execution time per task, in minutes. Defaults to 720 (12 hours).
#' @param slurm_workers Total number of parallel tasks the controller can handle. Defaults to 350.
#' @param slurm_partition SLURM partition for job submission. Default set by cluster.
#' See [package options][package-options] for defaults.
#'
#' @details
#' `create_controller` streamlines SLURM job setup on COH clusters using
#' Singularity containers for consistent computing environments. Singularity containers
#' package software and dependencies, ensuring that jobs run reliably across both
#' clusters. This approach aids in computational reproducibility and
#' solves environment inconsistencies between Apollo and Gemini.
#'
#' The function allows customization of SLURM job parameters, including CPUs, memory,
#' and walltime, while managing SLURM logs and script directories. It abstracts cluster-specific
#' configurations, making it easier to run jobs without detailed knowledge of the underlying
#' cluster setup. This functionality is especially useful in environments with varying
#' resource paths and setups, simplifying job execution across the two platforms.
#'
#' @return A `crew_controller` object, ready to manage SLURM job submissions and monitoring.
#' @export
#' @examples
#' \dontrun{
#'  # Basic controller with minimal resources
#'  ctrl <- create_controller("test",
#'                         slurm_cpus = 2,
#'                         slurm_mem_gigabytes = 8)
#' }
#' # GPU configuration on Gemini
#' \dontrun{
#'  if (get_cluster() == "gemini") {
#'       gpu_ctrl <- create_controller("gpu_job",
#'                               slurm_cpus = 4,
#'                               slurm_mem_gigabytes = 60,
#'                               slurm_partition = "gpu-a100")
#'  }
#' }
#'
#' # Retry controller with escalating resources
#' \dontrun{
#' retry_ctrl <- create_controller("retry",
#'                               slurm_cpus = c(2, 4, 8),
#'                               slurm_mem_gigabytes = c(8, 16, 32))
#' }
#' @importFrom glue glue
#' @importFrom here here
#' @importFrom crew.cluster crew_controller_slurm
#' @seealso \code{\link[crew.cluster]{crew_controller_slurm}} for more on SLURM controllers.
create_controller <- function(name,
                              slurm_cpus,
                              slurm_mem_gigabytes,
                              slurm_walltime_minutes = 720L,
                              slurm_workers = 350L,
                              slurm_partition = default_partition()) {
    # GPU check
    if (grepl("gpu", slurm_partition)) {
        if (get_cluster() != "gemini") {
            stop("GPU jobs are only supported on the Gemini cluster.")
        }
        if (slurm_cpus > 8L) {
            stop("For GPU partitions, the number of CPUs must be less than or equal to 8.")
        }
        gpu_req <- glue::glue("#SBATCH --gres gpu:1 \n#SBATCH --ntasks=1 \n")
    } else {
        gpu_req <- NULL
    }

    script_lines <- glue::glue(
        "{if (!is.null(gpu_req) && nzchar(gpu_req)) gpu_req else '\n'} ",
        "{HPRCC$slurm_account}\n",
        "cd {here::here()} \n",
        "{HPRCC$singularity_bin} exec {HPRCC$r_libs_user} \\
--env R_LIBS_SITE={HPRCC$r_libs_site} \\
--env R_PARALLELLY_AVAILABLECORES_METHODS=Slurm \\
-B {HPRCC$singularity_bind_dirs} \\
{HPRCC$singularity_container} \\"
    )

    slurm_options <- crew.cluster::crew_options_slurm(
        script_directory = HPRCC$slurm_jobs_dir,
        script_lines = script_lines,
        cpus_per_task = slurm_cpus,
        memory_gigabytes_required = slurm_mem_gigabytes,
        time_minutes = slurm_walltime_minutes,
        partition = slurm_partition,
        log_output = HPRCC$log_output,
        log_error = HPRCC$log_output,
        verbose = HPRCC$verbose_slurm
    )

    crew.cluster::crew_controller_slurm(
        name = name,
        workers = slurm_workers,
        seconds_idle = 30L,
        garbage_collection = TRUE,
        options_cluster = slurm_options,
        options_metrics = crew::crew_options_metrics(path = "/dev/stdout", seconds_interval = 1L)
    )
}


# Internal functions ---------------------------------------------------------

r_libs_site <- function() {
    if (!is.null(getOption("hprcc.r_libs_site"))) {
        return(getOption("hprcc.r_libs_site"))
    } else if (nzchar(Sys.getenv("R_LIBS_SITE"))) {
        return(Sys.getenv("R_LIBS_SITE"))
    } else if (get_cluster() == "apollo") {
        return(glue::glue("/opt/singularity-images/rbioc/rlibs/bioc-{Sys.getenv('BIOCONDUCTOR_VERSION')}"))
    } else if (get_cluster() == "gemini") {
        return(glue::glue("/packages/singularity/shared_cache/rbioc/rlibs/bioc-{Sys.getenv('BIOCONDUCTOR_VERSION')}"))
    } else {
        warning("Unknown cluster, please set R_LIBS_SITE env var or option")
    }
}

singularity_bin <- function() {
    if (!is.null(getOption("hprcc.singularity_bin"))) {
        return(getOption("hprcc.singularity_bin"))
    } else if (nzchar(Sys.getenv("SINGULARITY_BIN"))) {
        return(Sys.getenv("SINGULARITY_BIN"))
    } else if (get_cluster() == "apollo") {
        return("/opt/singularity/3.7.0/bin/singularity")
    } else if (get_cluster() == "gemini") {
        return("/packages/easy-build/software/singularity/3.7.0/bin/singularity")
    } else {
        warning("Unknown cluster, please set SINGULARITY_BIN env var or option")
    }
}

singularity_container <- function() {
    if (!is.null(getOption("hprcc.singularity_container"))) {
        return(getOption("hprcc.singularity_container"))
    } else if (nzchar(Sys.getenv("SINGULARITY_CONTAINER"))) {
        return(Sys.getenv("SINGULARITY_CONTAINER"))
    } else if (get_cluster() == "apollo") {
        return(glue::glue("/opt/singularity-images/rbioc/vscode-rbioc_", Sys.getenv("BIOCONDUCTOR_VERSION"), ".sif"))
    } else if (get_cluster() == "gemini") {
        return(glue::glue("/packages/singularity/shared_cache/rbioc/vscode-rbioc_{Sys.getenv('BIOCONDUCTOR_VERSION')}.sif"))
    } else {
        warning("Unknown cluster, please set SINGULARITY_CONTAINER env var or option")
    }
}

singularity_bind_dirs <- function() {
    if (!is.null(getOption("hprcc.singularity_bind_dirs"))) {
        return(getOption("hprcc.singularity_bind_dirs"))
    } else if (nzchar(Sys.getenv("SINGULARITY_BIND"))) {
        return(Sys.getenv("SINGULARITY_BIND"))
    } else if (get_cluster() == "apollo") {
        return("/labs,/opt,/ref_genome,/run")
    } else if (get_cluster() == "gemini") {
        return("/packages,/run,/ref_genomes,/scratch")
    } else {
        warning("Unknown cluster, please set SINGULARITY_BIND env var or option")
    }
}

slurm_default_partition <- function() {
    # Try to run scontrol command, return NULL if it fails
    tryCatch(
        {
            cmd_output <- system("scontrol show partition", intern = TRUE)
            current_partition <- NULL
            # Process each line
            for (i in seq_along(cmd_output)) {
                line <- cmd_output[i]
                # If line starts with PartitionName, get the partition name
                if (grepl("^PartitionName=", line)) {
                    current_partition <- sub("PartitionName=([^ ]+).*", "\\1", line)
                } else if (!is.null(current_partition) && grepl("Default=YES", line)) {
                    return(current_partition)
                }
            }
            # Return NULL if no default partition found
            return(NULL)
        },
        error = function(e) {
            return(NULL)
        }
    )
}

default_partition <- function() {
    # Check for partition in options
    if (!is.null(getOption("hprcc.default_partition"))) {
        return(getOption("hprcc.default_partition"))
    }

    # Get the system default partition
    sys_default <- slurm_default_partition()
    if (!is.null(sys_default)) {
        return(sys_default)
    }

    warning("Could not determine default partition, please set hprcc.default_partition option")
    return(NULL)
}


# Default Targets options ----------------------------------------------------
#' @import autometric
#' @import qs2
configure_targets_options <- function() {
    # Populate the HPRCC environment
    HPRCC$r_libs_user <- if (nzchar(user_libs_path <- getOption("hprcc.r_libs_user", Sys.getenv("R_LIBS_USER")))) {
        glue::glue("--env R_LIBS_USER={user_libs_path}")
    } else {
        ""
    }
    HPRCC$r_libs_site <- r_libs_site()
    HPRCC$slurm_account <- if (nzchar(account <- getOption("hprcc.slurm_account", ""))) glue::glue("#SBATCH --account {account}") else ""
    HPRCC$singularity_bin <- singularity_bin()
    HPRCC$singularity_bind_dirs <- singularity_bind_dirs()
    HPRCC$singularity_container <- singularity_container()

    HPRCC$use_jobs_dir <- isTRUE(getOption("hprcc.slurm_jobs", FALSE))
    HPRCC$slurm_jobs_dir <- if (HPRCC$use_jobs_dir) here::here(glue::glue("{targets::tar_path_store()}/jobs")) else tempdir()
    if (HPRCC$use_jobs_dir) dir.create(HPRCC$slurm_jobs_dir, recursive = TRUE, showWarnings = FALSE)

    HPRCC$use_slurm_log <- isTRUE(getOption("hprcc.slurm_logs", FALSE))
    HPRCC$log_output <- here::here(glue::glue("{targets::tar_path_store()}/logs/crew-%j.out"))
    if (HPRCC$use_slurm_log) {
        dir.create(dirname(HPRCC$log_output), recursive = TRUE, showWarnings = FALSE)
    } else {
        HPRCC$log_output <- "/dev/null"
    }

    HPRCC$verbose_slurm <- isTRUE(getOption("hprcc.slurm_verbose", FALSE))

    # Define the common controllers
    controllers <- list(
        create_controller("tiny", slurm_cpus = 2L, slurm_mem_gigabytes = 8L, slurm_walltime_minutes = 60L),
        create_controller("small", slurm_cpus = 2L, slurm_mem_gigabytes = 20L, slurm_walltime_minutes = 360L),
        create_controller("medium", slurm_cpus = 4L, slurm_mem_gigabytes = 40L, slurm_walltime_minutes = 360L),
        create_controller("large", slurm_cpus = 8L, slurm_mem_gigabytes = 80L, slurm_walltime_minutes = 360L),
        create_controller("large_mem",
            slurm_cpus = 8L, slurm_mem_gigabytes = 800L, slurm_walltime_minutes = 360L,
            slurm_partition = ifelse(get_cluster() == "apollo", "all", "bigmem")
        ),
        create_controller("xlarge", slurm_cpus = 20L, slurm_mem_gigabytes = 200L),
        create_controller("huge", slurm_cpus = 40L, slurm_mem_gigabytes = 200L, slurm_walltime_minutes = 120L),
        create_controller("retry",
            slurm_cpus = c(2L, 4L, 8L, 20L, 40L),
            slurm_mem_gigabytes = c(8L, 20L, 40L, 80L, 120L, 200L),
            slurm_walltime_minutes = c(60L, 360L, 360L, 720L, 720L)
        )
    )

    # Conditionally add GPU controllers if on the 'gemini' cluster
    if (get_cluster() == "gemini") {
        gpu_controllers <- list(
            create_controller("gpu_medium", slurm_cpus = 4, slurm_mem_gigabytes = 60, slurm_walltime_minutes = 120, slurm_partition = "gpu-a100,gpu-v100"),
            create_controller("gpu_large", slurm_cpus = 8, slurm_mem_gigabytes = 120, slurm_walltime_minutes = 240, slurm_partition = "gpu-a100,gpu-v100")
        )
        controllers <- c(controllers, gpu_controllers)
    }

    # Targets options
    targets::tar_option_set(
        format = "qs",
        storage = "worker",
        retrieval = "worker",
        controller = do.call(crew::crew_controller_group, controllers),
        resources = targets::tar_resources(
        crew = targets::tar_resources_crew(controller = "small")
        )
    )
}

# -----------------------------------------------------------------------------
.onAttach <- function(libname, pkgname) {
    if (nzchar(Sys.getenv("SLURM_JOB_ID"))) {
        # Configure everything for SLURM environment
        configure_targets_options()
        options(parallelly.availableCores.methods = "Slurm")
        if (isTRUE(getOption("hprcc.slurm_logs", FALSE))) {
            log_hprcc_settings()
        }
    } else {
        packageStartupMessage("Note: This package is designed for use on the City of Hope High Performance Research Computing Cluster (HPRCC). Some functionality may be limited on other systems.")
    }
}

.onLoad <- function(libname, pkgname) {
    # Set parallelly options
    if (nzchar(Sys.getenv("SLURM_JOB_ID"))) options(parallelly.availableCores.methods = "Slurm")
}
