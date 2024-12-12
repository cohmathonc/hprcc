# Configurations for {crew.cluster} controllers
# On the COH HPRCC

#-----------------------------------------------------------------------------

#' Package Options for hprcc
#'
#' The **hprcc** package has a number of settings that can be configured
#' via \code{\link[base]{options}} or environment variables, providing
#' the flexibility to use it with any containerized environment supporting
#' R and `{targets>=1.9.1}`.
#'
#' Options can be set by calling \code{\link[base]{options}} _before_ loading the **hprcc** package in
#' `_targets.R`. Option settings take precedence over environment variables, where
#' indicated below. If no `options` are set, the default configuration
#' runs the [RStudio for Bioconductor](http://hprcc.coh.org/user-guide/rbioc/) container.
#'
#' @section Options:
#' \describe{
#'   \item{hprcc.slurm_logs}{logical. Enable SLURM job & autometric logging. If `TRUE`, logs are saved to
#'         \code{tar_store_path()/logs}. Logs capture the `stderr` and `stdout` of each SLURM job, and can be parsed by
#'         \code{autometric} package. Default: \code{FALSE}.}
#'   \item{hprcc.slurm_verbose}{logical. Show SLURM messages in the console. Default: \code{FALSE}}
#'   \item{hprcc.slurm_jobs}{logical. Write SLURM submission scripts to \code{tar_store_path()/jobs}; use the
#'         `targets` default of `$TMPDIR` if `FALSE`. Default: `TRUE`}
#'   \item{hprcc.r_libs_user}{Path to user R libraries. If not set, defaults to \code{R_LIBS_SITE} environment
#'         variable or the R default of "~/R/x86_64-pc-linux-gnu-library/%V".}
#'   \item{hprcc.r_libs_site}{Site-specific library path. Default set by \code{R_LIBS_USER}.
#'         Apollo default: \code{"/opt/singularity-images/rbioc/rlibs/bioc-VERSION"}.
#'         Gemini default: \code{"/packages/singularity/shared_cache/rbioc/rlibs/bioc-VERSION"}}
#'   \item{hprcc.singularity_bin}{Path to the Singularity binary.
#'         Apollo default: \code{"/opt/singularity/3.7.0/bin/singularity"}.
#'         Gemini default: \code{"/packages/easy-build/software/singularity/3.7.0/bin/singularity"}}
#'   \item{hprcc.singularity_container}{Path to the Singularity image.
#'         Default set by \code{SINGULARITY_CONTAINER}.
#'         Apollo default: \code{"/opt/singularity-images/rbioc/vscode-rbioc_VERSION.sif"}.
#'         Gemini default: \code{"/packages/singularity/shared_cache/rbioc/vscode-rbioc_VERSION.sif"}}
#'   \item{hprcc.bind_dirs}{Directories to bind in the Singularity container.
#'         Default set by \code{SINGULARITY_BIND}.
#'         Apollo default: \code{"/labs,/opt,/ref_genome"}.
#'         Gemini default: \code{"/packages/singularity,/ref_genomes,/scratch"}}
#'   \item{hprcc.default_partition}{Default SLURM partition.
#'         Apollo default: \code{"all"}.
#'         Gemini default: \code{"compute"}}
#' }
#'
#' @name package-options
#' @aliases hprcc-package
NULL

#' Determine Cluster Name Based on Hostname
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
#' Configures and initializes a
#' [`controller`](https://wlandau.github.io/crew.cluster/reference/crew_controller_slurm.html)
#' for managing SLURM jobs on City of Hope clusters using the
#' \{[`crew.cluster`](https://wlandau.github.io/crew.cluster/)\} package to facilitate job
#' execution, managing resources such as CPU, memory, walltime, and writing SLURM logs and scripts.
#'
#' @param name A unique identifier for the controller.
#' @param slurm_cpus Number of CPU cores allocated to each task.
#' @param slurm_mem_gigabytes Memory allocated to each task, in gigabytes.
#' @param slurm_walltime_minutes Maximum allowed execution time per task, in minutes. Defaults to 720 (12 hours).
#' @param slurm_workers Total number of parallel tasks the controller can handle. Defaults to 350.
#' @param slurm_partition SLURM partition for job submission. Default set by cluster.
#' See [package options](../reference/package-options.html) for defaults.
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
#' if (interactive()) {
#'     create_controller("my_controller", slurm_cpus = 4, slurm_gigabytes = 8)
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
        gpu_req <- ""
    }

    nodename <- Sys.info()["nodename"]

    r_libs_user <- if (nzchar(user_libs_path <- getOption("hprcc.r_libs_user", Sys.getenv("R_LIBS_USER")))) {
        glue::glue("--env R_LIBS_USER={user_libs_path}")
    } else {
        ""
    }
    r_libs_site <- r_libs_site()

    slurm_account <- if (nzchar(account <- getOption("hprcc.slurm_account", ""))) glue::glue("#SBATCH --account {account}") else ""

    singularity_bin <- singularity_bin()

    singularity_bind_dirs <- singularity_bind_dirs()

    singularity_container <- singularity_container()

    use_jobs_dir <- isTRUE(getOption("hprcc.slurm_jobs", TRUE))
    slurm_jobs_dir <- if (use_jobs_dir) here::here(glue::glue("{targets::tar_path_store()}/jobs")) else NULL
    if (use_jobs_dir) dir.create(slurm_jobs_dir, recursive = TRUE, showWarnings = FALSE)

    use_slurm_log <- isTRUE(getOption("hprcc.slurm_logs", FALSE))
    log_output <- here::here(glue::glue("{targets::tar_path_store()}/logs/slurm-%j.out"))
    if (use_slurm_log) {
        dir.create(dirname(log_output), recursive = TRUE, showWarnings = FALSE)
    } else {
        log_output <- "/dev/null"
    }

    verbose_slurm <- getOption("hprcc.slurm_verbose", FALSE)

    script_lines <- glue::glue(
    "{if (!is.null(gpu_req) && nzchar(gpu_req)) gpu_req else ''} ",
    "{if (!is.null(slurm_account) && nzchar(slurm_account)) slurm_account else ''} ",
    "{singularity_bin} exec {if (!is.null(r_libs_user) && nzchar(r_libs_user)) r_libs_user else ''} \\
--env R_LIBS_SITE={r_libs_site} \\
--env R_PARALLELLY_AVAILABLECORES_METHODS=Slurm \\
-B {singularity_bind_dirs} \\
{singularity_container} \\")

    slurm_options <- crew.cluster::crew_options_slurm(
        script_directory = slurm_jobs_dir,
        script_lines = script_lines,
        cpus_per_task = slurm_cpus,
        memory_gigabytes_required = slurm_mem_gigabytes,
        time_minutes = slurm_walltime_minutes,
        partition = slurm_partition,
        log_output = log_output,
        log_error = log_output,
        verbose = isTRUE(verbose_slurm)
    )

    crew.cluster::crew_controller_slurm(
        name = name,
        host = nodename,
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
}

default_partition <- function() {
    # Check for partition in options
    if (!is.null(getOption("hprcc.default_partition"))) {
        return(getOption("hprcc.default_partition"))
    } else {
        # Get the system default partition
        sys_default <- slurm_default_partition()
        if (!is.null(sys_default)) {
            return(sys_default)
        } else {
            warning("Could not determine default partition, please set hprcc.default_partition option")
            return(NULL)
        }
    }
}

# Default Targets options ----------------------------------------------------
configure_targets_options <- function() {
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
    # Set targets options
    configure_targets_options()
    # Set parallelly options
    if (nzchar(Sys.getenv("SLURM_JOB_ID"))) options(parallelly.availableCores.methods = "Slurm")
    # log hprcc settings to logs/hprcc_settings.txt if option(hprcc.slurm_logs = TRUE)
    log_hprcc_settings()
}

.onLoad <- function(libname, pkgname) {
    # Set parallelly options
    if (nzchar(Sys.getenv("SLURM_JOB_ID"))) options(parallelly.availableCores.methods = "Slurm")
}
