# Configurations for {crew.cluster} controllers
# On the COH HPRCC

#-----------------------------------------------------------------------------

#' Package Options for hprcc
#'
#' The **hprcc** package has a number of configurable options, providing the flexibility to use it with any
#' containerized environment supporting R and `{targets>=1.4.2}`.
#'
#' Settings are read from \code{\link[base]{options}} in an `.Rprofile` file, located either in your
#' `$HOME` directory or within the project directory. `.Rprofile` settings take precedence over
#' environment variables, where indicated below. If no `options` are set, the default configuration
#' runs the [RStudio for Bioconductor](http://hprcc.coh.org/user-guide/rbioc/) container.
#'
#' @section Options:
#' - \code{hprcc.log_slurm}: Controls SLURM job logging. \cr Default: \code{FALSE}
#' - \code{hprcc.slurm_script_dir}: Path to write SLURM job scripts. \cr Default: \code{tempdir()}
#' - \code{hprcc.r_libs_user}: Path to user R libraries. \cr Default set by \code{R_LIBS_SITE} or \code{"~/R/bioc-3.17"}
#' - \code{hprcc.r_libs_site}: Site-specific library path. \cr Default set by \code{R_LIBS_USER} \cr Apollo default: \code{"/opt/singularity-images/rbioc/rlibs/bioc-3.17"} \cr Gemini default: \code{"/packages/singularity/shared_cache/rbioc/rlibs/bioc-3.17"}
#' - \code{hprcc.singularity_bin}: Path to the Singularity binary. \cr Apollo default: \code{"/opt/singularity/3.7.0/bin/singularity"} \cr  Gemini default: \code{"/packages/easy-build/software/singularity/3.7.0/bin/singularity"}
#' - \code{hprcc.singularity_container}: Path to the Singularity image. \cr Default set by \code{SINGULARITY_CONTAINER} \cr Apollo default: \code{"/opt/singularity-images/rbioc/vscode-rbioc_3.17.sif"} \cr Gemini default: \code{"/packages/singularity/shared_cache/rbioc/vscode-rbioc_3.17.sif"}
#' - \code{hprcc.bind_dirs}: Directories to bind in the Singularity container. \cr Default set by \code{SINGULARITY_BIND} \cr Apollo default: \code{"/labs,/opt,/ref_genome"} \cr Gemini default: \code{"/packages/singularity,/ref_genomes,/scratch"}
#' - \code{hprcc.default_partition}: Default SLURM partition. \cr Apollo default: \code{"fast,all"} \cr Gemini default: \code{"defq"}
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
#'   get_cluster()
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
#' @param slurm_partition SLURM partition for job submission. Default set by cluster. See [package options](../reference/package-options.html) for defaults.
#' @param slurm_log_dir Path for storing SLURM logs when `option(hprcc.log_slurm = TRUE)`. Defaults to "logs" in the project directory.
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
#'   create_controller("my_controller", slurm_cpus = 4, slurm_gigabytes = 8)
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
                              slurm_partition = default_partition(),
                              slurm_log_dir = "logs") {
    # GPU check
    if (grepl("gpu", slurm_partition)) {
        if (get_cluster() != "gemini") {
            stop("GPU jobs are only supported on the Gemini cluster.")
        }
        if (slurm_cpus > 8) {
        stop("For GPU partitions, the number of CPUs must be less than or equal to 8.")
        }
        gpu_req <- glue::glue("#SBATCH --gres gpu:1 \n#SBATCH --ntasks=1 \n")
    } else {
        gpu_req <- ""
    }
  log_dir <- ifelse(slurm_log_dir == "logs", here::here("logs"), slurm_log_dir) 
  job_id <- Sys.getenv("SLURM_JOB_ID")
  nodename <- Sys.info()["nodename"]
  r_libs_user <- getOption("hprcc.r_libs_user", Sys.getenv("R_LIBS_USER"))
  r_libs_site <- r_libs_site()
  slurm_script_dir <- getOption("hprcc.slurm_script_dir", tempdir())
  singularity_bin <- singularity_bin()
  singularity_bind_dirs <- singularity_bind_dirs()
  singularity_container <- singularity_container()
  # Logging #TODO
  log_slurm <- getOption("hprcc.log_slurm", FALSE)
  if (isTRUE(log_slurm)) dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  log_output <- if (isTRUE(log_slurm)) glue::glue("{log_dir}/slurm-%j.out") else "/dev/null"
  log_error <- if (isTRUE(log_slurm)) glue::glue("{log_dir}/slurm-%j.err") else "/dev/null"
  # script directory #TODO
  if (slurm_script_dir != tempdir()) {
    dir.create(here::here(slurm_script_dir), showWarnings = FALSE, recursive = TRUE)
    slurm_script_dir <- here::here(slurm_script_dir)
  }

  script_lines <- glue::glue("#SBATCH --mem {slurm_mem_gigabytes}G \
#SBATCH --chdir {slurm_script_dir} \
{gpu_req} \
{singularity_bin} exec \\
--env R_LIBS_USER={r_libs_user} \\
--env R_LIBS_SITE={r_libs_site} \\
-B {singularity_bind_dirs} \\
{singularity_container} \\")

  crew.cluster::crew_controller_slurm(
    name = name,
    slurm_cpus_per_task = slurm_cpus,
    slurm_time_minutes = slurm_walltime_minutes,
    slurm_partition = slurm_partition,
    host = nodename,
    worker = slurm_workers,
    seconds_idle = 30,
    script_directory = slurm_script_dir,
    slurm_log_output = log_output,
    slurm_log_error = log_error,
    script_lines = script_lines
  )
}

# Internal functions ---------------------------------------------------------

r_libs_site <- function() {
  if (!is.null(getOption("hprcc.r_libs_site"))) {
    return(getOption("hprcc.r_libs_site"))
  } else if (nzchar(Sys.getenv("R_LIBS_SITE"))) {
    return(Sys.getenv("R_LIBS_SITE"))
  } else if (get_cluster() == "apollo") {
    return("/opt/singularity-images/rbioc/rlibs/bioc-3.17")
  } else if (get_cluster() == "gemini") {
    return("/packages/singularity/shared_cache/rbioc/rlibs/bioc-3.17")
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
    return("/opt/singularity-images/rbioc/vscode-rbioc_3.17.sif")
  } else if (get_cluster() == "gemini") {
    return("/packages/singularity/shared_cache/rbioc/vscode-rbioc_3.17.sif")
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
    return(c("/labs,/opt,/ref_genome"))
  } else if (get_cluster() == "gemini") {
    return(c("/packages,/ref_genomes,/scratch"))
  } else {
    warning("Unknown cluster, please set SINGULARITY_BIND env var or option")
  }
}

default_partition <- function() {
  if (!is.null(getOption("hprcc.default_partition"))) {
    return(getOption("hprcc.default_partition"))
  } else if (get_cluster() == "apollo") {
    return("fast,all")
  } else if (get_cluster() == "gemini") {
    return("defq")
  } else {
    warning("Unknown cluster, please set hprcc.default_partition env var or option")
  }
}

# Default Targets options ----------------------------------------------------
#' @importFrom targets tar_option_set
#' @importFrom targets tar_resources
#' @importFrom crew crew_controller_group
configure_targets_options <- function() {
    # Define the common controllers
  controllers <- list(
    create_controller(name = "tiny", slurm_cpus = 2, slurm_mem_gigabytes = 8, slurm_walltime_minutes = 60),
    create_controller("small", slurm_cpus = 2, slurm_mem_gigabytes = 20, slurm_walltime_minutes = 360),
    create_controller("medium", slurm_cpus = 4, slurm_mem_gigabytes = 40, slurm_walltime_minutes = 360),
    create_controller("large", slurm_cpus = 8, slurm_mem_gigabytes = 80, slurm_walltime_minutes = 360),
    create_controller("large_mem", slurm_cpus = 8, slurm_mem_gigabytes = 600, slurm_walltime_minutes = 360),
    create_controller("xlarge", slurm_cpus = 20, slurm_mem_gigabytes = 200),
    create_controller("huge", slurm_cpus = 40, slurm_mem_gigabytes = 200, slurm_walltime_minutes = 120)
  )
  
  # Conditionally add GPU controllers if on the 'gemini' cluster
  if (get_cluster() == "gemini") {
    gpu_controllers <- list(
      create_controller("gpu_medium", slurm_cpus = 4, slurm_mem_gigabytes = 60, slurm_walltime_minutes = 120, slurm_partition = "gpu"),
      create_controller("gpu_large", slurm_cpus = 8, slurm_mem_gigabytes = 120, slurm_walltime_minutes = 240, slurm_partition = "gpu")
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
  options(parallelly.availableCores.methods = "Slurm")
}

.onLoad <- function(libname, pkgname) {
  # Set parallelly options
  options(parallelly.availableCores.methods = "Slurm")
}

