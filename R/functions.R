# Configurations for {crew.cluster} controllers
# On the COH HPRCC

#-----------------------------------------------------------------------------

#' Package Options for hprcc
#'
#' The **hprcc** package offers configurable options, which can be set using \code{\link[base]{options}}
#' or in a user's `.Rprofile`. These options initialize based on environment variables, user settings in `.Rprofile`,
#' or fall back to cluster-specific defaults.
#'
#' @section Usage:
#' In `.Rprofile`: \code{options(hprcc.option_name = value)} for persistent settings across R sessions.
#' 
#' Environment Variables: Certain options can be automatically set based on specific environment variables, if present.
#' 
#' Defaults: Each option has a default value specific to the Apollo or Gemini cluster.
#'
#' @section Options:
#' - \code{hprcc.log_slurm}: Controls SLURM job scripts logging. \cr Default: \code{FALSE}
#' - \code{hprcc.slurm_script_dir}: Directory path for storing SLURM job scripts. \cr Default: \code{tempdir()}
#' - \code{hprcc.r_libs_user}: Path to user R libraries. \cr Default set by \code{R_LIBS_SITE} or \code{"~/R/bioc-3.17"}
#' - \code{hprcc.r_libs_site}: Site-specific library path. \cr Default set by \code{R_LIBS_USER} \cr Apollo default: \code{"/opt/singularity-images/rbioc/rlibs/bioc-3.17"} \cr Gemini default: \code{"/packages/singularity/shared_cache/rbioc/rlibs/bioc-3.17"}
#' - \code{hprcc.singularity_bin}: Path to the Singularity binary. \cr Apollo default: \code{"/opt/singularity/3.7.0/bin/singularity"} \cr  Gemini default: \code{"/packages/easy-build/software/singularity/3.7.0/bin/singularity"}
#' - \code{hprcc.singularity_container}: Path to the Singularity image. \cr Default set by \code{SINGULARITY_CONTAINER} \cr Apollo default: \code{"/opt/singularity-images/rbioc/vscode-rbioc_3.17.sif"} \cr Gemini default: \code{"/packages/singularity/shared_cache/rbioc/vscode-rbioc_3.17.sif"}
#' - \code{hprcc.bind_dirs}: Directories to bind in the Singularity container. \cr Default set by \code{SINGULARITY_BIND} \cr Apollo default: \code{"/labs,/opt/singularity,/opt/singularity-images"} \cr Gemini default: \code{"/packages/singularity,/ref_genomes,/scratch"}
#' - \code{hprcc.default_partition}: Default SLURM partition. \cr Apollo default: \code{"fast,all"} \cr Gemini default: \code{"defq"}
#'
#' @name package-options
#' @aliases package-options
#' @docType package
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
    } else if (grepl("^g-[a-z]-[0-9]-[0-9]-[0-9]{2}", hostname)) {
        return("gemini")
    } else {
        warning("Unknown cluster")
    }
}


#' Set Up a Controller for SLURM Jobs on COH Clusters
#'
#' Configures and initializes a controller for managing SLURM jobs on City of Hope clusters.
#' The function leverages the `crew.cluster` package to facilitate job execution, managing
#' resources such as CPU, memory, walltime, and directing SLURM logs and scripts.
#'
#' @param name A unique identifier for the controller.
#' @param slurm_cpus Number of CPU cores allocated to each task.
#' @param slurm_mem_gigabytes Memory allocated to each task, in gigabytes.
#' @param slurm_walltime_minutes Maximum allowed execution time per task, in minutes. Defaults to 720 (12 hours).
#' @param slurm_workers Total number of parallel tasks the controller can handle. Defaults to 350.
#' @param slurm_partition SLURM partition for job submission, varying by cluster. See [package options](reference/package-options.html) for defaults.
#' @param slurm_log_dir Directory path for storing SLURM logs if `option(hprcc.log_slurm = TRUE)`. Defaults to "logs".
#'
#' @details
#' `create_controller` streamlines SLURM job setup on COH clusters using
#' Singularity containers for consistent computing environments. Singularity containers
#' package software and dependencies, ensuring that jobs run reliably across different
#' clusters like Apollo and Gemini. This approach aids in computational reproducibility and
#' solves environment inconsistencies in cluster computing.
#'
#' The function allows customization of SLURM job parameters, including CPUs, memory,
#' and walltime, while managing SLURM logs and script directories. It abstracts cluster-specific
#' configurations, making it easier to run jobs without detailed knowledge of the underlying
#' cluster setup. This functionality is especially useful in environments with varying
#' resource paths and setups, simplifying job execution across diverse computational platforms.
#'
#' @return A `crew_controller` object, ready to manage SLURM job submissions and monitoring.
#' @export
#' @examples
#' if (interactive()) {
#'     create_controller("my_controller", 4, 8)
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
    job_id <- Sys.getenv("SLURM_JOB_ID")
    nodename <- Sys.info()["nodename"]
    r_libs_user <- getOption("hprcc.r_libs_user")
    r_libs_site <- r_libs_site()
    slurm_script_dir <- getOption("hprcc.slurm_script_dir", tempdir())
    singularity_bin <- singularity_bin()
    singularity_bind_dirs <- singularity_bind_dirs()
    singularity_image <- singularity_image()
    # Logging
    log_slurm <- getOption("hprcc.log_slurm")
    if (isTRUE(log_slurm)) dir.create(here::here(slurm_log_dir), showWarnings = FALSE, recursive = TRUE) else NULL
    log_output <- if (isTRUE(log_slurm)) here::here(glue::glue("{slurm_log_dir}/slurm-%j.out")) else NULL
    log_error <- if (isTRUE(log_slurm)) here::here(glue::glue("{slurm_log_dir}/slurm-%j.err")) else NULL
    # script directory
    if (slurm_script_dir != tempdir()) {
        dir.create(slurm_script_dir, showWarnings = FALSE, recursive = TRUE)
    }

    script_lines <- glue::glue("#SBATCH --mem {slurm_mem_gigabytes}G \
cd {getwd()} \
{singularity_bin} exec \\
--env R_LIBS_USER={r_libs_user} \\
--env R_LIBS_SITE={r_libs_site} \\
-B {singularity_bind_dirs} \\
{singularity_image} \\")

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
    if (get_cluster() == "apollo") {
        return(getOption("hprcc.r_libs_site_apollo"))
    } else {
        return(getOption("hprcc.r_libs_site_gemini"))
    }
}

singularity_bin <- function() {
    if (get_cluster() == "apollo") {
        return(getOption("hprcc.singularity_bin_apollo"))
    } else {
        return(getOption("hprcc.singularity_bin_gemini"))
    }
}

# get this from env - SINGULARITY_CONTAINER
singularity_image <- function() {
    if (get_cluster() == "apollo") {
        return(getOption("hprcc.singularity_image_apollo"))
    } else {
        return(getOption("hprcc.singularity_image_gemini"))
    }
}

# get this from env - SINGULARITY_BIND
singularity_bind_dirs <- function() {
    if (get_cluster() == "apollo") {
        return(getOption("hprcc.bind_dirs_apollo"))
    } else {
        return(getOption("hprcc.bind_dirs_gemini"))
    }
}

default_partition <- function() {
    if (get_cluster() == "apollo") {
        return(getOption("hprcc.default_partition_apollo"))
    } else {
        return(getOption("hprcc.default_partition_gemini"))
    }
}

# Default Targets options ----------------------------------------------------
#' @importFrom targets tar_option_set
#' @importFrom targets tar_resources
#' @importFrom crew crew_controller_group
configure_targets_options <- function() { # Targets options
    targets::tar_option_set(
        format = "qs",
        storage = "worker",
        retrieval = "worker",
        controller = crew::crew_controller_group(
            # tiny
            create_controller(name = "tiny", slurm_cpus = 1, slurm_mem_gigabytes = 1, slurm_walltime_minutes = 60),
            # small
            create_controller("small", 2, 20, 360),
            # medium
            create_controller("medium", 12, 80, 360),
            # large
            create_controller("large", 20, 200),
            # big memory job
            create_controller("bigmem", 10, 500, 360),
            # huge
            create_controller("huge", 40, 100, 120)
        ),
        resources = targets::tar_resources(
            crew = targets::tar_resources_crew(controller = "small")
        )
    )
}

# -----------------------------------------------------------------------------
.onLoad <- function(libname, pkgname) {
    # hprcc options
    options(
        hprcc.r_libs_user = "~/R/bioc-3.17",
        hprcc.slurm_script_dir = tempdir(),
        hprcc.r_libs_site_apollo = "/opt/singularity-images/rbioc/rlibs/bioc-3.17",
        hprcc.r_libs_site_gemini = "/packages/singularity/shared_cache/rbioc/rlibs/bioc-3.17",
        hprcc.singularity_bin_apollo = "/opt/singularity/3.7.0/bin/singularity",
        hprcc.singularity_bin_gemini = "/packages/easy-build/software/singularity/3.7.0/bin/singularity",
        hprcc.singularity_image_apollo = "/opt/singularity-images/rbioc/vscode-rbioc_3.17.sif",
        hprcc.singularity_image_gemini = "/packages/singularity/shared_cache/rbioc/vscode-rbioc_3.17.sif",
        hprcc.bind_dirs_apollo = "/labs,/opt/singularity,/opt/singularity-images",
        hprcc.bind_dirs_gemini = "/packages/singularity,/ref_genomes,/scratch",
        hprcc.default_partition_apollo = "fast,all",
        hprcc.default_partition_gemini = "defq",
        hprcc.log_slurm = TRUE)

    # Set targets options
    configure_targets_options()

}
