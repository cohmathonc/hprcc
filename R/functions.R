# Configurations for {crew.cluster} controllers
# On the COH HPRCC

#-----------------------------------------------------------------------------

#' Package Options
#'
#' This page documents the configurable options available in the **hprcc** package, settable using \code{\link[base]{options}}.
#'
#' @section log_slurm:
#' Controls whether SLURM job scripts logging is turned on or off. \cr
#' Default: \code{options(hprcc.log_slurm = FALSE)}
#'
#' @section r_libs_user:
#' Sets the path to user R libraries. \cr
#' Default: \code{options(hprcc.r_libs_user = "~/R/bioc-3.17")}
#'
#' @section r_libs_site_apollo:
#' Sets the site-specific library path for the Apollo cluster. \cr
#' Default: \code{options(hprcc.r_libs_site_apollo = "/opt/singularity-images/rbioc/rlibs/bioc-3.17")}
#'
#' @section r_libs_site_gemini:
#' Sets the site-specific library path for the Gemini cluster. \cr
#' Default: \code{options(hprcc.r_libs_site_gemini = "/packages/singularity/shared_cache/rbioc/rlibs/bioc-3.17")}
#'
#' @section singularity_bin_apollo:
#' Sets the path to the Singularity binary on the Apollo cluster. \cr
#' Default: \code{options(hprcc.singularity_bin_apollo = "/opt/singularity/3.7.0/bin/singularity")}
#'
#' @section singularity_bin_gemini:
#' Sets the path to the Singularity binary on the Gemini cluster. \cr
#' Default: \code{options(hprcc.singularity_bin_gemini = "/packages/easy-build/software/singularity/3.7.0/bin/singularity")}
#'
#' @section singularity_image_apollo:
#' Sets the path to the Singularity image on the Apollo cluster. \cr
#' Default: \code{options(hprcc.singularity_image_apollo = "/opt/singularity-images/rbioc/vscode-rbioc_3.17.sif")}
#'
#' @section singularity_image_gemini:
#' Sets the path to the Singularity image on the Gemini cluster. \cr
#' Default: \code{options(hprcc.singularity_image_gemini = "/packages/singularity/shared_cache/rbioc/vscode-rbioc_3.17.sif")}
#'
#' @section bind_dirs_apollo:
#' Sets the directories to bind in the Singularity container on the Apollo cluster. \cr
#' Default: \code{options(hprcc.bind_dirs_apollo = "/labs,/opt/singularity,/opt/singularity-images")}
#'
#' @section bind_dirs_gemini:
#' Sets the directories to bind in the Singularity container on the Gemini cluster. \cr
#' Default: \code{options(hprcc.bind_dirs_gemini = "/packages/singularity,/ref_genomes,/scratch")}
#'
#' @section default_partition_apollo:
#' Sets the default SLURM partition for the Apollo cluster. \cr
#' Default: \code{options(hprcc.default_partition_apollo = "fast,all")}
#'
#' @section default_partition_gemini:
#' Sets the default SLURM partition for the Gemini cluster. \cr
#' Default: \code{options(hprcc.default_partition_gemini = "defq")}
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
#' @param slurm_script_dir Directory path for storing SLURM job scripts. Defaults to the session's temporary directory.
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
                              slurm_partition = default_partition(), # TODO ??? does this need to be hprcc:::default_partition()?
                              slurm_log_dir = "logs",
                              slurm_script_dir = tempdir()) {
    job_id <- Sys.getenv("SLURM_JOB_ID")
    nodename <- Sys.info()["nodename"]
    r_libs_user <- getOption("hprcc.r_libs_user")
    r_libs_site <- r_libs_site()
    singularity_bin <- singularity_bin()
    singularity_bind_dirs <- singularity_bind_dirs()
    singularity_image <- singularity_image()
    # Logging
    log_slurm <- getOption("hprcc.log_slurm")

    if (isTRUE(log_slurm)) dir.create(here::here(slurm_log_dir), showWarnings = FALSE, recursive = TRUE) else NULL
    log_output <- if (isTRUE(log_slurm)) here::here(glue::glue("{slurm_log_dir}/slurm-%j.out")) else NULL
    log_error <- if (isTRUE(log_slurm)) here::here(glue::glue("{slurm_log_dir}/slurm-%j.err")) else NULL
    # script directory
    if (!is.null(slurm_script_dir) && slurm_script_dir != tempdir()) {
        dir.create(slurm_script_dir, showWarnings = FALSE, recursive = TRUE)
    }

    script_lines <- glue::glue("#SBATCH --mem {slurm_mem_gigabytes}G \
cd {here::here()} \
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

# #' Initialize targets options
# #'
# #' This function initializes the options for the targets package.
# #' It sets the format, storage, retrieval, controller, and resources options.
# #' The controller options specify the computing resources to be used for different job sizes.
# #' The resources option specifies the crew controller to be used.
# #' 
# #' @importFrom targets tar_option_set
# #' @importFrom targets tar_resources
# #' @importFrom crew crew_controller_group
# #' 
# #' @export
# init_targets <- function() {
# NULL
# }


#-----------------------------------------------------------------------------
#' @importFrom targets tar_option_set
#' @importFrom targets tar_resources
#' @importFrom crew crew_controller_group
.onLoad <- function(libname, pkgname) {
    # hprcc options
    options(
        hprcc.r_libs_user = "~/R/bioc-3.17",
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
        hprcc.log_slurm = FALSE)

    targets::tar_option_set(
        packages = "hprcc",
        format = "qs",
        storage = "worker", # essential to avoid errors with large objects
        retrieval = "worker", #
        controller = crew::crew_controller_group(
            # tiny
            create_controller(name = "tiny", slurm_cpus = 1, slurm_mem_gigabytes = 1, slurm_walltime_minutes = 60),
            # small
            create_controller(name = "small", slurm_cpus = 2, slurm_mem_gigabytes = 20, slurm_walltime_minutes = 360),
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
