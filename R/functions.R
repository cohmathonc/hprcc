# Configurations for {crew.cluster} controllers
# On the COH HPRCC
# ------------------------------------------------------------------------------

# bindings for testthat
system2 <- NULL
system <- NULL
Sys.getenv <- NULL
dir.create <- NULL
write <- NULL
message <- NULL


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
    hostname <- system2("hostname", "-f", stdout = TRUE)
    if (grepl("ppxhp", hostname)) {
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
#' @param slurm_queue SLURM partition for job submission, varying by cluster. See [package options](reference/package-options.html) for defaults.
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
#'     # Create a controller with specific resource allocation
#'     my_controller <- create_controller("my_controller", 4, 8)
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
                              slurm_queue = default_partition(),
                              slurm_log_dir = "logs",
                              slurm_script_dir = tempdir()) {
    job_id <- Sys.getenv("SLURM_JOB_ID")
    nodename <- Sys.info()["nodename"]
    # Logging
    if (isTRUE(log_slurm)) dir.create(here::here(slurm_log_dir), showWarnings = FALSE, recursive = TRUE) else NULL
    log_output <- if (isTRUE(log_slurm)) here::here(glue::glue("{slurm_log_dir}/slurm-%j.out")) else NULL
    log_error <- if (isTRUE(log_slurm)) here::here(glue::glue("{slurm_log_dir}/slurm-%j.err")) else NULL
    # script directory
    if (!is.null(slurm_script_dir) && slurm_script_dir != tempdir()) {
        dir.create(slurm_script_dir, showWarnings = FALSE, recursive = TRUE)
    }

    script_lines <- glue::glue("#SBATCH --mem {slurm_mem_gigabytes}G \
#SBATCH --partition {default_partition()} \
{singularity_exec()}")

    crew.cluster::crew_controller_slurm(
        name = name,
        slurm_cpus_per_task = slurm_cpus,
        slurm_time_minutes = slurm_walltime_minutes,
        host = nodename,
        worker = slurm_workers,
        seconds_idle = 30,
        script_directory = slurm_script_dir,
        slurm_log_output = log_output,
        slurm_log_error = log_error,
        script_lines = script_lines
    )
}
#-----------------------------------------------------------------------------

#' Package Options
#'
#' This page documents the configurable options available in the **hprcc** package, settable using \code{\link[base]{options}}.
#'
#' @section log_slurm:
#' Controls whether SLURM job scripts logging is turned on or off. \cr
#' Default: \code{options(hprcc.log_slurm = TRUE)}
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




#-----------------------------------------------------------------------------
.onLoad <- function(libname, pkgname) {
    # hprcc options
    options(
        hprcc.cluster = get_cluster(),
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
        hprcc.log_slurm = FALSE
    )
    # targets options
    #' @importFrom targets tar_option_set
    #' @importFrom targets tar_resources
    #' @importFrom crew crew_controller_group
    targets::tar_option_set(
        format = "qs",
        storage = "worker", # essential to avoid errors with large objects
        retrieval = "worker", #
        controller = crew::crew_controller_group(
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

cluster_name <- getOption("hprcc.cluster")
r_libs_user <- getOption("hprcc.r_libs_user")
r_libs_site_apollo <- getOption("hprcc.r_libs_site_apollo")
r_libs_site_gemini <- getOption("hprcc.r_libs_site_gemini")
singularity_bin_apollo <- getOption("hprcc.singularity_bin_apollo")
singularity_bin_gemini <- getOption("hprcc.singularity_bin_gemini")
singularity_image_apollo <- getOption("hprcc.singularity_image_apollo")
singularity_image_gemini <- getOption("hprcc.singularity_image_gemini")
bind_dirs_apollo <- getOption("hprcc.bind_dirs_apollo")
bind_dirs_gemini <- getOption("hprcc.bind_dirs_gemini")
default_partition_apollo <- getOption("hprcc.default_partition_apollo")
default_partition_gemini <- getOption("hprcc.default_partition_gemini")
log_slurm <- getOption("hprcc.log_slurm")

r_libs_site <- function() ifelse(cluster_name == "apollo", r_libs_site_apollo, r_libs_site_gemini)

singularity_bin <- function() ifelse(cluster_name == "apollo", singularity_bin_apollo, singularity_bin_gemini)

singularity_image <- function() ifelse(cluster_name == "apollo", singularity_image_apollo, singularity_image_gemini)

bind_dirs <- function() ifelse(cluster_name == "apollo", bind_dirs_apollo, bind_dirs_gemini)

default_partition <- function() ifelse(cluster_name == "apollo", default_partition_apollo, default_partition_gemini)

singularity_exec <- function() {
    glue::glue(
"cd {here::here()} \
{singularity_bin()} exec \\
--env R_LIBS_USER={r_libs_user} \\
--env R_LIBS_SITE={r_libs_site()} \\
-B {bind_dirs()} \\
{singularity_image()} \\"
    )
}

#-----------------------------------------------------------------------------
## Resources shortcuts for {targets}

#' Small SLURM Resource Configuration
#'
#' Defines SLURM resources for small-scale computations. Utilizes `targets::tar_resources`
#' with a "small" controller setting, suitable for less resource-intensive tasks.
#'
#' | CPUs | Memory (GB) | Time (minutes) |
#' |------|-------------|----------------|
#' | 2    | 20          | 360            |
#'
#' @export
#' @seealso \code{\link[targets]{tar_resources}}, \code{\link[targets]{tar_resources_crew}}
small <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "small")
)

#' Medium SLURM Resource Configuration
#'
#' Configures medium-level SLURM resources. This object is constructed using
#' `targets::tar_resources` with a "medium" controller setting, suitable for
#' moderately demanding computational tasks.
#'
#' | CPUs | Memory (GB) | Time (minutes) |
#' |------|-------------|----------------|
#' | 12   | 80          | 360            |
#'
#' @export
#' @seealso \code{\link[targets]{tar_resources}}, \code{\link[targets]{tar_resources_crew}}
medium <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "medium")
)

#' Large SLURM Resource Configuration
#'
#' Configures SLURM resources for large-scale tasks using `targets::tar_resources`
#' with a "large" controller setting. Ideal for high-demand computations.
#'
#' | CPUs | Memory (GB) | Time (minutes) |
#' |------|-------------|----------------|
#' | 20   | 200         | 720            |
#'
#' @export
#' @seealso \code{\link[targets]{tar_resources}}, \code{\link[targets]{tar_resources_crew}}
large <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "large")
)

#' Huge SLURM Resource Configuration
#'
#' Sets up SLURM resources for very large tasks using `targets::tar_resources`
#' with a "huge" controller. Designed for very high computational loads.
#'
#' | CPUs | Memory (GB) | Time (minutes) |
#' |------|-------------|----------------|
#' | 40   | 100         | 120            |
#' @export
#' @seealso \code{\link[targets]{tar_resources}}, \code{\link[targets]{tar_resources_crew}}
huge <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "huge")
)

#' Bigmem SLURM Resource Configuration
#'
#' Establishes SLURM resources for tasks requiring significant memory via
#' `targets::tar_resources` with a "bigmem" controller. Suitable for memory-intensive jobs.
#'
#' | CPUs | Memory (GB) | Time (minutes) |
#' |------|-------------|----------------|
#' | 10   | 500         | 360            |
#'
#' @export
#' @seealso \code{\link[targets]{tar_resources}}, \code{\link[targets]{tar_resources_crew}}
bigmem <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "bigmem")
)

#-----------------------------------------------------------------------------
# Functions for logging and resource allocation
# ----------------------------------------------------------------------------

#' Retrieve SLURM Job Allocation
#'
#' Retrieves the CPU and memory allocation for a SLURM job. Returns a list with job ID,
#' CPU count, and memory in GB, or `NULL` if the `sacct` command fails.
#'
#' @return A list with elements: job_id, CPUs, and Memory_GB.
#' @export
#' @examples
#' if (interactive()) {
#'     slurm_allocation()
#' }
slurm_allocation <- function() {
    slurm_job_id <- Sys.getenv("SLURM_JOB_ID")
    if (slurm_job_id == "") {
        warning("SLURM_JOB_ID not set.")
        return(NULL)
    }

    job_info <- system2("sacct", c(
        "-j", slurm_job_id, "--format=AllocCPUS,AllocTRES",
        "--noheader", "--parsable2"
    ), stdout = TRUE)

    if (length(job_info) == 0) {
        warning("Failed to retrieve job information.")
        return(NULL)
    }

    allocated_cpus <- as.numeric(stringr::str_extract(job_info[1], "(?<=cpu=)\\d+"))
    mem_info <- stringr::str_extract(job_info[1], "mem=[0-9]+[GM]")
    mem_in_gb <- as.numeric(sub("mem=([0-9]+)[GM]", "\\1", mem_info))
    mem_in_gb <- ifelse(grepl("M", mem_info), mem_in_gb / 1024, mem_in_gb)

    list(job_id = slurm_job_id, CPUs = allocated_cpus, Memory_GB = mem_in_gb)
}

# ------------------------------------------------------------------------------
#' Initialize Multisession Computation with Future
#'
#' Configures the \{future\} package for multisession computation, adapting resource
#' settings based on the SLURM job's allocation or system resources.
#'
#' @details
#' In a SLURM environment, it sets the number of workers and memory allocation
#' based on the job's resources. Outside SLURM, it defaults to system memory
#' and standard multisession settings.
#'
#' @examples
#' if (interactive()) {
#'     init_multisession()
#' }
#' @export
#' @importFrom future plan multisession
init_multisession <- function() {
    slurm_job_id <- Sys.getenv("SLURM_JOB_ID")
    if (!is.na(as.numeric(slurm_job_id)) && slurm_job_id != "") {
        resources <- slurm_allocation()
        if (is.null(resources)) {
            stop("Unable to allocate SLURM resources.")
        }
        future::plan(multisession, workers = resources$CPUs)
        options(future.globals.maxSize = resources$Memory_GB * 1024^3)
    } else {
        system_memory_gb <- as.numeric(system("grep MemTotal /proc/meminfo | awk '{print $2/1024/1024}'", intern = TRUE))
        future::plan(multisession)
        options(future.globals.maxSize = system_memory_gb * 1024^3)
    }
}

# ------------------------------------------------------------------------------
# This function isn't that useful - it's a summary of the instant memory and CPU
# usage
#' @importFrom ps ps_memory_info
#' @importFrom ps ps_cpu_times
get_usage_summary <- function() {
    # Get memory info
    mem_info <- ps::ps_memory_info()
    ram_usage_gb <- round(mem_info[1] / (1024^3), 2) # Convert from bytes to GB

    # Get CPU times
    cpu_times <- ps::ps_cpu_times()
    cpu_usage_user <- round(cpu_times[1], 2)
    cpu_usage_system <- round(cpu_times[2], 2)

    # Create summary string
    summary_str <- paste(
        Sys.time(),
        "| Mem Usage:", ram_usage_gb, "GB |",
        "CPU User Time:", cpu_usage_user, "seconds |",
        "CPU System Time:", cpu_usage_system, "seconds"
    )

    return(summary_str)
}

# ------------------------------------------------------------------------------
#' Print a message and optionally write it to a log file
#'
#' This function prints a message and optionally writes it to a log file.
#'
#' @param message_str The message to be logged.
#' @param log Logical indicating whether to write the message to a log file. Default is TRUE.
#' @param log_dir The directory to store the log file. Default is "logs".
#' @return None, prints a message to the terminal
#' @export
#' @examples
#' if (interactive()) {
#'     log_message("This is a log message")
#' }
log_message <- function(message_str, log = TRUE, log_dir = "logs") {
    # Set up the message
    job_id <- Sys.getenv("SLURM_JOB_ID")
    usage_summary <- get_usage_summary()
    dir.create(here::here(log_dir), showWarnings = FALSE)
    log_file <- glue::glue("{log_dir}/job_{job_id}.log")
    message_str <- glue::glue("{message_str}\n{usage_summary}}")
    # Print the message
    message(message_str)
    # Log it
    if (isTRUE(log)) {
        write(message_str,
            file = log_file,
            append = TRUE
        )
    }
}

#' Log Parameters
#'
#' Captures and logs the arguments of the calling function. Useful for debugging
#' and tracking function usage.
#'
#' @param log A logical value indicating whether to log the message to the log file (default is TRUE).
#' @return None, prints a message to the log.
#' @export
#' @importFrom glue glue
#' @examples
#' if (interactive()) {
#'     some_function(x = 42, y = "example") # Assuming some_function calls log_params()
#' }
log_params <- function(log = TRUE) {
    # Capture arguments from the calling function
    args <- as.list(sys.call(-1L))
    args <- args[-1L] # Remove the function call itself
    arg_strs <- vapply(args, function(arg) {
        paste0(deparse(arg))
    }, character(1))

    # Construct and log the message
    arg_list_str <- paste(names(arg_strs), arg_strs, sep = " = ", collapse = "; ")
    message_str <- glue("Arguments: {arg_list_str}")

    # Assuming log_message is a function that handles logging
    if (log) log_message(message_str)
}
