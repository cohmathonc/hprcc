# ------------------------------------------------------------------------------
# bindings for testthat
system2 <- NULL
system <- NULL
Sys.info <- NULL
Sys.getenv <- NULL
dir.create <- NULL
write <- NULL
message <- NULL

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

#' Retrieve SLURM Job Allocation
#'
#' Retrieves the CPU and memory allocation for a SLURM job. Returns a list with job ID,
#' CPU count, and memory in GB, or `NULL` if the `sacct` command fails.
#'
#' @return A list with elements: job_id, CPUs, and Memory_GB.
#' @export
#' @importFrom stringr str_extract
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

