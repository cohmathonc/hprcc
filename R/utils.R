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
#' @return
#' Invisibly returns the result of \code{future::plan()}.
#'
#' @examples
#' if (interactive()) {
#'     init_multisession()
#' }
#'
#' @seealso
#' \code{\link[future]{plan}}, \code{\link[future]{multisession}}
#'
#' @export
#' @importFrom future plan multisession
init_multisession <- function() {
    # Check for SLURM environment
    slurm_job_id <- Sys.getenv("SLURM_JOB_ID", "")

    # If we're in a valid SLURM job
    if (nzchar(slurm_job_id) && !is.na(as.numeric(slurm_job_id))) {
        tryCatch(
            {
                resources <- slurm_allocation()
                if (is.null(resources)) {
                    warning("Could not determine SLURM resources, using default multisession settings")
                    return(future::plan(multisession))
                }

                # Set plan with SLURM-specific resources
                future::plan(
                    multisession,
                    workers = resources$CPUs
                )

                # Set memory limit per worker
                worker_memory <- resources$Memory_GB * 1024^3 / resources$CPUs
                options(future.globals.maxSize = worker_memory)
            },
            error = function(e) {
                warning(
                    "Error setting SLURM-specific resources: ", e$message,
                    "\nFalling back to default multisession settings"
                )
                future::plan(multisession)
            }
        )
    } else {
        # Not in SLURM, use default settings
        future::plan(multisession)
    }

    invisible(future::plan())
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
    # Check if we're in a SLURM job
    slurm_job_id <- Sys.getenv("SLURM_JOB_ID")
    if (slurm_job_id == "") {
        warning("SLURM_JOB_ID not set.")
        return(NULL)
    }

    # Get CPU allocation from environment variables
    cpus <- if (Sys.getenv("SLURM_CPUS_ON_NODE") != "") {
        as.numeric(Sys.getenv("SLURM_CPUS_ON_NODE"))
    }  else {
        warning("No CPU allocation information found in environment.")
        1 # default if no SLURM CPU env vars set
    }

    # Get memory allocation from environment variable (SLURM provides this in MB)
    mem_mb <- Sys.getenv("SLURM_MEM_PER_NODE")
    mem_in_gb <- if (mem_mb != "") {
        as.numeric(mem_mb) / 1024 # Convert MB to GB
    } else {
        warning("SLURM_MEM_PER_NODE not set.")
        NULL
    }

    list(
        job_id = slurm_job_id,
        CPUs = cpus,
        Memory_GB = mem_in_gb
    )
}
