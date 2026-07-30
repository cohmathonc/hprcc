# slurm_job_result S3 Class
# Tracks SLURM job submission state for the fire-and-track pattern

#' Create a SLURM Job Result Object
#'
#' S3 class that tracks SLURM job submission state for targets integration.
#' Stores the path to the output/script, job ID, status, working directory,
#' and expected completion files.
#'
#' @param path Character. Path to completion file (if done) or script (if pending).
#' @param job_id Character or NULL. SLURM job ID (NULL if job already complete).
#' @param status Character. One of "submitted", "complete", "already_submitted".
#' @param working_dir Character. Working directory for the job.
#' @param completion_files Character vector or NULL. Expected output files that
#'   signal successful completion.
#'
#' @return An S3 object of class `slurm_job_result`.
#'
#' @details
#' The fire-and-track pattern uses this class to pass job state through targets:
#' \itemize{
#'   \item `"complete"`: Completion file exists, job is done
#'   \item `"submitted"`: Job was just submitted via sbatch
#'   \item `"already_submitted"`: Job was previously submitted and is still running
#' }
#'
#' Barrier targets use [get_result_path()] to block until the job completes.
#' Dependency injection uses [dispatch_with_deps()] to chain SLURM jobs.
#'
#' @seealso [run_slurm_job()], [get_result_path()], [dispatch_with_deps()]
#' @export
slurm_job_result <- function(path, job_id = NULL, status = "complete",
                              working_dir = dirname(path),
                              completion_files = NULL) {
    stopifnot(
        is.character(path), length(path) == 1L,
        is.null(job_id) || (is.character(job_id) && length(job_id) == 1L),
        status %in% c("submitted", "complete", "already_submitted"),
        is.character(working_dir), length(working_dir) == 1L,
        is.null(completion_files) || is.character(completion_files)
    )

    structure(
        list(
            path = path,
            job_id = job_id,
            status = status,
            working_dir = working_dir,
            completion_files = completion_files
        ),
        class = "slurm_job_result"
    )
}

#' @export
as.character.slurm_job_result <- function(x, ...) x$path

#' @export
print.slurm_job_result <- function(x, ...) {
    status_icon <- switch(x$status,
        complete = cli::col_green("\u2714"),
        submitted = cli::col_yellow("\u25b6"),
        already_submitted = cli::col_blue("\u25b6"),
        cli::col_red("?")
    )
    cli::cli_text("{status_icon} {.path {basename(x$path)}} [{x$status}]")
    if (!is.null(x$job_id)) cli::cli_text("  Job ID: {x$job_id}")
    invisible(x)
}

#' Test if Object is a slurm_job_result
#'
#' @param x Object to test.
#' @return Logical.
#' @export
is.slurm_job_result <- function(x) {
    inherits(x, "slurm_job_result")
}
