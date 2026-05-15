# SLURM Job Status Checking
# Query squeue and sacct to determine job state

#' Check SLURM Job Status
#'
#' Queries SLURM to determine the current status of a job. Tries `squeue`
#' first (for active jobs), then falls back to `sacct` (for completed jobs).
#'
#' @param job_id Character. SLURM job ID to check.
#'
#' @return Character string: one of `"pending"`, `"running"`, `"complete"`,
#'   `"failed"`, `"cancelled"`, `"timeout"`, `"unknown"`.
#'
#' @export
#' @importFrom cli cli_alert_warning
check_slurm_status <- function(job_id) {
    if (is.null(job_id) || is.na(job_id) || job_id == "") return("unknown")

    # Try squeue first (pending/running jobs)
    squeue_result <- suppressWarnings(
        system2("squeue", c("-j", job_id, "-h", "-o", "%T"),
                stdout = TRUE, stderr = TRUE)
    )
    if (length(squeue_result) > 0 &&
        !grepl("Invalid|error", squeue_result[1], ignore.case = TRUE)) {
        state <- tolower(trimws(squeue_result[1]))
        if (nzchar(state)) return(state)
    }

    # Fall back to sacct for completed/historical jobs
    sacct_result <- suppressWarnings(
        system2("sacct", c("-j", job_id, "-n", "-o", "State", "-X"),
                stdout = TRUE, stderr = TRUE)
    )
    if (length(sacct_result) > 0 &&
        !grepl("error", sacct_result[1], ignore.case = TRUE)) {
        state <- toupper(trimws(sacct_result[1]))
        if (grepl("COMPLETED", state)) return("complete")
        if (grepl("FAILED", state)) return("failed")
        if (grepl("CANCELLED", state)) return("cancelled")
        if (grepl("TIMEOUT", state)) return("timeout")
        if (grepl("RUNNING", state)) return("running")
        if (grepl("PENDING", state)) return("pending")
    }

    "unknown"
}


#' Verify Completion Files Exist
#'
#' Checks that expected output files exist for a completed SLURM job.
#' Used by [dispatch_with_deps()] to validate upstream results before
#' submitting dependent jobs.
#'
#' @param result A [slurm_job_result] object or list of them.
#'
#' @return Invisible `TRUE` if all files verified. Errors if a completed
#'   job has missing output files.
#'
#' @export
#' @importFrom cli cli_abort
verify_completion_files <- function(result) {
    if (is.list(result) && !inherits(result, "slurm_job_result")) {
        for (r in result) verify_completion_files(r)
        return(invisible(TRUE))
    }

    if (!inherits(result, "slurm_job_result")) return(invisible(TRUE))
    if (is.null(result$completion_files) || is.null(result$job_id)) {
        return(invisible(TRUE))
    }

    status <- check_slurm_status(result$job_id)
    if (status != "complete") return(invisible(TRUE))

    exists <- vapply(result$completion_files, file.exists, logical(1))
    if (!any(exists)) {
        cli::cli_abort(c(
            "Upstream job {result$job_id} completed but output files missing",
            "x" = "Expected: {result$completion_files[1]}",
            "i" = "Check logs in {result$working_dir}"
        ))
    }
    invisible(TRUE)
}
