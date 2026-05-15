# Fire-and-Track Pipeline Dispatch
# SLURM dependency chaining and barrier functions for targets integration

#' Dispatch Job with SLURM Dependencies
#'
#' Wraps a processing function to handle SLURM job dependencies automatically.
#' Inspects upstream [slurm_job_result] objects, errors on failed jobs, and
#' injects `--dependency=afterok:JOBID` for jobs still running.
#'
#' @param fn Function that calls [run_slurm_job()] or [run_singularity_job()].
#' @param upstream A [slurm_job_result] or list of them from an upstream target.
#' @param ... Additional arguments passed to `fn`.
#'
#' @return A [slurm_job_result] from `fn`.
#'
#' @details
#' The dispatch flow:
#' 1. Extract job IDs from upstream results
#' 2. Check status of each upstream job
#' 3. Error immediately if any upstream failed/cancelled/timed out
#' 4. Verify completion files exist for completed jobs
#' 5. For pending/running jobs, set `options(slurm_job_dependency = "afterok:JOBID")`
#'    which [run_slurm_job()] reads to inject `#SBATCH --dependency=` headers
#' 6. Call `fn(get_result_path(upstream, wait = FALSE), ...)`
#'
#' This enables the entire SLURM DAG to be submitted in one `tar_make()` call
#' without blocking. SLURM's own dependency system handles sequencing.
#'
#' @section Usage in targets:
#' ```r
#' tar_target(
#'   cellbender_result,
#'   dispatch_with_deps(run_cellbender, upstream = cellranger_result, ...),
#'   deployment = "main",
#'   cue = tar_cue(mode = "always")
#' )
#' ```
#'
#' @seealso [get_result_path()], [run_slurm_job()], [slurm_job_result]
#' @export
#' @importFrom cli cli_alert_info cli_abort
dispatch_with_deps <- function(fn, upstream, ...) {
    job_ids <- collect_job_ids(upstream)

    if (length(job_ids) > 0) {
        statuses <- vapply(job_ids, check_slurm_status, character(1))

        failed <- statuses %in% c("failed", "cancelled", "timeout")
        if (any(failed)) {
            cli::cli_abort(c(
                "Upstream job(s) failed",
                "x" = "Job IDs: {paste(job_ids[failed], collapse = ', ')}"
            ))
        }

        complete <- statuses == "complete"
        if (any(complete)) {
            verify_completion_files(upstream)
        }

        pending <- statuses %in% c("pending", "running", "submitted")
        if (any(pending)) {
            dep_string <- paste0("afterok:", paste(job_ids[pending], collapse = ":"))
            cli::cli_alert_info("Adding dependency: {dep_string}")
            old_opt <- getOption("slurm_job_dependency")
            on.exit(options(slurm_job_dependency = old_opt), add = TRUE)
            options(slurm_job_dependency = dep_string)
        }
    }

    fn(get_result_path(upstream, wait = FALSE), ...)
}


#' Get Result Path from SLURM Job Result
#'
#' Extracts the output path from a [slurm_job_result], optionally blocking
#' until the SLURM job completes. Used as a barrier in targets pipelines.
#'
#' @param x A [slurm_job_result] or character path.
#' @param wait Logical. If `TRUE` (default), polls SLURM until job completes.
#' @param poll_interval Numeric. Seconds between status checks (default 30).
#' @param timeout Numeric. Maximum seconds to wait (default 7200 = 2 hours).
#'
#' @return Character path to the completion file or script.
#'
#' @section Barrier pattern in targets:
#' ```r
#' tar_target(
#'   job_ready,
#'   get_result_path(job_result, wait = TRUE),
#'   deployment = "main"
#' )
#' ```
#'
#' @seealso [dispatch_with_deps()], [slurm_job_result]
#' @export
#' @importFrom cli cli_alert_info cli_alert_success cli_abort
get_result_path <- function(x, wait = TRUE, poll_interval = 30, timeout = 7200) {
    if (!inherits(x, "slurm_job_result")) return(as.character(x))

    if (x$status == "complete" || is.null(x$job_id)) {
        return(x$path)
    }

    if (!wait) return(x$path)

    cli::cli_alert_info("Waiting for SLURM job {x$job_id}...")
    start_time <- Sys.time()

    repeat {
        status <- check_slurm_status(x$job_id)

        if (status == "complete") {
            cli::cli_alert_success("Job {x$job_id} completed")
            break
        }

        if (status %in% c("failed", "cancelled", "timeout")) {
            cli::cli_abort(c(
                "SLURM job {x$job_id} {status}",
                "i" = "Check logs in {x$working_dir}"
            ))
        }

        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        if (elapsed > timeout) {
            cli::cli_abort("Timeout waiting for job {x$job_id} after {timeout}s")
        }

        Sys.sleep(poll_interval)
    }

    x$path
}


#' Collect Job IDs from Upstream Results
#'
#' Extracts SLURM job IDs from a single [slurm_job_result] or list of them.
#'
#' @param upstream A [slurm_job_result], list of them, or NULL.
#' @return Character vector of job IDs (may be empty).
#'
#' @export
collect_job_ids <- function(upstream) {
    if (is.null(upstream)) return(character(0))

    if (is.list(upstream) && !inherits(upstream, "slurm_job_result")) {
        ids <- vapply(upstream, function(x) {
            if (inherits(x, "slurm_job_result") && !is.null(x$job_id)) {
                x$job_id
            } else {
                NA_character_
            }
        }, character(1))
        return(ids[!is.na(ids)])
    }

    if (inherits(upstream, "slurm_job_result") && !is.null(upstream$job_id)) {
        return(upstream$job_id)
    }

    character(0)
}


#' Get Working Directory from SLURM Result
#'
#' Extracts the working directory from a [slurm_job_result] or infers it
#' from a file path by walking up directory levels.
#'
#' @param result A [slurm_job_result] or character path.
#' @param completion_depth Integer. Number of directory levels to walk up
#'   from the path to reach the working directory (default 0).
#'
#' @return Character path to the working directory, or NULL.
#'
#' @export
get_slurm_working_dir <- function(result, completion_depth = 0L) {
    if (inherits(result, "slurm_job_result")) return(result$working_dir)

    path <- as.character(result)
    if (is.null(path) || length(path) == 0 || is.na(path)) return(NULL)
    if (grepl("\\.sh$", path)) return(dirname(path))

    for (i in seq_len(completion_depth + 1L)) path <- dirname(path)
    path
}
