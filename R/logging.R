
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
#' @importFrom glue glue
#' @importFrom here here
#' @examples
#' if (interactive()) {
#'     log_message("This is a log message")
#' }
log_message <- function(message_str, log = TRUE, log_dir = "logs") {
    # Set up the message
    job_id <- Sys.getenv("SLURM_JOB_ID")
    usage_summary <- get_usage_summary()
    dir.create(here(log_dir), showWarnings = FALSE)
    log_file <- glue("{log_dir}/job_{job_id}.log")
    message_str <- glue("{message_str}\n{usage_summary}}")
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

    if (log) log_message(message_str)
}
