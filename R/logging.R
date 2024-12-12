
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


log_hprcc_settings <- function() {
    # Only proceed if logging is enabled
    if (!isTRUE(getOption("hprcc.slurm_logs", FALSE))) {
        return(invisible(NULL))
    }

    # Create logs directory using here for proper path resolution
    log_dir <- here::here(targets::tar_path_store(), "logs")
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

    # Get job ID and construct filename
    job_id <- Sys.getenv("SLURM_JOB_ID")
    if (!nzchar(job_id)) job_id <- "no_slurm"
    log_file <- glue::glue("{log_dir}/hprcc_settings_{job_id}.txt")

    # Determine cluster
    cluster <- tryCatch(
        get_cluster(),
        warning = function(w) "unknown"
    )

    # Collect all HPRCC options
    hprcc_opts <- list(
        cluster = cluster,
        slurm_logs = getOption("hprcc.slurm_logs", default = FALSE),
        slurm_verbose = getOption("hprcc.slurm_verbose", default = FALSE),
        slurm_jobs = getOption("hprcc.slurm_jobs", default = TRUE),
        slurm_account = getOption("hprcc.slurm_account", default = ""),
        r_libs_user = getOption("hprcc.r_libs_user", default = Sys.getenv("R_LIBS_USER")),
        r_libs_site = getOption("hprcc.r_libs_site", default = Sys.getenv("R_LIBS_SITE")),
        singularity_bin = getOption("hprcc.singularity_bin", default = singularity_bin()),
        singularity_container = getOption("hprcc.singularity_container", default = Sys.getenv("SINGULARITY_CONTAINER")),
        bind_dirs = getOption("hprcc.bind_dirs", default = Sys.getenv("SINGULARITY_BIND")),
        default_partition = getOption("hprcc.default_partition", default = default_partition()),
        parallelly_cores_methods = getOption("parallelly.availableCores.methods", default = "")
    )

    # Collect relevant environment variables
    env_vars <- list(
        # R paths
        R_LIBS = Sys.getenv("R_LIBS"),
        R_LIBS_USER = Sys.getenv("R_LIBS_USER"),
        R_LIBS_SITE = Sys.getenv("R_LIBS_SITE"),
        R_LIBS_CLUSTER = Sys.getenv("R_LIBS_CLUSTER"),

        # System paths
        TMPDIR = Sys.getenv("TMPDIR"),
        PWD = Sys.getenv("PWD"),
        HOME = Sys.getenv("HOME"),

        # Singularity settings
        SINGULARITY_BIND = Sys.getenv("SINGULARITY_BIND"),
        SINGULARITY_CONTAINER = Sys.getenv("SINGULARITY_CONTAINER"),
        SINGULARITY_BIN = Sys.getenv("SINGULARITY_BIN"),
        SINGULARITY_NAME = Sys.getenv("SINGULARITY_NAME"),

        # Bioconductor
        BIOCONDUCTOR_VERSION = Sys.getenv("BIOCONDUCTOR_VERSION"),

        # SLURM job info
        SLURM_JOB_ID = Sys.getenv("SLURM_JOB_ID"),
        SLURM_JOB_NAME = Sys.getenv("SLURM_JOB_NAME"),
        SLURM_SUBMIT_DIR = Sys.getenv("SLURM_SUBMIT_DIR"),
        SLURM_JOB_PARTITION = Sys.getenv("SLURM_JOB_PARTITION"),
        SLURM_CPUS_PER_TASK = Sys.getenv("SLURM_CPUS_PER_TASK"),
        SLURM_MEM_PER_NODE = Sys.getenv("SLURM_MEM_PER_NODE"),
        SLURM_NODELIST = Sys.getenv("SLURM_NODELIST"),
        SLURM_NTASKS = Sys.getenv("SLURM_NTASKS"),
        SLURM_TASKS_PER_NODE = Sys.getenv("SLURM_TASKS_PER_NODE"),
        SLURM_ARRAY_TASK_ID = Sys.getenv("SLURM_ARRAY_TASK_ID"),
        SLURM_ARRAY_JOB_ID = Sys.getenv("SLURM_ARRAY_JOB_ID"),

        # GPU-related (for Gemini)
        CUDA_VISIBLE_DEVICES = Sys.getenv("CUDA_VISIBLE_DEVICES"),
        GPU_DEVICE_ORDINAL = Sys.getenv("GPU_DEVICE_ORDINAL")
    )

    # Format timestamp
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

    # Create log content using glue for better string interpolation
    log_content <- c(
        "HPRCC Settings Log",
        "================",
        glue::glue("Generated: {timestamp}"),
        "",
        "Cluster Information",
        "------------------",
        glue::glue("Cluster: {cluster}"),
        glue::glue("Hostname: {Sys.info()['nodename']}"),
        "",
        "HPRCC Options",
        "-------------",
        vapply(names(hprcc_opts), function(x) {
            value <- hprcc_opts[[x]]
            if (is.null(value) || (is.character(value) && !nzchar(value))) {
                return(glue::glue("{x}: <not set>"))
            }
            glue::glue("{x}: {as.character(value)}")
        }, character(1)),
        "",
        "Environment Variables",
        "--------------------",
        vapply(names(env_vars), function(x) {
            value <- env_vars[[x]]
            if (is.null(value) || !nzchar(value)) {
                glue::glue("{x}=<not set>")
            } else {
                glue::glue("{x}={value}")
            }
        }, character(1)),
        "",
        "System Information",
        "-----------------",
        glue::glue("R Version: {R.version$version.string}"),
        glue::glue("Operating System: {Sys.info()['sysname']}"),
        glue::glue("User: {Sys.info()['user']}"),
        glue::glue("Working Directory: {here::here()}"),
        glue::glue("targets Store Path: {here::here(targets::tar_path_store())}")
    )

    # Write to file
    writeLines(log_content, log_file)

    invisible(log_file)
}