#' Read and combine multiple autometric log files
#' 
#' @param path Character string path to the _targets/log directory. If NULL, defaults to "_targets/log"
#' in current working directory.
#' @param pattern Character string pattern to match log files. Defaults to NULL (all files).
#' @param units_cpu Character string with the units of the cpu field. See autometric::log_read()
#' @param units_memory Character string with the units of the memory field. See autometric::log_read()
#' @param units_time Character string with the units of the time field. See autometric::log_read()
#' @param hidden TRUE to include hidden files, FALSE to omit
#' 
#' @return A data frame combining all log files with additional columns:
#' \itemize{
#'   \item file_name: Source log file name
#'   \item slurm_job_id: SLURM job ID extracted from crew-{jobid}.out filename 
#'   \item ... all standard autometric log columns
#' }
#' @export
#'
#' @importFrom autometric log_read
#'
#' @examples
#' \dontrun{
#' logs <- read_target_logs("_targets/logs")
#' log_plot(logs, metric = "rss")
#' }
read_target_logs <- function(path = NULL, 
                           pattern = NULL,
                           units_cpu = "percentage",
                           units_memory = "megabytes", 
                           units_time = "seconds",
                           hidden = FALSE) {
  
  # Default to _targets/log if no path provided
  if (is.null(path)) {
    path <- file.path("_targets", "logs")
  }
  
  # Check if directory exists
  if (!dir.exists(path)) {
    stop("Directory not found: ", path)
  }
  
  # List all files in directory matching crew-*.out pattern
  files <- list.files(path = path, 
                     pattern = if(is.null(pattern)) "crew-[0-9]+\\.out$" else pattern,
                     full.names = TRUE,
                     recursive = FALSE,
                     include.dirs = FALSE,
                     no.. = TRUE)
  
  if (length(files) == 0) {
    stop("No crew log files found in: ", path)
  }
  
  # Read and combine all logs
  logs <- lapply(files, function(f) {
    # Read log file
    log_data <- autometric::log_read(f,
                                   units_cpu = units_cpu,
                                   units_memory = units_memory,
                                   units_time = units_time,
                                   hidden = hidden)
    
    # Skip if empty
    if (is.null(log_data) || nrow(log_data) == 0) {
      return(NULL)
    }
    
    # Add filename and extract SLURM job ID
    log_data$file_name <- basename(f)
    # Extract job ID from crew-JOBID.out pattern
    log_data$slurm_job_id <- sub("crew-([0-9]+)\\.out$", "\\1", basename(f))
    
    return(log_data)
  })
  
  # Remove NULL entries and combine
  logs <- do.call(rbind, logs[!sapply(logs, is.null)])
  
  if (is.null(logs) || nrow(logs) == 0) {
    stop("No valid log data found")
  }
  
  # Sort by time
  logs <- logs[order(logs$time), ]
  
  return(logs)
}

#' Plot combined metrics from multiple log files
#'
#' @param logs Data frame returned by read_target_logs()
#' @param metric Character string with the name of metric to plot
#' @param job_ids Optional character vector of SLURM job IDs to include
#' @param ... Additional arguments passed to autometric::log_plot()
#'
#' @return A base plot of the metric over time, colored by SLURM job ID
#' @export
#'
#' @examples
#' \dontrun{
#' logs <- read_target_logs()
#' plot_target_logs(logs, metric = "rss")
#' }
plot_target_logs <- function(logs, metric = "resident", job_ids = NULL, ...) {
  
  if (!is.null(job_ids)) {
    logs <- logs[logs$slurm_job_id %in% job_ids, ]
  }
  
  if (nrow(logs) == 0) {
    stop("No log data to plot")
  }
  
  # Create base plot
  plot(logs$time, logs[[metric]], 
       type = "n",
       xlab = "Time",
       ylab = metric,
       ...)
  
  # Plot each job with different color
  job_colors <- rainbow(length(unique(logs$slurm_job_id)))
  for (i in seq_along(unique(logs$slurm_job_id))) {
    job_id <- unique(logs$slurm_job_id)[i]
    job_data <- logs[logs$slurm_job_id == job_id, ]
    lines(job_data$time, job_data[[metric]], 
          col = job_colors[i])
  }
  
  # Add legend
  legend("topright", 
         legend = unique(logs$slurm_job_id),
         col = job_colors,
         lty = 1,
         title = "SLURM Job ID")
}