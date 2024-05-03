# Configurations for Apollo {crew} & {crew.cluster} controllers

# path for running nf-core pipelines, slurm templates, sbatch scripts, targets cache, logs, etc...
cache_path <- "preprocessing_nfcore"
crew_cache_path <- here::here(glue::glue("{cache_path}/_crew"))
log_slurm <- FALSE

# Setup
library("crew")
library("crew.cluster")
options(future.seed = TRUE)
targets::tar_config_set("workers" = 350)

#-----------------------------------------------------------------------------
# Crew controllers
local <- crew::crew_controller_local(
    name = "local",
    workers = 5L,
    seconds_interval = 0.25,
    seconds_timeout = 10,
    seconds_launch = 30,
    seconds_idle = 30,
    tasks_max = Inf,
    tasks_timers = 0L,
)

## Slurm
# Define the controller
small <- crew.cluster::crew_controller_slurm(
    name = "small",
    slurm_cpus_per_task = 2L,
    slurm_time_minutes = 360L,
    host = Sys.info()["nodename"],
    workers = 350L,
    seconds_idle = 30,
    script_directory = here::here(glue::glue("{crew_cache_path}")),
    slurm_log_output = if (isTRUE(log_slurm)) here::here(glue::glue("{crew_cache_path}/slurm-%j.out")) else NULL,
    slurm_log_error  = if (isTRUE(log_slurm)) here::here(glue::glue("{crew_cache_path}/slurm-%j.err")) else NULL,
    script_lines = glue::glue("#SBATCH --partition=compute \
#SBATCH --mem 20G \
cd {here::here()} \
/packages/easy-build/software/singularity/3.7.0/bin/singularity exec \\
    --env R_LIBS_USER=~/R/bioc-3.17 \\
    --env R_LIBS_SITE=/packages/singularity/shared_cache/rbioc/rlibs/bioc-3.17 \\
    -B /packages/singularity,/ref_genomes,/scratch \\
    /packages/singularity/shared_cache/rbioc/vscode-rbioc_3.17.sif \\"),
    verbose = TRUE
)

#-----------------------------------------------------------------------------
## Run jobs locally by default
tar_option_set(
    error = "abridge",
    workspace_on_error = TRUE,
    format = "qs",
    storage = "worker",   # essential to avoid errors with large objects
    retrieval = "worker", #
    controller = crew::crew_controller_group(local, small),
    resources = tar_resources(
        crew = tar_resources_crew(controller = "small")
    )
)
## Some shortcuts
run_local <- tar_resources(
      crew = tar_resources_crew(controller = "local")
    )

slurm_small <- tar_resources(
      crew = tar_resources_crew(controller = "small")
    )

#' Retrieve the number of CPUs and amount of memory allocated to a SLURM job.
#'
#' This function retrieves the number of CPUs and amount of memory allocated to a SLURM job
#' using the sacct command. It returns a list with two elements: CPUs and Memory_GB.
#' If the sacct command fails, it returns NULL.
#'
#' @return A list with two elements: CPUs and Memory_GB.
#' @export
#'
#' @examples
#' slurm_allocation()
#'
#' @importFrom stringr strsplit
#' @importFrom stringr gsub
slurm_allocation <- function() {
    slurm_job_id <- Sys.getenv("SLURM_JOB_ID")
    
    # Get the job information
    job_info <- system2(
        command = "sacct",
        args = c(
            "-j", slurm_job_id,
            "--format=AllocCPUS,AllocTRES",
            "--noheader",
            "--parsable2"
        ),
        stdout = TRUE
    )
    
    # If sacct command fails, return NULL
    if (length(job_info) == 0) {
        warning("Failed to retrieve job information.")
        return(NULL)
    }
    
    # Extract the number of allocated CPUs
    # We'll take the first value assuming it's consistent across all rows
    allocated_cpus <- as.numeric(str_extract(job_info[1], "(?<=cpu=)\\d+"))
    
    # The memory information is in the format "mem=<memory_in_G>M",
    # so we'll need to extract the memory amount and convert it to GB if necessary.
    # We'll take the memory info from the first row assuming it's consistent across all rows
    mem_info <- gsub(".*mem=([0-9]+)([GM]).*", "\\1\\2", job_info[1])
    
    # If memory is in MB, convert to GB
    allocated_mem_gb <- ifelse(
        grepl("M", mem_info),
        as.numeric(substr(mem_info, 1, nchar(mem_info) - 1)) / 1024,
        as.numeric(substr(mem_info, 1, nchar(mem_info) - 1))
    )
    
    list(
        job_id = slurm_job_id,
        CPUs = allocated_cpus,
        Memory_GB = allocated_mem_gb
    )
}

init_multisession <- function() {
  if (!is.na(Sys.getenv("SLURM_JOB_ID"))) {
    # Running within a SLURM job
    resources <- slurm_allocation()
    options(future.globals.maxSize = resources$Memory_GB * 1024^3)
    future::plan("future::multisession", workers = resources$CPUs)
  } else {
    # Not running within a SLURM job
    # Determine the system's total memory size in GB (example for Linux systems)
    system_memory_gb <- as.numeric(system("grep MemTotal /proc/meminfo | awk '{print $2/1024/1024}'", intern = TRUE))
    options(future.globals.maxSize = system_memory_gb * 1024^3)
    future::plan("future::multisession")
  }
}

get_usage_summary <- function() {
  # Get memory info
  mem_info <- ps::ps_memory_info()
  ram_usage_gb <- round(mem_info[1] / (1024^3), 2)  # Convert from bytes to GB

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


log_message <- function(message_str, log = TRUE) {
# Set up the message
job_id <- Sys.getenv("SLURM_JOB_ID")
usage_summary <- get_usage_summary()
dir.create(here::here("tmp"), showWarnings = FALSE)
log_file <- glue::glue("tmp/job_{job_id}.log")
message_str <- glue::glue("{message_str}\n{usage_summary}}")
# Print the message
message(message_str)
# Log it
    if (!is.null(log)) {
        write(message_str,
        file = log_file,
        append = TRUE
        )
    }
}

log_params <- function(log = TRUE) {
  # Capture arguments from the calling function
  args <- as.list(sys.call(-1L)[-1L])
  arg_strs <- sapply(names(args), function(name) {
    paste0(name, " = ", deparse(args[[name]]))
  }, USE.NAMES = FALSE)
  arg_list_str <- paste(arg_strs, collapse = "; ")

  # Construct the message
  message_str <- glue::glue("Arguments: {arg_list_str}")

  # Print the message
  message(message_str)
  
  # Log it
  if (!is.null(log)) {
    # Set up the log file
    job_id <- Sys.getenv("SLURM_JOB_ID")
    dir.create(here::here("log"), showWarnings = FALSE)
    log_file <- glue::glue("log/job_{job_id}.log")
    write(message_str,
          file = log_file,
          append = TRUE
    )
  }
}
