# SLURM Job Submission for External Tools
# Fire-and-track pattern for running containerized tools via SLURM
# Returns slurm_job_result objects for targets integration

#' Submit a SLURM Job with Completion Tracking
#'
#' Submits a shell command as a SLURM job and tracks completion via output files.
#' Implements a "fire and track" pattern for targets integration:
#' 1. Check if completion files exist (job already done) -> return complete result
#' 2. Check if script exists with SLURM output (already submitted) -> check status
#' 3. Otherwise, create script and submit via sbatch -> return submitted result
#'
#' Use with `cue = tar_cue(mode = "always")` and `deployment = "main"` in targets.
#'
#' @param name Character. Job name used for script filename and SLURM job name.
#' @param command Character. The shell command to execute.
#' @param working_dir Character. Directory for script creation and job execution.
#' @param completion_files Character vector. Paths (relative to working_dir or
#'   absolute) that indicate successful completion. First found file is returned.
#' @param slurm_options Named list. SLURM parameters:
#'   \itemize{
#'     \item time: Wall time (e.g., "24:00:00")
#'     \item mem: Total memory (e.g., "64G")
#'     \item cpus_per_task: Number of CPUs (integer)
#'     \item partition: SLURM partition (default from cluster)
#'     \item gres: Generic resources (e.g., "gpu:1")
#'     \item dependency: Job dependency string (usually set by [dispatch_with_deps()])
#'   }
#' @param modules_to_load Character vector. Modules to load (e.g., "singularity").
#' @param env_vars Named list. Environment variables to export.
#' @param force_resubmit Logical. If TRUE, resubmit even if already submitted.
#'
#' @return A [slurm_job_result] object.
#'
#' @export
#' @importFrom cli cli_alert_success cli_alert_info cli_alert_warning cli_abort
#'
#' @examples
#' \dontrun{
#' tar_target(
#'   my_job,
#'   run_slurm_job(
#'     name = "my_analysis",
#'     command = "python script.py --input data.csv",
#'     working_dir = "/scratch/user/analysis",
#'     completion_files = "results/output.csv",
#'     slurm_options = list(time = "4:00:00", mem = "32G", cpus_per_task = 4L)
#'   ),
#'   cue = tar_cue(mode = "always"),
#'   deployment = "main"
#' )
#' }
run_slurm_job <- function(
    name,
    command,
    working_dir,
    completion_files,
    slurm_options = list(),
    modules_to_load = NULL,
    env_vars = NULL,
    force_resubmit = FALSE
) {
    stopifnot(
        is.character(name), length(name) == 1L, nzchar(name),
        is.character(command), length(command) == 1L,
        is.character(working_dir), length(working_dir) == 1L,
        is.character(completion_files), length(completion_files) >= 1L,
        is.list(slurm_options),
        is.null(modules_to_load) || is.character(modules_to_load),
        is.null(env_vars) || is.list(env_vars),
        is.logical(force_resubmit), length(force_resubmit) == 1L
    )

    working_dir <- normalizePath(working_dir, mustWork = FALSE)
    script_path <- file.path(working_dir, paste0(name, ".sh"))

    # Inject dependency from dispatch_with_deps if present
    injected_dep <- getOption("slurm_job_dependency")
    if (!is.null(injected_dep)) {
        if (is.null(slurm_options$dependency)) {
            slurm_options$dependency <- injected_dep
        } else {
            slurm_options$dependency <- paste0(
                slurm_options$dependency, ":",
                gsub("^afterok:", "", injected_dep)
            )
        }
    }

    # Resolve completion file paths
    resolved_completion <- vapply(completion_files, function(f) {
        if (startsWith(f, "/")) f else file.path(working_dir, f)
    }, character(1))

    # 1. Check completion files
    if (!force_resubmit) {
        for (full_path in resolved_completion) {
            if (file.exists(full_path)) {
                cli::cli_alert_success("Job complete: {name}")
                return(slurm_job_result(
                    full_path, NULL, "complete",
                    working_dir, resolved_completion
                ))
            }
        }
    }

    # 2. Check if already submitted
    if (file.exists(script_path) && !force_resubmit) {
        slurm_files <- list.files(
            working_dir, pattern = "^slurm-[0-9]+\\.out$", full.names = TRUE
        )
        if (length(slurm_files) > 0) {
            existing_job_id <- max(
                gsub(".*slurm-([0-9]+)\\.out", "\\1", slurm_files)
            )
            status <- check_slurm_status(existing_job_id)

            if (status %in% c("failed", "cancelled", "timeout")) {
                cli::cli_alert_warning(
                    "Previous job {existing_job_id} {status}, cleaning up..."
                )
                file.remove(script_path)
                file.remove(slurm_files)
            } else if (status %in% c("pending", "running", "submitted", "unknown")) {
                cli::cli_alert_info("Job already submitted: {name}")
                return(slurm_job_result(
                    script_path, existing_job_id, "already_submitted",
                    working_dir, resolved_completion
                ))
            }
        } else {
            cli::cli_alert_warning("Stale script found for {name}, cleaning up...")
            file.remove(script_path)
        }
    }

    # 3. Create directory and script
    if (!dir.exists(working_dir)) {
        dir.create(working_dir, recursive = TRUE, showWarnings = FALSE)
    }

    script_content <- generate_slurm_script(
        name = name,
        command = command,
        working_dir = working_dir,
        slurm_options = slurm_options,
        modules_to_load = modules_to_load,
        env_vars = env_vars
    )

    writeLines(script_content, script_path)
    Sys.chmod(script_path, mode = "0755")

    # 4. Submit job
    submit_result <- system2("sbatch", script_path, stdout = TRUE, stderr = TRUE)
    exit_status <- attr(submit_result, "status")

    if (!is.null(exit_status) && exit_status != 0) {
        cli::cli_abort(c(
            "sbatch failed with exit code {exit_status}",
            "x" = paste(submit_result, collapse = "\n")
        ))
    }

    # Parse job ID
    job_line <- grep("Submitted batch job", submit_result, value = TRUE)
    if (length(job_line) == 0) {
        cli::cli_abort(c(
            "sbatch did not return job ID",
            "x" = paste(submit_result, collapse = "\n")
        ))
    }
    job_id <- gsub(".*Submitted batch job ([0-9]+).*", "\\1", job_line[1])

    dep_msg <- if (!is.null(slurm_options$dependency)) {
        paste0(", depends: ", slurm_options$dependency)
    } else {
        ""
    }
    cli::cli_alert_success("Submitted: {name} (ID: {job_id}{dep_msg})")

    slurm_job_result(
        script_path, job_id, "submitted",
        working_dir, resolved_completion
    )
}


#' Submit a Singularity Container Job via SLURM
#'
#' Convenience wrapper around [run_slurm_job()] for running commands inside
#' Singularity containers. Automatically handles bind paths and GPU settings.
#'
#' @inheritParams run_slurm_job
#' @param container Character. Path to the Singularity .sif file.
#' @param command Character. Command to run inside the container.
#' @param bind_paths Character vector. Additional paths to bind into container.
#'   Merged with cluster defaults from [singularity_bind_dirs()].
#' @param gpu Logical. If TRUE, adds `--nv` flag, sets `gres=gpu:1`,
#'   and selects partition via [choose_gpu_partition()].
#'
#' @return A [slurm_job_result] object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tar_target(
#'   cellbender_result,
#'   run_singularity_job(
#'     name = paste0("cellbender_", sample_id),
#'     container = "/packages/singularity/shared_cache/cellbender.sif",
#'     command = "cellbender remove-background --cuda ...",
#'     working_dir = output_dir,
#'     completion_files = "output.h5",
#'     gpu = TRUE,
#'     slurm_options = list(time = "4:00:00", cpus_per_task = 4L, mem = "60G")
#'   ),
#'   cue = tar_cue(mode = "always"),
#'   deployment = "main"
#' )
#' }
run_singularity_job <- function(
    name,
    container,
    command,
    working_dir,
    completion_files,
    bind_paths = NULL,
    gpu = FALSE,
    slurm_options = list(),
    modules_to_load = NULL,
    env_vars = NULL,
    force_resubmit = FALSE
) {
    stopifnot(
        is.character(container), length(container) == 1L,
        is.logical(gpu), length(gpu) == 1L
    )

    if (!file.exists(container)) {
        cli::cli_abort("Container not found: {.path {container}}")
    }

    # GPU-specific options
    if (gpu) {
        slurm_options$gres <- slurm_options$gres %||% "gpu:1"
        slurm_options$partition <- slurm_options$partition %||% choose_gpu_partition()
    }

    # Build bind paths: cluster defaults + user-specified
    default_binds <- tryCatch(
        strsplit(singularity_bind_dirs(), ",")[[1]],
        error = function(e) c("/scratch", "/packages", "/ref_genomes")
    )
    all_binds <- unique(c(default_binds, bind_paths))
    all_binds <- all_binds[dir.exists(all_binds)]

    # Ensure working_dir root is bound
    working_dir_norm <- normalizePath(working_dir, mustWork = FALSE)
    working_dir_root <- strsplit(working_dir_norm, "/")[[1]][2]
    if (!is.na(working_dir_root) && nzchar(working_dir_root)) {
        root_path <- paste0("/", working_dir_root)
        if (!root_path %in% all_binds) {
            all_binds <- c(all_binds, root_path)
        }
    }

    bind_string <- paste(all_binds, collapse = ",")

    # Build singularity command
    sing_bin <- tryCatch(singularity_bin(), error = function(e) "singularity")

    full_command <- paste(
        c(
            sing_bin, "exec",
            if (gpu) "--nv" else NULL,
            "-B", bind_string,
            container,
            "bash", "-c", shQuote(command)
        ),
        collapse = " "
    )

    # Ensure singularity module is loaded
    modules <- unique(c("singularity", modules_to_load))

    run_slurm_job(
        name = name,
        command = full_command,
        working_dir = working_dir,
        completion_files = completion_files,
        slurm_options = slurm_options,
        modules_to_load = modules,
        env_vars = env_vars,
        force_resubmit = force_resubmit
    )
}


#' Generate SLURM Script Content
#'
#' @param name Job name
#' @param command Shell command to run
#' @param working_dir Working directory
#' @param slurm_options Named list of SLURM options
#' @param modules_to_load Character vector of modules
#' @param env_vars Named list of environment variables
#'
#' @return Character string with full script content
#' @keywords internal
generate_slurm_script <- function(
    name,
    command,
    working_dir,
    slurm_options,
    modules_to_load,
    env_vars
) {
    # Resolve defaults
    default_part <- tryCatch(default_partition(), error = function(e) NULL)
    defaults <- list(time = "02:00:00", mem = "8G", cpus_per_task = 1L)
    if (!is.null(default_part)) defaults$partition <- default_part
    opts <- modifyList(defaults, slurm_options)

    # Header
    lines <- c(
        "#!/bin/bash",
        paste0("#SBATCH --job-name=", name),
        "#SBATCH --export=NONE",
        "#SBATCH --get-user-env=L",
        paste0("#SBATCH --output=", working_dir, "/slurm-%j.out"),
        paste0("#SBATCH --error=", working_dir, "/slurm-%j.out")
    )

    # SLURM options
    option_map <- list(
        time = "time",
        mem = "mem",
        cpus_per_task = "cpus-per-task",
        partition = "partition",
        gres = "gres",
        account = "account",
        qos = "qos",
        dependency = "dependency"
    )

    for (opt_name in names(opts)) {
        if (opt_name %in% names(option_map)) {
            sbatch_name <- option_map[[opt_name]]
        } else {
            sbatch_name <- gsub("_", "-", opt_name)
        }
        lines <- c(lines, sprintf("#SBATCH --%s=%s", sbatch_name, opts[[opt_name]]))
    }

    # Setup
    lines <- c(
        lines, "",
        "# --- Setup ---",
        'echo "Job started on $(hostname) at $(date)"',
        sprintf('cd "%s" || exit 1', working_dir)
    )

    # Modules
    if (!is.null(modules_to_load) && length(modules_to_load) > 0) {
        lines <- c(lines, "", "# --- Load Modules ---")
        for (mod in modules_to_load) {
            lines <- c(lines, sprintf("module load %s", mod))
        }
    }

    # Environment variables
    if (!is.null(env_vars) && length(env_vars) > 0) {
        lines <- c(lines, "", "# --- Environment ---")
        for (var_name in names(env_vars)) {
            lines <- c(lines, sprintf('export %s="%s"', var_name, env_vars[[var_name]]))
        }
    }

    # Command with signal handling
    lines <- c(
        lines, "",
        "# --- Execute Command ---",
        paste0(command, " &"),
        "cmd_pid=$!",
        'trap "kill -TERM $cmd_pid 2>/dev/null" SIGTERM SIGINT',
        "wait $cmd_pid",
        "exit_code=$?", "",
        "# --- Completion ---",
        'echo "Job finished at $(date) with exit code $exit_code"',
        "exit $exit_code"
    )

    paste(lines, collapse = "\n")
}
