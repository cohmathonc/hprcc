# Configurations for {crew.cluster} controllers
# On the COH HPRCC

#-----------------------------------------------------------------------------

#' Package Options for hprcc
#'
#' The **hprcc** package has a number of settings that can be configured
#' via [options()][base::options] or environment variables, providing
#' the flexibility to use it with any containerized environment supporting
#' R and [targets][targets::targets-package] (>=1.9.1).
#'
#' Options can be set by calling [options()][base::options] _before_ loading the **hprcc** package in
#' `_targets.R`. Option settings take precedence over environment variables, where
#' indicated below. If no `options` are set, the default configuration
#' runs the [RStudio for Bioconductor](http://hprcc.coh.org/user-guide/rbioc/) container.
#'
#' @section Options:
#' \describe{
#'   \item{hprcc.slurm_logs}{logical. Enable SLURM job & [autometric](https://wlandau.github.io/autometric/index.html)
#'         logging. If `TRUE`, logs are saved to `logs/` in [targets::tar_path_store()]. Logs capture the `stderr`
#'         and `stdout` of each SLURM job, and can be parsed by [autometric::log_read()]. \cr
#'         Default: `FALSE`.}
#'   \item{hprcc.slurm_verbose}{logical. Show SLURM messages in the console. \cr
#'         Default: `FALSE`}
#'   \item{hprcc.slurm_jobs}{logical. Write SLURM submission scripts to `jobs/ in `[targets::tar_path_store()]; use the
#'         default of `$TMPDIR` if `FALSE`. \cr
#'         Default: `FALSE`}
#'   \item{hprcc.slurm_account}{character. SLURM account for job submission. \cr
#'         Default: `$USER`}
#'   \item{hprcc.r_libs_user}{Path to user R libraries. \cr
#'         Environment: `$R_LIBS_USER` \cr
#'         Default: `"~/R/x86_64-pc-linux-gnu-library/%V"`}
#'   \item{hprcc.r_libs_site}{Site-specific library path. \cr
#'         Environment: `$R_LIBS_SITE` \cr
#'         Apollo default: `"/opt/singularity-images/rbioc/rlibs/bioc-$BIOCONDUCTOR_VERSION"` \cr
#'         Gemini default: `"/packages/singularity/shared_cache/rbioc/rlibs/bioc-$BIOCONDUCTOR_VERSION"`}
#'   \item{hprcc.singularity_bin}{Path to the Singularity binary. \cr
#'         Environment: `$SINGULARITY_BIN` \cr
#'         Apollo default: `"/opt/singularity/3.7.0/bin/singularity"` \cr
#'         Gemini default: `"/packages/easy-build/software/singularity/3.7.0/bin/singularity"`}
#'   \item{hprcc.singularity_container}{Path to the Singularity image. \cr
#'         Environment: `$SINGULARITY_CONTAINER` \cr
#'         Apollo default: `"/opt/singularity-images/rbioc/vscode-rbioc_$BIOCONDUCTOR_VERSION.sif"` \cr
#'         Gemini default: `"/packages/singularity/shared_cache/rbioc/vscode-rbioc_$BIOCONDUCTOR_VERSION.sif"`}
#'   \item{hprcc.bind_dirs}{Directories to bind in the Singularity container. \cr
#'         Environment: `$SINGULARITY_BIND` \cr
#'         Apollo default: `"/labs,/opt,/ref_genome,/run"` \cr
#'         Gemini default: `"/packages,/run,/ref_genomes,/scratch"`}
#'   \item{hprcc.default_partition}{Default SLURM partition. Automatically detected using \code{"scontrol show partition"}. \cr
#'         Default: Dynamically retrieved default partition from SLURM configuration.}
#' }
#'
#' @keywords package
#' @seealso \code{\link{add_controller}} for creating SLURM job controllers
#' @name package-options
#' @aliases hprcc-package
NULL

# Env for storing package settings
HPRCC <- new.env(parent = environment())

#' Determine Cluster Based on Hostname
#'
#' Retrieves the name of the COH HPRCC cluster by matching the system's hostname
#' against known patterns. It supports 'apollo' and 'gemini' clusters.
#'
#' @return A character string: either "apollo" or "gemini".
#' @export
#' @examples
#' if (interactive()) {
#'     get_cluster()
#' }
#' @note This function will throw an error if the hostname does not match any known cluster pattern.
get_cluster <- function() {
    hostname <- as.character(Sys.info()["nodename"])
    if (grepl("ppxhpc", hostname)) {
        return("apollo")
    } else if (grepl("^g-[a-z]-[0-9]-[0-9]-[0-9]{2}|^gemini", hostname)) {
        return("gemini")
    } else {
        warning("Unknown cluster")
        return(NULL)
    }
}

#' Set Up a Controller for SLURM Jobs on COH Clusters
#'
#' Configures and initializes a [controller][crew.cluster::crew_controller_slurm]
#' for managing SLURM jobs on City of Hope clusters using the [crew.cluster][crew.cluster::crew.cluster-package]
#' package to facilitate job execution, managing resources such as CPU, memory, walltime, and
#' writing SLURM logs and scripts.
#'
#' @param name A unique identifier for the controller.
#' @param slurm_cpus Number of CPU cores allocated to each task.
#' @param slurm_mem_gigabytes Memory allocated to each task, in gigabytes.
#' @param slurm_walltime_minutes Maximum allowed execution time per task, in minutes. Defaults to 720 (12 hours).
#' @param slurm_workers Total number of parallel tasks the controller can handle. Defaults to 350.
#' @param slurm_partition SLURM partition for job submission. Default set by cluster.
#' @param tasks_max Number of targets a single worker will run before exiting.
#'   Defaults to `1L`: one task per worker, so each target gets a fresh R process
#'   sized for it and `slurm_walltime_minutes` means what it says - a per-task
#'   limit.
#'
#'   This was previously unset, taking `crew`'s default of `Inf`, which meant a
#'   worker kept accepting targets until it idled out or SLURM killed it at the
#'   walltime. That silently discarded work: a worker would finish target A, start
#'   target B, and be killed mid-B, so B's hours were lost and it had to be redone
#'   on a later worker. Observed in a scRNA-seq pipeline where all 6 workers on a
#'   long-running SCTransform step ended `FAILED` rather than `COMPLETED`, each
#'   part-way through a second task.
#'
#'   Raise it only for many short targets, where process startup dominates and no
#'   single target comes close to the walltime.
#' See [package options][package-options] for defaults.
#'
#' @details
#' `create_controller` streamlines SLURM job setup on COH clusters using
#' Singularity containers for consistent computing environments. Singularity containers
#' package software and dependencies, ensuring that jobs run reliably across both
#' clusters. This approach aids in computational reproducibility and
#' solves environment inconsistencies between Apollo and Gemini.
#'
#' The function allows customization of SLURM job parameters, including CPUs, memory,
#' and walltime, while managing SLURM logs and script directories. It abstracts cluster-specific
#' configurations, making it easier to run jobs without detailed knowledge of the underlying
#' cluster setup. This functionality is especially useful in environments with varying
#' resource paths and setups, simplifying job execution across the two platforms.
#'
#' @return A `crew_controller` object, ready to manage SLURM job submissions and monitoring.
#' @keywords internal
#' @examples
#' \dontrun{
#'  # Basic controller with minimal resources
#'  ctrl <- create_controller("test",
#'                         slurm_cpus = 2,
#'                         slurm_mem_gigabytes = 8)
#' }
#' # GPU configuration on Gemini
#' \dontrun{
#'  if (get_cluster() == "gemini") {
#'       gpu_ctrl <- create_controller("gpu_job",
#'                               slurm_cpus = 4,
#'                               slurm_mem_gigabytes = 60,
#'                               slurm_partition = "gpu-a100")
#'  }
#' }
#' @importFrom glue glue
#' @importFrom here here
#' @importFrom crew.cluster crew_controller_slurm
#' @seealso \code{\link[crew.cluster]{crew_controller_slurm}} for more on SLURM controllers.
create_controller <- function(
    name,
    slurm_cpus,
    slurm_mem_gigabytes,
    slurm_walltime_minutes = 720L,
    slurm_workers = 350L,
    slurm_partition = default_partition(),
    tasks_max = 1L
) {
    # GPU check
    if (grepl("gpu", slurm_partition)) {
        if (get_cluster() != "gemini") {
            stop("GPU jobs are only supported on the Gemini cluster.")
        }
        if (slurm_cpus > 8L) {
            stop(
                "For GPU partitions, the number of CPUs must be less than or equal to 8."
            )
        }
        gpu_req <- glue::glue("#SBATCH --gres gpu:1 \n#SBATCH --ntasks=1 \n")
    } else {
        gpu_req <- NULL
    }

    script_lines <- glue::glue(
        "{if (!is.null(gpu_req) && nzchar(gpu_req)) gpu_req else '\n'} ",
        "{HPRCC$slurm_account}\n",
        "cd {getwd()} \n",
        "{HPRCC$singularity_bin} exec {HPRCC$r_libs_user} \\
--env R_LIBS={HPRCC$r_libs_site}:/usr/local/lib/R/site-library:/usr/local/lib/R/library \\
--env R_LIBS_SITE={HPRCC$r_libs_site} \\
--env R_PARALLELLY_AVAILABLECORES_METHODS=Slurm \\
--env HPRCC_TARGETS_STORE_BASE={HPRCC$store_base} \\
-B {HPRCC$singularity_bind_dirs} \\
{HPRCC$singularity_container} \\"
    )

    slurm_options <- crew.cluster::crew_options_slurm(
        script_directory = HPRCC$slurm_jobs_dir,
        script_lines = script_lines,
        cpus_per_task = slurm_cpus,
        memory_gigabytes_required = slurm_mem_gigabytes,
        time_minutes = slurm_walltime_minutes,
        partition = slurm_partition,
        log_output = HPRCC$log_output,
        log_error = HPRCC$log_output,
        verbose = HPRCC$verbose_slurm
    )

    crew.cluster::crew_controller_slurm(
        name = name,
        workers = slurm_workers,
        seconds_idle = 30L,
        garbage_collection = TRUE,
        tasks_max = tasks_max,
        options_cluster = slurm_options,
        options_metrics = crew::crew_options_metrics(
            path = "/dev/stdout",
            seconds_interval = 1L
        )
    )
}


#' Add a Custom Controller to the Existing Controller Group
#'
#' Creates a custom SLURM controller and adds it to the existing
#' [crew controller group][crew::crew_controller_group]. This simplifies adding
#' ad-hoc controllers for jobs with specific resource requirements that don't
#' match the pre-defined controllers (tiny, small, medium, large, etc.).
#'
#' @param name A unique identifier for the controller. Must not conflict with
#'   existing controller names.
#' @param slurm_cpus Number of CPU cores allocated to each task.
#' @param slurm_mem_gigabytes Memory allocated to each task, in gigabytes.
#' @param slurm_walltime_minutes Maximum allowed execution time per task, in
#'   minutes. Defaults to 720 (12 hours).
#' @param slurm_partition SLURM partition for job submission. Defaults to the
#'   cluster's default partition. See [package options][package-options].
#' @param tasks_max Number of targets a single worker will run before exiting.
#'   Defaults to `1L`, matching [create_controller()]. Leave it at the default
#'   unless you have a specific reason not to: with `crew`'s own default of
#'   `Inf`, a worker keeps accepting targets until it idles out or SLURM kills
#'   it at the walltime, so a long second task can be killed part-way through
#'   and discard its hours. This argument was missing until 0.2.2 - custom
#'   controllers inherited the safe default but could not override it, and the
#'   behaviour was undocumented here.
#'
#' @details
#' This function is useful when a project requires custom SLURM resource
#' configurations beyond the pre-defined controllers. For example, a job might
#' need more walltime but less memory than the standard `large_mem` controller.
#'
#' The function modifies the targets controller group in place by calling
#' [targets::tar_option_set()]. It must be called after loading the hprcc
#' package (which sets up the default controller group) and before defining
#' targets that use the custom controller.
#'
#' @return Invisibly returns a `tar_resources` object for the new controller,
#'   which can be used directly in [targets::tar_target()] via the `resources`
#'   argument.
#'
#' @export
#' @examples
#' \dontrun{
#' library(hprcc)
#' library(targets)
#'
#' # Add a custom controller for long-running jobs
#' singler_resources <- add_controller(
#'     name = "singler",
#'     slurm_cpus = 16L,
#'     slurm_mem_gigabytes = 200L,
#'     slurm_walltime_minutes = 720L,
#'     slurm_partition = "bigmem"
#' )
#'
#' # Use in a target
#' tar_target(
#'     annotated_cells,
#'     annotate_with_singler(sce),
#'     resources = singler_resources
#' )
#' }
#' @seealso [SLURM-Resource-Configurations] for pre-defined resource shortcuts.
add_controller <- function(
    name,
    slurm_cpus,
    slurm_mem_gigabytes,
    slurm_walltime_minutes = 720L,
    slurm_partition = default_partition(),
    tasks_max = 1L
) {
    # Create the new controller
    new_controller <- create_controller(
        name = name,
        slurm_cpus = slurm_cpus,
        slurm_mem_gigabytes = slurm_mem_gigabytes,
        slurm_walltime_minutes = slurm_walltime_minutes,
        slurm_partition = slurm_partition,
        tasks_max = tasks_max
    )

    # Get the existing controller group
    existing_group <- targets::tar_option_get("controller")

    if (is.null(existing_group)) {
        cli::cli_abort(c(
            "No existing controller group found.",
            "i" = "Make sure {.pkg hprcc} is loaded before calling {.fn add_controller}."
        ))
    }

    # Check for duplicate controller names
    existing_names <- names(existing_group$controllers)
    if (name %in% existing_names) {
        cli::cli_abort(c(
            "Controller {.val {name}} already exists.",
            "i" = "Choose a unique name for your custom controller.",
            "i" = "Existing controllers: {.val {existing_names}}"
        ))
    }

    # Add the new controller to the list
    all_controllers <- c(
        existing_group$controllers,
        stats::setNames(list(new_controller), name)
    )

    # Update targets options with the new controller group
    targets::tar_option_set(
        controller = do.call(crew::crew_controller_group, unname(all_controllers))
    )

    cli::cli_alert_success(
        "Added controller {.val {name}} ({slurm_cpus} CPUs, {slurm_mem_gigabytes}GB, {slurm_walltime_minutes}min)"
    )

    # Return the tar_resources object for convenient use
    invisible(targets::tar_resources(
        crew = targets::tar_resources_crew(controller = name)
    ))
}

# Internal functions ---------------------------------------------------------

r_libs_site <- function() {
    if (!is.null(getOption("hprcc.r_libs_site"))) {
        return(getOption("hprcc.r_libs_site"))
    } else if (nzchar(Sys.getenv("R_LIBS_SITE"))) {
        return(Sys.getenv("R_LIBS_SITE"))
    } else if (get_cluster() == "apollo") {
        return(glue::glue(
            "/opt/singularity-images/rbioc/rlibs/bioc-{Sys.getenv('BIOCONDUCTOR_VERSION')}"
        ))
    } else if (get_cluster() == "gemini") {
        return(glue::glue(
            "/packages/singularity/shared_cache/rbioc/rlibs/bioc-{Sys.getenv('BIOCONDUCTOR_VERSION')}"
        ))
    } else {
        warning("Unknown cluster, please set R_LIBS_SITE env var or option")
    }
}

singularity_bin <- function() {
    if (!is.null(getOption("hprcc.singularity_bin"))) {
        return(getOption("hprcc.singularity_bin"))
    } else if (nzchar(Sys.getenv("SINGULARITY_BIN"))) {
        return(Sys.getenv("SINGULARITY_BIN"))
    } else if (get_cluster() == "apollo") {
        return("/opt/singularity/3.7.0/bin/singularity")
    } else if (get_cluster() == "gemini") {
        return(
            "/packages/easy-build/software/singularity/3.7.0/bin/singularity"
        )
    } else {
        warning("Unknown cluster, please set SINGULARITY_BIN env var or option")
    }
}

singularity_container <- function() {
    if (!is.null(getOption("hprcc.singularity_container"))) {
        return(getOption("hprcc.singularity_container"))
    } else if (nzchar(Sys.getenv("SINGULARITY_CONTAINER"))) {
        return(Sys.getenv("SINGULARITY_CONTAINER"))
    } else if (get_cluster() == "apollo") {
        return(glue::glue(
            "/opt/singularity-images/rbioc/vscode-rbioc_",
            Sys.getenv("BIOCONDUCTOR_VERSION"),
            ".sif"
        ))
    } else if (get_cluster() == "gemini") {
        return(glue::glue(
            "/packages/singularity/shared_cache/rbioc/vscode-rbioc_{Sys.getenv('BIOCONDUCTOR_VERSION')}.sif"
        ))
    } else {
        warning(
            "Unknown cluster, please set SINGULARITY_CONTAINER env var or option"
        )
    }
}

singularity_bind_dirs <- function() {
    if (!is.null(getOption("hprcc.singularity_bind_dirs"))) {
        return(getOption("hprcc.singularity_bind_dirs"))
    } else if (nzchar(Sys.getenv("SINGULARITY_BIND"))) {
        return(Sys.getenv("SINGULARITY_BIND"))
    } else if (get_cluster() == "apollo") {
        return("/labs,/opt,/ref_genome,/run")
    } else if (get_cluster() == "gemini") {
        return("/packages,/run,/ref_genomes,/scratch")
    } else {
        warning(
            "Unknown cluster, please set SINGULARITY_BIND env var or option"
        )
    }
}

slurm_default_partition <- function() {
    # Try to run scontrol command, return NULL if it fails
    tryCatch(
        {
            cmd_output <- system("scontrol show partition", intern = TRUE)
            current_partition <- NULL
            # Process each line
            for (i in seq_along(cmd_output)) {
                line <- cmd_output[i]
                # If line starts with PartitionName, get the partition name
                if (grepl("^PartitionName=", line)) {
                    current_partition <- sub(
                        "PartitionName=([^ ]+).*",
                        "\\1",
                        line
                    )
                } else if (
                    !is.null(current_partition) && grepl("Default=YES", line)
                ) {
                    return(current_partition)
                }
            }
            # Return NULL if no default partition found
            return(NULL)
        },
        error = function(e) {
            return(NULL)
        }
    )
}

default_partition <- function() {
    # Check for partition in options
    if (!is.null(getOption("hprcc.default_partition"))) {
        return(getOption("hprcc.default_partition"))
    }

    # Get the system default partition
    sys_default <- slurm_default_partition()
    if (!is.null(sys_default)) {
        return(sys_default)
    }

    warning(
        "Could not determine default partition, please set hprcc.default_partition option"
    )
    return(NULL)
}


# Default Targets options ----------------------------------------------------
#' @import autometric
#' @import qs2
configure_targets_options <- function() {
    # Populate the HPRCC environment
    HPRCC$r_libs_user <- if (
        nzchar(
            user_libs_path <- getOption(
                "hprcc.r_libs_user",
                Sys.getenv("R_LIBS_USER")
            )
        )
    ) {
        glue::glue("--env R_LIBS_USER={user_libs_path}")
    } else {
        ""
    }
    HPRCC$r_libs_site <- r_libs_site()
    HPRCC$slurm_account <- if (
        nzchar(account <- getOption("hprcc.slurm_account", ""))
    )
        glue::glue("#SBATCH --account {account}") else ""
    HPRCC$singularity_bin <- singularity_bin()
    HPRCC$singularity_bind_dirs <- singularity_bind_dirs()
    HPRCC$singularity_container <- singularity_container()

    HPRCC$use_jobs_dir <- isTRUE(getOption("hprcc.slurm_jobs", FALSE))
    # Workers receive the resolved store base via env var set by the main process
    if (nzchar(store_base_env <- Sys.getenv("HPRCC_TARGETS_STORE_BASE"))) {
        store_base <- store_base_env
    } else {
        store_base <- normalizePath(
            path.expand(targets::tar_path_store()),
            mustWork = FALSE
        )
    }
    HPRCC$store_base <- store_base
    HPRCC$slurm_jobs_dir <- if (HPRCC$use_jobs_dir) {
        normalizePath(file.path(store_base, "jobs"), mustWork = FALSE)
    } else {
        tempdir()
    }
    if (HPRCC$use_jobs_dir)
        dir.create(HPRCC$slurm_jobs_dir, recursive = TRUE, showWarnings = FALSE)

    HPRCC$use_slurm_log <- isTRUE(getOption("hprcc.slurm_logs", FALSE))
    HPRCC$log_output <- normalizePath(
        file.path(store_base, "logs", "crew-%j.out"),
        mustWork = FALSE
    )
    if (HPRCC$use_slurm_log) {
        dir.create(
            dirname(HPRCC$log_output),
            recursive = TRUE,
            showWarnings = FALSE
        )
    } else {
        HPRCC$log_output <- "/dev/null"
    }

    HPRCC$verbose_slurm <- isTRUE(getOption("hprcc.slurm_verbose", FALSE))

    # Define the common controllers
    controllers <- list(
        create_controller(
            "tiny",
            slurm_cpus = 2L,
            slurm_mem_gigabytes = 8L,
            slurm_walltime_minutes = 60L
        ),
        create_controller(
            "small",
            slurm_cpus = 2L,
            slurm_mem_gigabytes = 20L,
            slurm_walltime_minutes = 360L
        ),
        create_controller(
            "medium",
            slurm_cpus = 4L,
            slurm_mem_gigabytes = 40L,
            slurm_walltime_minutes = 360L
        ),
        create_controller(
            "large",
            slurm_cpus = 8L,
            slurm_mem_gigabytes = 80L,
            slurm_walltime_minutes = 360L
        ),
        create_controller(
            "large_mem",
            slurm_cpus = 8L,
            slurm_mem_gigabytes = 100L,
            slurm_walltime_minutes = 480L
        ),
        # Closes the 100 GB -> 600 GB gap reported in #33. Sized from measured
        # peaks across 1082 crew workers on a scRNA-seq pipeline: p99 was 220 GB
        # and the highest per-target peak 139 GB, so this covers the real tail
        # without leaving the general partition. Previously any target over 100 GB
        # had to jump 6x AND change partition.
        #
        # Cluster-gated, because the two clusters' node sizes differ and a tier
        # that fits one can be a scarcity trap on the other - the same reasoning as
        # the GPU gate below:
        #   gemini `compute`: 43 nodes @ 503 GB, 12 @ 754, 30 @ 1007  -> 250 GB
        #                     schedules on all 85
        #   apollo  `all`:     7 nodes @  54 GB, 20 @ 239, 5 @ 488, 5 @ 1465
        #                     -> 250 GB EXCLUDES the 20 mid-tier nodes (239 GB
        #                        usable), leaving only 10. 200 GB keeps all 30.
        create_controller(
            "large_mem_2x",
            slurm_cpus = 8L,
            slurm_mem_gigabytes = if (get_cluster() == "apollo") 200L else 250L,
            slurm_walltime_minutes = 720L
        ),
        # Above large_mem_2x but still off bigmem. Added after a second pipeline
        # (DCD.umass_kent.2025) reported measured peaks of 437.4 GB
        # (pankbase_integrated) and 252 GB (Erdem UCell) - real demand between
        # 250 GB and 600 GB that only large_mem_xl spanned, and that tier routes
        # to bigmem. Their cost was concrete: on 2026-07-27 bigmem was fully
        # allocated with 10+ jobs queued and SLURM estimated their start six days
        # out, for a target that would have run immediately on `compute`.
        #
        # Cluster-gated on node memory, as large_mem_2x is. 550 GB schedules on
        # gemini's 42 nodes with >=754 GB - still 8x bigmem's 5. Apollo has only
        # 10 nodes at >=488 GB, so it gets 450 GB to stay inside them rather than
        # recreating the scarcity this tier exists to avoid.
        create_controller(
            "large_mem_3x",
            slurm_cpus = 8L,
            slurm_mem_gigabytes = if (get_cluster() == "apollo") 450L else 550L,
            slurm_walltime_minutes = 720L
        ),
        # The other half of #33: a long-running target that is NOT memory-hungry
        # had no tier. Stock walltimes top out at 480 min on `compute`, so a
        # 12-hour step was forced onto large_mem_xl purely for its duration - and
        # dragged a 600 GB bigmem reservation with it, which then triggers
        # MaxMemoryPerAccount and throttles the same pipeline's other workers.
        #
        # 4 CPUs deliberately: the motivating workload (SCTransform per library)
        # measured 22.7 GB peak and 49.8% of ONE core over 712 minutes. It is
        # `future`-aware but parallelising it is counterproductive - on a
        # 4000-cell subset, sequential 34.7 s vs 4 workers 166.7 s (4.8x slower),
        # 8 workers OOM-killed, because future_lapply serialises the full model to
        # each worker. Many Seurat/Bioconductor steps are single-threaded like
        # this, so an 8- or 20-CPU tier wastes allocation and inflates the billing
        # weight that drives account limits.
        create_controller(
            "long",
            slurm_cpus = 4L,
            slurm_mem_gigabytes = 100L,
            slurm_walltime_minutes = 1440L
        ),
        # `extra_long`: same shape as `long`, 3x the hours. Added after a single
        # SCTransform branch ran 23h+ at 99% CPU and was killed at `long`'s 24h
        # ceiling, discarding the whole branch (haemProcessR#136). `long` was
        # sized from a measured peak of 712 min; that sample missed the tail.
        #
        # The 24h ceiling was never a cluster limit - `compute` has
        # MaxTime=14-00:00:00 - so this is a tier gap, not a scheduler one.
        #
        # 4320 min (3 days) is deliberately well clear of the 1387 min observed
        # rather than a tight fit, because runtime here is not predictable from
        # data size: across 15 libraries, cell count vs runtime gave Pearson
        # r = -0.167 (12,197 cells -> 1h48m; 12,386 cells -> 11h51m). Until a
        # real cost driver is identified (haemProcessR#135), headroom is the
        # only defence.
        create_controller(
            "extra_long",
            slurm_cpus = 4L,
            slurm_mem_gigabytes = 100L,
            slurm_walltime_minutes = 4320L
        ),
        # 2160 min (36h) is within bigmem's MaxTime of 2-00:00:00 and no QOS caps
        # it lower (cpubased allows 14 days), but it had never actually been
        # exercised: the installed 0.1.0 still had 720, so every large_mem_xl
        # worker to date was generated with `#SBATCH --time=720` and reported
        # Timelimit=12:00:00. Issue #33 read that as SLURM silently overriding the
        # request; it was simply the older installed value. Verified against the
        # generated job scripts, not inferred.
        create_controller(
            "large_mem_xl",
            slurm_cpus = 8L,
            slurm_mem_gigabytes = 600L,
            slurm_walltime_minutes = 2160L,
            slurm_partition = ifelse(get_cluster() == "apollo", "all", "bigmem")
        ),
        create_controller(
            "xlarge",
            slurm_cpus = 20L,
            slurm_mem_gigabytes = 200L,
            slurm_walltime_minutes = 720L
        ),
        create_controller(
            "huge",
            slurm_cpus = 40L,
            slurm_mem_gigabytes = 200L,
            slurm_walltime_minutes = 720L
        )
    )

    # Conditionally add GPU controllers if on the 'gemini' cluster
    if (get_cluster() == "gemini") {
        gpu_controllers <- list(
            create_controller(
                "gpu_medium",
                slurm_cpus = 4,
                slurm_mem_gigabytes = 60,
                slurm_walltime_minutes = 120,
                slurm_partition = "gpu-a100,gpu-v100"
            ),
            create_controller(
                "gpu_large",
                slurm_cpus = 8,
                slurm_mem_gigabytes = 120,
                slurm_walltime_minutes = 240,
                slurm_partition = "gpu-a100,gpu-v100"
            )
        )
        controllers <- c(controllers, gpu_controllers)
    }

    # Targets options
    targets::tar_option_set(
        format = "qs",
        storage = "worker",
        retrieval = "worker",
        controller = do.call(crew::crew_controller_group, controllers),
        resources = targets::tar_resources(
            crew = targets::tar_resources_crew(controller = "small")
        )
    )
}

# -----------------------------------------------------------------------------
.onAttach <- function(libname, pkgname) {
    # Only configure on known clusters (apollo or gemini)
    cluster <- suppressWarnings(get_cluster())
    if (!is.null(cluster)) {
        # Configure HPRCC environment so user options are respected
        # This allows options(hprcc.slurm_logs = TRUE) set before library() to work
        configure_targets_options()

        if (nzchar(Sys.getenv("SLURM_JOB_ID"))) {
            # Additional SLURM-specific configuration
            options(parallelly.availableCores.methods = "Slurm")
            if (isTRUE(getOption("hprcc.slurm_logs", FALSE))) {
                log_hprcc_settings()
            }
        }
    } else {
        packageStartupMessage(
            "Note: This package is designed for use on the City of Hope High Performance Research Computing Cluster (HPRCC). Some functionality may be limited on other systems."
        )
    }
}

.onLoad <- function(libname, pkgname) {
    # Set parallelly options
    if (nzchar(Sys.getenv("SLURM_JOB_ID")))
        options(parallelly.availableCores.methods = "Slurm")
}
