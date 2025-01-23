library(targets)

# Set up temporary directory
old_dir <- getwd()
temp_dir <- file.path(old_dir, "targets_temp_dir")
dir.create(temp_dir, showWarnings = FALSE)
setwd(temp_dir)

# Configure targets store
tar_config_set(store = file.path(temp_dir, "_targets"))

# Create _targets.R file in the temporary directory
tar_script(
    {
        library(targets)
        library(crew.cluster)
        library(crew)

        # Create job scripts directory if it doesn't exist
        slurm_script_dir <- here::here(getwd(), "job_scripts")
        dir.create(slurm_script_dir, showWarnings = FALSE, recursive = TRUE)

        # Configure SLURM script and options
        script_lines <-
            # if cluster is apollo
            if (hprcc::get_cluster() == "apollo") {
                glue::glue("/opt/singularity/3.7.0/bin/singularity exec \\
    --env R_LIBS_SITE=/opt/singularity-images/rbioc/rlibs/bioc-3.19 \\
    -B /labs,/opt/singularity,/opt/singularity-images \\
    /opt/singularity-images/rbioc/vscode-rbioc_3.19.sif \\")
            } else {
                # if cluster is not apollo
                glue::glue("/packages/easy-build/software/singularity/3.7.0/bin/singularity exec \\
    --env R_LIBS_SITE=/packages/singularity/shared_cache/rbioc/rlibs/bioc-3.19 \\
    -B /packages,/run,/ref_genomes,/scratch \\
    /packages/singularity/shared_cache/rbioc/vscode-rbioc_3.19.sif \\")
            }
        # Configure the SLURM controller
        slurm_opts <- crew.cluster::crew_options_slurm(
            verbose = TRUE,
            memory_gigabytes_required = c(4, 8, 16),
            cpus_per_task = 1,
            time_minutes = 5,
            script_lines = script_lines,
            log_output = here::here(slurm_script_dir, "crew_log_%A.txt"),
            log_error = here::here(slurm_script_dir, "crew_log_%A.txt"),
            script_directory = slurm_script_dir
        )

        slurm_controller <- crew.cluster::crew_controller_slurm(
            name = "example_pipeline",
            workers = 4,
            tasks_max = 2,
            seconds_idle = 10,
            options_cluster = slurm_opts,
            options_metrics = crew::crew_options_metrics(path = "/dev/stdout", seconds_interval = 1)
        )

        # set the controller as default
        tar_option_set(controller = slurm_controller)

        # Define pipeline functions
        get_data <- function(n) {
            data.frame(
                x = rnorm(n),
                y = rnorm(n)
            )
        }

        fit_model <- function(data) {
            Sys.sleep(10) # Simulate long computation
            lm(y ~ x, data = data)
        }

        analyze_results <- function(model) {
            summary(model)$r.squared
        }

        # Define the pipeline targets
        list(
            tar_target(
                name = data,
                command = get_data(10000),
                deployment = "worker"
            ),

            tar_target(
                name = model,
                command = fit_model(data),
                deployment = "worker"
            ),

            tar_target(
                name = results,
                command = analyze_results(model),
                deployment = "worker"
            )
        )
    },
    ask = FALSE
)

# Run the pipeline
tar_make()

# Clean up
setwd(old_dir)
unlink(temp_dir, recursive = TRUE)







tar_script({
# Base paths that change based on cluster
singularity_bin <- if (hprcc::get_cluster() == "apollo") {
    "/opt/singularity/3.7.0/bin/singularity"
} else {
    "/packages/easy-build/software/singularity/3.7.0/bin/singularity"
}

base_dir <- if (hprcc::get_cluster() == "apollo") {
    "/opt/singularity-images/rbioc"
} else {
    "/packages/singularity/shared_cache/rbioc"
}
singularity_exec <- glue::glue("cd {here::here()} \
{singularity_bin} exec \\
-B {base_dir} \\
--env R_LIBS_SITE={base_dir}/rlibs/bioc-3.19 \\
{base_dir}/vscode-rbioc_3.19.sif \\")
    slurm <- crew.cluster::crew_controller_slurm(
    options_cluster = crew.cluster::crew_options_slurm(script_lines = singularity_exec))

    tar_option_set(controller = slurm)

    list(
        tar_target(y1, 1 + 1),
        tar_target(y2, 1 + 1),
        tar_target(z, y1 + y2)
    )}, ask = FALSE)
tar_make()



## Integration test for the hprcc package
# Run this using git hook pre-push
# your local .git/hooks/pre-push should look like

# Rscript inst/extdata/integration_test.R

# # Check the exit status of the R script
# if [ $? -ne 0 ]; then
# echo "Integration tests failed. Push aborted."
# exit 1
# fi

# # If the script exits with 0, the push will proceed
# exit 0

library(targets)

old_dir <- getwd()
dir <- paste0(old_dir, "/targets_temp_dir")
dir.create(dir, showWarnings = FALSE)
setwd(dir)

tar_config_set(store = paste0(dir, "/_targets"))

tar_script(
    {
        options(hprcc.slurm_logs = TRUE, hprcc.slurm_jobs = TRUE, hprcc.default_partition = "fast,all")
        tar_source("../R")
        configure_targets_options()
        Sys.setenv("R_LIBS_USER" = "")
        log_hprcc_settings()

        list(
            tar_target(y1, {Sys.sleep(2);1 + 1}, resources = small),
            tar_target(y2, 1 + 1, resources = tiny),
            tar_target(z, y1 + y2, resources = tiny)
        )
    },
    ask = FALSE
)

tar_make()

setwd(old_dir)
unlink(dir, recursive = TRUE)

# library(targets)
# Sys.setenv(TAR_WARN = "false")

# old <- getwd()
# dir <- paste0(old, "/targets_temp_dir")
# dir.create(dir, showWarnings = FALSE)
# setwd(dir)

# tar_config_set(store = paste0(dir, "/_targets"))

# tar_script(
#     {
#         options(hprcc.slurm_logs = TRUE, hprcc.slurm_jobs = TRUE)
#         Sys.setenv("TMPDIR" = "/home/domeally/R/jobs")
#         library(hprcc)

#         list(
#             tar_target(y1, 1 + 1, deployment = "main"),
#             tar_target(y2, 1 + 1, resources = tiny),
#             tar_target(z, y1 + y2, resources = tiny)
#         )
#     },
#     ask = FALSE
# )

# tar_make()

# setwd(old)
# unlink(dir, recursive = TRUE)