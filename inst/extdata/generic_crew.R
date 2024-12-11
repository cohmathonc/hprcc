library(targets)
Sys.setenv(TAR_WARN = "true")

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
        job_scripts_dir <- here::here(getwd(), "job_scripts")
        dir.create(job_scripts_dir, showWarnings = FALSE, recursive = TRUE)

        # Configure SLURM script and options
        script_lines <- glue::glue("/opt/singularity/3.7.0/bin/singularity exec \\
    --env R_LIBS_USER=/home/domeally/workspaces/hprcc/.library \\
    --env R_LIBS_SITE=/opt/singularity-images/rbioc/rlibs/bioc-3.19 \\
    -B /labs,/opt/singularity,/opt/singularity-images \\
    /opt/singularity-images/rbioc/vscode-rbioc_3.19.sif \\")

        # Configure the SLURM controller
        slurm_opts <- crew.cluster::crew_options_slurm(
            verbose = TRUE,
            memory_gigabytes_required = c(4, 8, 16),
            cpus_per_task = 1,
            time_minutes = 5,
            script_lines = script_lines,
            log_output = here::here(job_scripts_dir, "crew_log_%A.txt"),
            log_error = here::here(job_scripts_dir, "crew_log_%A.txt"),
            script_directory = job_scripts_dir
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
                command = get_data(1000),
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
    singularity_bin <- "/opt/singularity/3.7.0/bin/singularity"
    base_dir <- "/opt/singularity-images/rbioc"
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