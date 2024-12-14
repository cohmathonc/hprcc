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
Sys.setenv(TAR_WARN = "false")

old <- getwd()
dir <- paste0(old, "/targets_temp_dir")
dir.create(dir, showWarnings = FALSE)
setwd(dir)

tar_config_set(store = paste0(dir, "/_targets"))

tar_script(
    {
        options(hprcc.slurm_logs = TRUE, hprcc.slurm_jobs = TRUE)
        tar_source("../R")
        configure_targets_options()
        controller <- create_controller("tiny", slurm_cpus = 2, slurm_mem_gigabytes = 8, slurm_walltime_minutes = 60)
        log_hprcc_settings()
        targets::tar_option_set(
            format = "qs",
            storage = "worker",
            retrieval = "worker",
            controller = crew::crew_controller_group(controller)
        )
        list(
            tar_target(y1, 1 + 1, deployment = "main"),
            tar_target(y2, 1 + 1, resources = tiny),
            tar_target(z, y1 + y2, resources = tiny)
        )
    },
    ask = FALSE
)

tar_make()

setwd(old)
unlink(dir, recursive = TRUE)
