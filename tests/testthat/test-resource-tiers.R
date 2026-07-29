library(testthat)
library(hprcc)

# Guards against a class of failure that only surfaces at runtime: the exported
# tar_resources objects in R/targets_resource_shortcuts.R name their controller
# as a string, so a typo or a rename on one side alone produces a target that
# dispatches to a controller that does not exist. Nothing else in the package
# checks that the two lists agree.

test_that("every exported resource shortcut names a registered controller", {
    registered <- names(targets::tar_option_get("controller")$controllers)
    expect_true(length(registered) > 0)

    shortcuts <- c(
        "tiny", "small", "medium", "large", "large_mem", "large_mem_2x",
        "long", "large_mem_xl", "xlarge", "huge"
    )

    for (nm in shortcuts) {
        res <- get(nm, envir = asNamespace("hprcc"))
        named <- res$crew$controller
        expect_true(
            named %in% registered,
            info = paste0(
                "shortcut `", nm, "` names controller `", named,
                "`, which is not registered. Registered: ",
                paste(registered, collapse = ", ")
            )
        )
    }
})

test_that("controllers default to one task per worker", {
    # tasks_max = Inf (crew's default) lets a worker accept a second target and
    # then be killed at the SLURM walltime part-way through it, discarding that
    # work. Default is 1L so slurm_walltime_minutes is a per-task limit.
    ctrl <- create_controller(
        "tasks_max_default_probe",
        slurm_cpus = 1L,
        slurm_mem_gigabytes = 1L
    )
    expect_equal(ctrl$launcher$tasks_max, 1L)
})

test_that("tasks_max is overridable", {
    ctrl <- create_controller(
        "tasks_max_override_probe",
        slurm_cpus = 1L,
        slurm_mem_gigabytes = 1L,
        tasks_max = 25L
    )
    expect_equal(ctrl$launcher$tasks_max, 25L)
})

test_that("large_mem_2x memory fits the target cluster's nodes", {
    # apollo's `all` partition has 20 nodes at 239 GB usable; a 250 GB request
    # would exclude them and leave only 10 candidates, recreating the scarcity
    # this tier exists to avoid. gemini `compute` holds 250 GB on all 85.
    registered <- targets::tar_option_get("controller")$controllers
    mem <- registered[["large_mem_2x"]]$launcher$options_cluster$memory_gigabytes_required
    expected <- if (get_cluster() == "apollo") 200L else 250L
    expect_equal(mem, expected)
})
