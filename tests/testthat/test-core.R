library(testthat)

# Tests for get_cluster()
test_that("get_cluster returns 'apollo' for matching hostname", {
    with_mocked_bindings(
        Sys.info = function() list(nodename = "ppxhpc123"),
        {
            expect_equal(get_cluster(), "apollo")
        }
    )
})

test_that("get_cluster returns 'gemini' for matching hostname", {
    with_mocked_bindings(
        Sys.info = function() list(nodename = "g-a-1-2-34"),
        {
            expect_equal(get_cluster(), "gemini")
        }
    )
})

test_that("get_cluster gives a warning for unknown hostname", {
    with_mocked_bindings(
        Sys.info = function() list(nodename = "unknown"),
        {
            expect_warning(cluster <- get_cluster(), "Unknown cluster")
            expect_null(cluster)
        }
    )
})
# Tests for slurm_allocation()
test_that("slurm_allocation retrieves correct SLURM job resources", {
    mock_sys_getenv <- function(x) {
        switch(x,
            "SLURM_JOB_ID" = "123456",
            "SLURM_CPUS_PER_TASK" = "",
            "SLURM_CPUS_ON_NODE" = "6",
            "SLURM_MEM_PER_NODE" = "61440", # 60GB in MB
            "" # default for any other env var
        )
    }

    with_mocked_bindings(
        Sys.getenv = mock_sys_getenv,
        code = {
            allocation <- slurm_allocation()
            expect_equal(allocation$job_id, "123456")
            expect_equal(allocation$CPUs, 6)
            expect_equal(allocation$Memory_GB, 60)
        }
    )
})

test_that("slurm_allocation handles non-SLURM environment", {
    mock_sys_getenv <- function(x) "" # Return empty string for all env vars

    with_mocked_bindings(
        Sys.getenv = mock_sys_getenv,
        code = {
            expect_warning(allocation <- slurm_allocation(), "SLURM_JOB_ID not set")
            expect_null(allocation)
        }
    )
})

test_that("slurm_allocation handles partial environment variables", {
    mock_sys_getenv <- function(x) {
        switch(x,
            "SLURM_JOB_ID" = "123456",
            "SLURM_CPUS_ON_NODE" = "4", # Changed from SLURM_CPUS_PER_TASK to SLURM_CPUS_ON_NODE
            "SLURM_CPUS_PER_TASK" = "",
            "SLURM_MEM_PER_NODE" = "",
            ""
        )
    }

    with_mocked_bindings(
        Sys.getenv = mock_sys_getenv,
        code = {
            expect_warning(allocation <- slurm_allocation(), "SLURM_MEM_PER_NODE not set")
            expect_equal(allocation$job_id, "123456")
            expect_equal(allocation$CPUs, 4)
            expect_null(allocation$Memory_GB)
        }
    )
})

# Tests for init_multisession()
# Test for SLURM environment
test_that("init_multisession sets up future plan correctly in SLURM environment", {
    mock_sys_getenv <- function(...) {
        args <- list(...)
        var <- args[[1]]
        switch(var,
            "SLURM_JOB_ID" = "123456",
            "SLURM_CPUS_ON_NODE" = "4",
            "SLURM_CPUS_PER_TASK" = "",
            "SLURM_MEM_PER_NODE" = "8192", # 8GB in MB
            ""
        )
    }

    withr::with_options(
        new = list(future.globals.maxSize = NULL),
        with_mocked_bindings(
            Sys.getenv = mock_sys_getenv,
            code = {
                init_multisession()
                expect_true(inherits(future::plan(), "multisession"))
                # Test memory calculation:
                # Memory_GB = 8192 MB / 1024 = 8 GB
                # worker_memory = 8 GB * 1024^3 / 4 CPUs
                expected_memory <- (8192 / 1024) * 1024^3 / 4 # Convert MB to GB, then to bytes, divide by CPUs
                expect_equal(
                    getOption("future.globals.maxSize"),
                    expected_memory
                )
            }
        )
    )
})

# Test for non-SLURM environment
test_that("init_multisession works correctly outside SLURM environment", {
    mock_sys_getenv <- function(...) "" # Always return empty string
    mock_system <- function(...) "16" # 16GB system memory

    withr::with_options(
        new = list(future.globals.maxSize = NULL),
        with_mocked_bindings(
            `Sys.getenv` = mock_sys_getenv,
            system = mock_system,
            code = {
                init_multisession()
                expect_true(inherits(future::plan(), "multisession"))
                # In non-SLURM case, future.globals.maxSize isn't set
                expect_null(getOption("future.globals.maxSize"))
            }
        )
    )
})