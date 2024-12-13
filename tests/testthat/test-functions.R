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
            expect_warning(get_cluster(), "Unknown cluster")
        }
    )
})

test_that("slurm_allocation retrieves correct SLURM job resources", {
    mock_sys_getenv <- function(varname, ...) "123456"
    mock_system2 <- function(command, args, stdout) {
        return("6|billing=6,cpu=6,mem=60G,node=1")
    }

    with_mocked_bindings(
        Sys.getenv = mock_sys_getenv,
        system2 = mock_system2,
        code = {
            allocation <- slurm_allocation()
            expect_equal(allocation$job_id, "123456")
            expect_equal(allocation$CPUs, 6)
            expect_equal(allocation$Memory_GB, 60)
        }
    )
})

test_that("init_multisession sets up future plan based on resources", {
    # Mock SLURM environment
    mock_sys_getenv_slurm <- function(varname, ...) "123456"
    mock_slurm_allocation <- function() {
        list(job_id = "123456", CPUs = 4, Memory_GB = 8)
    }
    mock_system <- function(command, intern = TRUE) "8" # System memory in GB

    with_mocked_bindings(
        Sys.getenv = mock_sys_getenv_slurm,
        slurm_allocation = mock_slurm_allocation,
        system = mock_system,
        code = {
            # Run in a SLURM environment
            init_multisession()
            expect_true(inherits(future::plan(), "multisession"))
            expect_equal(getOption("future.globals.maxSize"), 8 * 1024^3/4)

            # Run outside of a SLURM environment
            Sys.getenv <- function(varname, ...) ""
            init_multisession()
            # Similar assertions for system resources
            # TODO: Add assertions for system resources
        }
    )
})

