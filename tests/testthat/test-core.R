library(testthat)
library(hprcc)

# Tests for log_hprcc_settings()
test_that("log_hprcc_settings returns NULL when logging disabled", {
    withr::with_options(
        list(hprcc.slurm_logs = FALSE),
        {
            result <- log_hprcc_settings()
            expect_null(result)
        }
    )
})

# Tests for default_partition()
test_that("default_partition uses option when set", {
    withr::with_options(
        list(hprcc.default_partition = "test_partition"),
        {
            expect_equal(default_partition(), "test_partition")
        }
    )
})

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
    withr::with_envvar(
        c(
            SLURM_JOB_ID = "123456",
            SLURM_CPUS_ON_NODE = "4",
            SLURM_CPUS_PER_TASK = "",
            SLURM_MEM_PER_NODE = "8192"
        ),
        withr::with_options(
            new = list(
                future.globals.maxSize = NULL,
                parallelly.availableCores.methods = "Slurm"
            ),
            {
                init_multisession()
                expect_true(inherits(future::plan(), "multisession"))
                # Memory_GB = 8192 MB / 1024 = 8 GB
                # worker_memory = 8 GB * 1024^3 / 4 CPUs
                expected_memory <- (8192 / 1024) * 1024^3 / 4
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
    withr::with_envvar(
        c(SLURM_JOB_ID = ""),
        withr::with_options(
            new = list(future.globals.maxSize = NULL),
            {
                init_multisession()
                expect_true(inherits(future::plan(), "multisession"))
                # In non-SLURM case, future.globals.maxSize isn't set
                expect_null(getOption("future.globals.maxSize"))
            }
        )
    )
})
# ---------------------------------------------------------------------------
# work_dir() / nf_workdir()
#
# Cluster defaults are exercised by mocking get_cluster(), so these run
# anywhere - no HPRCC host required.
# ---------------------------------------------------------------------------

test_that("work_dir uses the option ahead of everything else", {
    withr::with_envvar(c(HPRCC_WORK_DIR = "/env/path"), {
        withr::with_options(list(hprcc.work_dir = "/opt/path"), {
            expect_equal(work_dir(), "/opt/path")
        })
    })
})

test_that("work_dir falls back to the env var when no option is set", {
    withr::with_options(list(hprcc.work_dir = NULL), {
        withr::with_envvar(c(HPRCC_WORK_DIR = "/env/path"), {
            expect_equal(work_dir(), "/env/path")
        })
    })
})

test_that("work_dir appends path components", {
    withr::with_options(list(hprcc.work_dir = "/base"), {
        expect_equal(work_dir("proj", "_targets"), "/base/proj/_targets")
    })
})

test_that("work_dir returns gemini's per-user scratch by default", {
    withr::with_options(list(hprcc.work_dir = NULL), {
        withr::with_envvar(c(HPRCC_WORK_DIR = NA), {
            testthat::local_mocked_bindings(get_cluster = function() "gemini")
            expect_equal(work_dir(), file.path("/scratch", Sys.info()[["user"]]))
        })
    })
})

test_that("work_dir returns apollo's shared lab dir, with no user component", {
    withr::with_options(list(hprcc.work_dir = NULL), {
        withr::with_envvar(c(HPRCC_WORK_DIR = NA), {
            testthat::local_mocked_bindings(get_cluster = function() "apollo")
            # Deliberately NOT per-user - apollo's root is shared, which is why
            # this helper is not called scratch_dir().
            expect_equal(work_dir(), "/labs/rrockne/MHO")
            expect_false(grepl(Sys.info()[["user"]], work_dir(), fixed = TRUE))
        })
    })
})

test_that("work_dir aborts on an unrecognised cluster rather than guessing", {
    withr::with_options(list(hprcc.work_dir = NULL), {
        withr::with_envvar(c(HPRCC_WORK_DIR = NA), {
            testthat::local_mocked_bindings(get_cluster = function() NULL)
            expect_error(work_dir(), "Cannot determine a working directory")
        })
    })
})

test_that("work_dir rejects overrides that break the absolute-path contract", {
    withr::with_options(list(hprcc.work_dir = "relative/path"), {
        expect_error(work_dir(), "must be an absolute path")
    })
    withr::with_options(list(hprcc.work_dir = ""), {
        expect_error(work_dir(), "non-empty")
    })
    withr::with_options(list(hprcc.work_dir = c("/a", "/b")), {
        expect_error(work_dir(), "single non-empty")
    })
    withr::with_options(list(hprcc.work_dir = NA_character_), {
        expect_error(work_dir(), "single non-empty")
    })
})

test_that("work_dir expands ~ in an override", {
    withr::with_options(list(hprcc.work_dir = "~/somewhere"), {
        expect_equal(work_dir(), path.expand("~/somewhere"))
    })
})

test_that("nf_workdir is a sibling of the run dir, not a child", {
    withr::with_options(list(hprcc.work_dir = "/scratch/someone"), {
        expect_equal(nf_workdir(), "/scratch/someone/nf-workdir")
        # The sibling arrangement is the point: it must not land inside an
        # nf-core run directory.
        expect_false(grepl("nf-core", nf_workdir(), fixed = TRUE))
    })
})
