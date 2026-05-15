test_that("check_slurm_status handles NULL/NA/empty input", {
    expect_equal(check_slurm_status(NULL), "unknown")
    expect_equal(check_slurm_status(NA), "unknown")
    expect_equal(check_slurm_status(""), "unknown")
})

test_that("verify_completion_files passes for non-slurm_job_result", {
    expect_invisible(verify_completion_files("plain_string"))
    expect_invisible(verify_completion_files(list(a = 1)))
})

test_that("verify_completion_files passes when no completion_files", {
    result <- slurm_job_result(
        path = "/tmp/test.sh",
        job_id = "12345",
        status = "submitted",
        completion_files = NULL
    )
    expect_invisible(verify_completion_files(result))
})

test_that("verify_completion_files passes when no job_id", {
    result <- slurm_job_result(
        path = "/tmp/output.csv",
        job_id = NULL,
        status = "complete",
        completion_files = "/tmp/output.csv"
    )
    expect_invisible(verify_completion_files(result))
})

test_that("verify_completion_files handles list of results", {
    results <- list(
        slurm_job_result(path = "/tmp/a.csv", completion_files = NULL),
        slurm_job_result(path = "/tmp/b.csv", completion_files = NULL)
    )
    expect_invisible(verify_completion_files(results))
})
