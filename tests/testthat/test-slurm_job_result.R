test_that("slurm_job_result constructor works", {
    result <- slurm_job_result(
        path = "/tmp/test.sh",
        job_id = "12345",
        status = "submitted",
        working_dir = "/tmp",
        completion_files = c("/tmp/output.h5")
    )

    expect_s3_class(result, "slurm_job_result")
    expect_equal(result$path, "/tmp/test.sh")
    expect_equal(result$job_id, "12345")
    expect_equal(result$status, "submitted")
    expect_equal(result$working_dir, "/tmp")
    expect_equal(result$completion_files, "/tmp/output.h5")
})

test_that("slurm_job_result defaults work", {
    result <- slurm_job_result(path = "/scratch/user/output.csv")

    expect_equal(result$status, "complete")
    expect_null(result$job_id)
    expect_equal(result$working_dir, "/scratch/user")
    expect_null(result$completion_files)
})

test_that("slurm_job_result validates status", {
    expect_error(
        slurm_job_result(path = "/tmp/test.sh", status = "invalid"),
        "status"
    )
})

test_that("as.character returns path", {
    result <- slurm_job_result(path = "/tmp/output.csv")
    expect_equal(as.character(result), "/tmp/output.csv")
})

test_that("is.slurm_job_result works", {
    result <- slurm_job_result(path = "/tmp/test.sh")
    expect_true(is.slurm_job_result(result))
    expect_false(is.slurm_job_result(list(path = "/tmp/test.sh")))
    expect_false(is.slurm_job_result("string"))
})

test_that("print method runs without error", {
    result <- slurm_job_result(
        path = "/tmp/test.sh",
        job_id = "12345",
        status = "submitted"
    )
    expect_no_error(capture.output(print(result)))
})
