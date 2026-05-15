test_that("collect_job_ids handles NULL", {
    expect_equal(collect_job_ids(NULL), character(0))
})

test_that("collect_job_ids extracts from single result", {
    result <- slurm_job_result(
        path = "/tmp/test.sh",
        job_id = "12345",
        status = "submitted"
    )
    expect_equal(collect_job_ids(result), "12345")
})

test_that("collect_job_ids extracts from list of results", {
    results <- list(
        slurm_job_result(path = "/tmp/a.sh", job_id = "111", status = "submitted"),
        slurm_job_result(path = "/tmp/b.sh", job_id = "222", status = "submitted"),
        slurm_job_result(path = "/tmp/c.csv", job_id = NULL, status = "complete")
    )
    ids <- collect_job_ids(results)
    expect_equal(ids, c("111", "222"))
})

test_that("collect_job_ids returns empty for complete results", {
    result <- slurm_job_result(path = "/tmp/output.csv")
    expect_equal(collect_job_ids(result), character(0))
})

test_that("collect_job_ids handles plain string", {
    expect_equal(collect_job_ids("not_a_result"), character(0))
})

test_that("get_result_path returns path for complete result", {
    result <- slurm_job_result(path = "/tmp/output.csv", status = "complete")
    expect_equal(get_result_path(result, wait = FALSE), "/tmp/output.csv")
    expect_equal(get_result_path(result, wait = TRUE), "/tmp/output.csv")
})

test_that("get_result_path returns path for result without job_id", {
    result <- slurm_job_result(path = "/tmp/test.sh", status = "submitted")
    # job_id is NULL by default in this construction... but let's be explicit
    result_no_id <- slurm_job_result(path = "/tmp/done.csv", job_id = NULL, status = "complete")
    expect_equal(get_result_path(result_no_id), "/tmp/done.csv")
})

test_that("get_result_path passes through plain strings", {
    expect_equal(get_result_path("/tmp/file.csv"), "/tmp/file.csv")
    expect_equal(get_result_path("/tmp/file.csv", wait = TRUE), "/tmp/file.csv")
})

test_that("get_result_path returns immediately with wait=FALSE", {
    result <- slurm_job_result(
        path = "/tmp/test.sh",
        job_id = "99999",
        status = "submitted"
    )
    expect_equal(get_result_path(result, wait = FALSE), "/tmp/test.sh")
})

test_that("get_slurm_working_dir extracts from result", {
    result <- slurm_job_result(
        path = "/tmp/test.sh",
        working_dir = "/scratch/user/analysis"
    )
    expect_equal(get_slurm_working_dir(result), "/scratch/user/analysis")
})

test_that("get_slurm_working_dir infers from script path", {
    expect_equal(get_slurm_working_dir("/scratch/user/job.sh"), "/scratch/user")
})

test_that("get_slurm_working_dir walks up completion_depth levels", {
    path <- "/scratch/user/sample1/outs/filtered.h5"
    expect_equal(
        get_slurm_working_dir(path, completion_depth = 1L),
        "/scratch/user/sample1"
    )
})

test_that("get_slurm_working_dir handles NULL", {
    expect_null(get_slurm_working_dir(NULL))
})
