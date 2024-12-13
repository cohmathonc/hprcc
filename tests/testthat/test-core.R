library(testthat)
library(mockery)

test_that("default_partition uses option if set", {
    withr::with_options(
        new = list(hprcc.default_partition = "test_partition"),
        code = {
            expect_equal(default_partition(), "test_partition")
        }
    )
})

test_that("default_partition handles missing SLURM gracefully", {
    withr::with_options(
        new = list(hprcc.default_partition = NULL),
        code = {
            # Mock slurm_default_partition to return NULL
            mockery::stub(default_partition, "slurm_default_partition", NULL)

            expect_warning(
                partition <- default_partition(),
                "Could not determine default partition"
            )
            expect_null(partition)
        }
    )
})