# Debug prints
cat("Starting test setup...\n")

# Basic setup
library(testthat)
library(mockery)

cat("Libraries loaded...\n")

# Prevent SLURM configuration during tests
Sys.setenv(
    R_TESTS = "",
    TESTTHAT = "true",
    SLURM_JOB_ID = ""
)

cat("Environment set...\n")

# Basic package options
options(
    hprcc.slurm_logs = FALSE,
    hprcc.slurm_verbose = FALSE
)

cat("Running tests...\n")
test_check("hprcc", reporter = "progress")
cat("Tests complete\n")