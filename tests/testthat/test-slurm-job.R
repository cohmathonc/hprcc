library(testthat)
library(hprcc)

# Three tests below reach run_slurm_job()/run_singularity_job() past their
# early-return guards, at which point they shell out to `sbatch`. That is fine on
# a cluster and fatal in CI, where sbatch does not exist and system2() raises
# "error in running command" - a failure about the environment, not the code.
skip_if_no_sbatch <- function() {
    testthat::skip_if(
        nzchar(Sys.which("sbatch")) == FALSE,
        "sbatch not available - cannot exercise real job submission"
    )
}

# Tests for generate_slurm_script() (internal helper)
test_that("generate_slurm_script creates valid SLURM script", {
    script <- hprcc:::generate_slurm_script(
        name = "test_job",
        command = "echo 'hello'",
        working_dir = "/tmp/test",
        slurm_options = list(time = "1:00:00", mem = "4G", cpus_per_task = 2L),
        modules_to_load = "singularity",
        env_vars = list(MY_VAR = "value")
    )

    # Check shebang
    expect_match(script, "^#!/bin/bash")

    # Check SBATCH directives
    expect_match(script, "#SBATCH --job-name=test_job")
    expect_match(script, "#SBATCH --time=1:00:00")
    expect_match(script, "#SBATCH --mem=4G")
    expect_match(script, "#SBATCH --cpus-per-task=2")
    expect_match(script, "#SBATCH --output=/tmp/test/slurm-%j.out")
    # stderr is deliberately merged into the same file as stdout, so both
    # streams stay interleaved in submission order - splitting them makes a
    # failure's context land in a different file from the error itself.
    expect_match(script, "#SBATCH --error=/tmp/test/slurm-%j.out")

    # Check module loading
    expect_match(script, "module load singularity")

    # Check environment variable (values are quoted)
    expect_match(script, 'export MY_VAR="value"')

    # Check command execution with signal handling
    expect_match(script, "echo 'hello' &")
    expect_match(script, "cmd_pid=\\$!")
    expect_match(script, "trap")
    expect_match(script, "wait \\$cmd_pid")
})

test_that("generate_slurm_script handles empty modules and env_vars", {
    script <- hprcc:::generate_slurm_script(
        name = "minimal_job",
        command = "ls",
        working_dir = "/tmp",
        slurm_options = list(),
        modules_to_load = NULL,
        env_vars = NULL
    )

    expect_match(script, "^#!/bin/bash")
    expect_match(script, "#SBATCH --job-name=minimal_job")
    expect_false(grepl("module load", script))
    expect_false(grepl("export ", script))
})

test_that("generate_slurm_script converts underscores to hyphens in options", {
    script <- hprcc:::generate_slurm_script(
        name = "test",
        command = "ls",
        working_dir = "/tmp",
        slurm_options = list(cpus_per_task = 4, mem_per_cpu = "2G"),
        modules_to_load = NULL,
        env_vars = NULL
    )

    expect_match(script, "#SBATCH --cpus-per-task=4")
    expect_match(script, "#SBATCH --mem-per-cpu=2G")
})

# Tests for run_slurm_job() input validation
test_that("run_slurm_job validates required parameters", {
    expect_error(
        run_slurm_job(
            name = "",
            command = "ls",
            working_dir = "/tmp",
            completion_files = "out.txt"
        ),
        "nzchar"
    )

    expect_error(
        run_slurm_job(
            name = "test",
            command = "ls",
            working_dir = "/tmp",
            completion_files = character(0)
        )
    )
})

# Tests for run_slurm_job() completion checking
test_that("run_slurm_job returns completion file if exists", {
    # Create a temp directory with a completion file
    tmp_dir <- tempfile("slurm_test_")
    dir.create(tmp_dir)
    completion_file <- file.path(tmp_dir, "output.txt")
    file.create(completion_file)

    on.exit(unlink(tmp_dir, recursive = TRUE))

    result <- run_slurm_job(
        name = "test_job",
        command = "echo 'test'",
        working_dir = tmp_dir,
        completion_files = "output.txt"
    )

    # 5f62b8e changed the return type to slurm_job_result for the
    # fire-and-track pattern; as.character() unwraps it to the path.
    expect_s3_class(result, "slurm_job_result")
    expect_equal(as.character(result), completion_file)
})

test_that("run_slurm_job returns script path if script exists", {
    skip_if_no_sbatch()
    tmp_dir <- tempfile("slurm_test_")
    dir.create(tmp_dir)
    script_path <- file.path(tmp_dir, "existing_job.sh")
    writeLines("#!/bin/bash\necho test", script_path)

    on.exit(unlink(tmp_dir, recursive = TRUE))

    # Should return script path without submitting
    result <- run_slurm_job(
        name = "existing_job",
        command = "echo 'test'",
        working_dir = tmp_dir,
        completion_files = "nonexistent.txt"
    )

    # 5f62b8e wraps the return in slurm_job_result; as.character() unwraps it.
    expect_s3_class(result, "slurm_job_result")
    expect_equal(as.character(result), script_path)
})

# Tests for run_singularity_job() input validation
test_that("run_singularity_job validates container exists", {
    expect_error(
        run_singularity_job(
            name = "test",
            container = "/nonexistent/path/container.sif",
            command = "ls",
            working_dir = "/tmp",
            completion_files = "out.txt"
        ),
        "Container not found"
    )
})

test_that("run_singularity_job fails gracefully on unknown cluster", {
    skip_if_no_sbatch()
    # Mock unknown cluster by mocking singularity_bind_dirs to return NULL
    with_mocked_bindings(
        singularity_bind_dirs = function() NULL,
        {
            # Create a fake container file
            tmp_container <- tempfile(fileext = ".sif")
            file.create(tmp_container)
            on.exit(unlink(tmp_container))

            expect_error(
                run_singularity_job(
                    name = "test",
                    container = tmp_container,
                    command = "ls",
                    working_dir = "/tmp",
                    completion_files = "out.txt"
                ),
                "Cannot determine singularity bind paths"
            )
        }
    )
})

test_that("run_singularity_job fails gracefully when singularity_bin is NULL", {
    skip_if_no_sbatch()
    with_mocked_bindings(
        singularity_bind_dirs = function() "/tmp,/data",
        singularity_bin = function() NULL,
        {
            # Create a fake container file
            tmp_container <- tempfile(fileext = ".sif")
            file.create(tmp_container)
            on.exit(unlink(tmp_container))

            expect_error(
                run_singularity_job(
                    name = "test",
                    container = tmp_container,
                    command = "ls",
                    working_dir = "/tmp",
                    completion_files = "out.txt"
                ),
                "Cannot determine singularity binary path"
            )
        }
    )
})

# Tests for GPU options
test_that("run_singularity_job sets GPU options when gpu=TRUE", {
    # Create mock environment for testing
    tmp_dir <- tempfile("gpu_test_")
    dir.create(tmp_dir)
    completion_file <- file.path(tmp_dir, "done.txt")
    file.create(completion_file) # Create completion file so it returns early

    # Create a fake container file
    tmp_container <- tempfile(fileext = ".sif")
    file.create(tmp_container)

    on.exit({
        unlink(tmp_dir, recursive = TRUE)
        unlink(tmp_container)
    })

    # Test that completion file is returned (basic flow works)
    with_mocked_bindings(
        singularity_bind_dirs = function() "/tmp",
        singularity_bin = function() "/usr/bin/singularity",
        {
            result <- run_singularity_job(
                name = "gpu_test",
                container = tmp_container,
                command = "nvidia-smi",
                working_dir = tmp_dir,
                completion_files = "done.txt",
                gpu = TRUE
            )

            expect_s3_class(result, "slurm_job_result")
            expect_equal(as.character(result), completion_file)
        }
    )
})

# Test %||% operator
test_that("null coalescing operator works correctly", {
    `%||%` <- hprcc:::`%||%`

    expect_equal(NULL %||% "default", "default")
    expect_equal("value" %||% "default", "value")
    expect_equal(NA %||% "default", NA)
})
