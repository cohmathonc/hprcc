#-----------------------------------------------------------------------------
## Resources shortcuts for {targets}

#' Tiny SLURM Resource Configuration
#'
#' Defines SLURM resources for testing or very small-scale computations. Utilizes `targets::tar_resources`
#' with a "tiny" controller setting, suitable for less resource-intensive tasks.
#'
#' | CPUs | Memory (GB) | Time (minutes) |
#' |------|-------------|----------------|
#' | 1    | 1           | 60             |
#'
#' @export
#' @seealso \code{\link[targets]{tar_resources}}, \code{\link[targets]{tar_resources_crew}}
tiny <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "tiny")
)

#' Small SLURM Resource Configuration
#'
#' Defines SLURM resources for small-scale computations. Utilizes `targets::tar_resources`
#' with a "small" controller setting, suitable for less resource-intensive tasks.
#'
#' | CPUs | Memory (GB) | Time (minutes) |
#' |------|-------------|----------------|
#' | 2    | 20          | 360            |
#'
#' @export
#' @seealso \code{\link[targets]{tar_resources}}, \code{\link[targets]{tar_resources_crew}}
small <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "small")
)

#' Medium SLURM Resource Configuration
#'
#' Configures medium-level SLURM resources. This object is constructed using
#' `targets::tar_resources` with a "medium" controller setting, suitable for
#' moderately demanding computational tasks.
#'
#' | CPUs | Memory (GB) | Time (minutes) |
#' |------|-------------|----------------|
#' | 12   | 80          | 360            |
#'
#' @export
#' @seealso \code{\link[targets]{tar_resources}}, \code{\link[targets]{tar_resources_crew}}
medium <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "medium")
)

#' Large SLURM Resource Configuration
#'
#' Configures SLURM resources for large-scale tasks using `targets::tar_resources`
#' with a "large" controller setting. Ideal for high-demand computations.
#'
#' | CPUs | Memory (GB) | Time (minutes) |
#' |------|-------------|----------------|
#' | 20   | 200         | 720            |
#'
#' @export
#' @seealso \code{\link[targets]{tar_resources}}, \code{\link[targets]{tar_resources_crew}}
large <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "large")
)

#' Huge SLURM Resource Configuration
#'
#' Sets up SLURM resources for very large tasks using `targets::tar_resources`
#' with a "huge" controller. Designed for very high computational loads.
#'
#' | CPUs | Memory (GB) | Time (minutes) |
#' |------|-------------|----------------|
#' | 40   | 100         | 120            |
#' @export
#' @seealso \code{\link[targets]{tar_resources}}, \code{\link[targets]{tar_resources_crew}}
huge <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "huge")
)

#' Bigmem SLURM Resource Configuration
#'
#' Establishes SLURM resources for tasks requiring significant memory via
#' `targets::tar_resources` with a "bigmem" controller. Suitable for memory-intensive jobs.
#'
#' | CPUs | Memory (GB) | Time (minutes) |
#' |------|-------------|----------------|
#' | 10   | 500         | 360            |
#'
#' @export
#' @seealso \code{\link[targets]{tar_resources}}, \code{\link[targets]{tar_resources_crew}}
bigmem <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "bigmem")
)
#' GPU Medium SLURM Resource Configuration
#'
#' Establishes SLURM resources for tasks requiring GPU capabilities via
#' `targets::tar_resources` with a "gpu_medium" controller. Suitable for medium-sized GPU-intensive jobs.
#'
#' | CPUs | GPUs | Memory (GB) | Time (minutes) | Partition |
#' |------|------|-------------|----------------|-----------|
#' | 4    | 1    | 60          | 120            | gpu       |
#'
#' @export
#' @seealso \code{\link[targets]{tar_resources}}, \code{\link[targets]{tar_resources_crew}}
gpu_medium <- targets::tar_resources(
  crew = targets::tar_resources_crew(controller = "gpu_medium")
)

#' GPU Large SLURM Resource Configuration
#'
#' Establishes SLURM resources for tasks requiring GPU capabilities via
#' `targets::tar_resources` with a "gpu_large" controller. Suitable for large GPU-intensive jobs.
#'
#' | CPUs | GPUs | Memory (GB) | Time (minutes) | Partition |
#' |------|------|-------------|----------------|-----------|
#' | 8    | 1    | 120         | 240            | gpu       |
#'
#' @export
#' @seealso \code{\link[targets]{tar_resources}}, \code{\link[targets]{tar_resources_crew}}
gpu_large <- targets::tar_resources(
  crew = targets::tar_resources_crew(controller = "gpu_large")
)
