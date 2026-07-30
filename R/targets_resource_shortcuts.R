#' SLURM Resource Configurations
#'
#' Defines SLURM resources for a variety of computational scales, from testing and small-scale computations
#' to very large tasks with high computational demands. Utilizes `targets::tar_resources`
#' with specific controller settings suitable for the respective task sizes.
#'
#' The available configurations are:
#'
#' | Job Type          | CPUs | Memory (GB)          | Time (minutes)    | Partition        |
#' |-------------------|------|---------------------|------------------|-----------------|
#' | tiny              | 2    | 8                   | 60               | compute          |
#' | small             | 2    | 20                  | 360              | compute          |
#' | medium            | 4    | 40                  | 360              | compute          |
#' | large             | 8    | 80                  | 360              | compute          |
#' | large_mem         | 8    | 100                 | 480              | compute          |
#' | large_mem_2x (b)   | 8    | 250 / 200           | 720              | compute          |
#' | large_mem_3x (b)  | 8    | 550 / 450           | 720              | compute          |
#' | long              | 4    | 100                 | 1440             | compute          |
#' | large_mem_xl      | 8    | 600                 | 2160             | bigmem           |
#' | xlarge            | 20   | 200                 | 720              | compute          |
#' | huge              | 40   | 200                 | 720              | compute          |
#' | gpu_medium (a)       | 4    | 60                  | 120              | gpu-a100,gpu-v100|
#' | gpu_large (a)         | 8    | 120                 | 240              | gpu-a100,gpu-v100|
#' 
#' (a) GPUs only available on _Gemini_
#'
#' (b) Memory differs per cluster, to stay inside each one's node sizes:
#' `large_mem_2x` is 250 GB on _Gemini_ / 200 GB on _Apollo_ (apollo has 20 nodes
#' at 239 GB that a 250 GB request would exclude); `large_mem_3x` is 550 GB /
#' 450 GB (apollo has only 10 nodes at >=488 GB).
#'
#' Use `large_mem_2x` up to ~250 GB, `large_mem_3x` up to ~550 GB, and `long` for targets that run
#' more than 8 hours without needing much memory. Reserve `large_mem_xl` for work
#' genuinely above 550 GB - it is the only tier that leaves the general partition
#' (`bigmem` has 5 nodes against `compute`'s 85), so jobs there can queue for
#' days.
#' 
#'
#' @name SLURM-Resource-Configurations
#' @aliases tiny small medium large large_mem large_mem_2x large_mem_3x long large_mem_xl xlarge huge gpu_medium gpu_large
#' @docType data
#'
#' @seealso \code{\link[targets]{tar_resources}}, \code{\link[targets]{tar_resources_crew}}
NULL

# Define each configuration
#' @export
tiny <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "tiny")
)

#' @export
small <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "small")
)

#' @export
medium <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "medium")
)

#' @export
large <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "large")
)

#' @export
large_mem <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "large_mem")
)

#' @export
large_mem_2x <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "large_mem_2x")
)

#' @export
large_mem_3x <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "large_mem_3x")
)

#' @export
long <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "long")
)

#' @export
large_mem_xl <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "large_mem_xl")
)

#' @export
xlarge <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "xlarge")
)

#' @export
huge <- targets::tar_resources(
    crew = targets::tar_resources_crew(controller = "huge")
)

#' @export
gpu_medium <- targets::tar_resources(
  crew = targets::tar_resources_crew(controller = "gpu_medium")
)

#' @export
gpu_large <- targets::tar_resources(
  crew = targets::tar_resources_crew(controller = "gpu_large")
)
