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
#' | large_mem_2x[2]   | 8    | 250 / 200           | 720              | compute          |
#' | long              | 4    | 100                 | 1440             | compute          |
#' | large_mem_xl      | 8    | 600                 | 2160             | bigmem           |
#' | xlarge            | 20   | 200                 | 720              | compute          |
#' | huge              | 40   | 200                 | 720              | compute          |
#' | gpu_medium[1]     | 4    | 60                  | 120              | gpu-a100,gpu-v100|
#' | gpu_large[1]      | 8    | 120                 | 240              | gpu-a100,gpu-v100|
#' 
#' [1] GPUs only available on _Gemini_
#'
#' [2] `large_mem_2x` is 250 GB on _Gemini_ and 200 GB on _Apollo_: apollo's
#' `all` partition has 20 nodes at 239 GB, which a 250 GB request would exclude,
#' leaving only 10 candidates. 200 GB keeps all 30.
#'
#' Use `large_mem_2x` for memory up to ~250 GB and `long` for targets that run
#' more than 8 hours without needing much memory. Reserve `large_mem_xl` for work
#' genuinely above 250 GB - it is the only tier that leaves the general partition
#' (`bigmem` has 5 nodes against `compute`'s 85), so jobs there can queue for
#' days.
#' 
#'
#' @name SLURM-Resource-Configurations
#' @aliases tiny small medium large large_mem large_mem_2x long large_mem_xl xlarge huge gpu_medium gpu_large
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
