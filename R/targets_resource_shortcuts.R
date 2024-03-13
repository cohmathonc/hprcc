#' SLURM Resource Configurations
#'
#' Defines SLURM resources for a variety of computational scales, from testing and small-scale computations
#' to very large tasks with high computational demands. Utilizes `targets::tar_resources`
#' with specific controller settings suitable for the respective task sizes.
#'
#' The available configurations are:
#'
#' | Job Type       | CPUs | Memory (GB) | Time (minutes) |
#' |----------------|------|-------------|----------------|
#' | tiny           | 2    | 8           | 60             |
#' | small          | 2    | 20          | 360            |
#' | medium         | 4    | 40          | 360            |
#' | large          | 8    | 80          | 360            |
#' | large_mem      | 8    | 600         | 360            |
#' | xlarge         | 20   | 200         | 360            |
#' | huge           | 40   | 200         | 120            |
#' | gpu_medium\eqn{^†}  | 4    | 60          | 120            |
#' | gpu_large\eqn{^†}  | 8    | 120         | 240            |
#' 
#' \eqn{^†} GPUs only available on _Gemini_
#' 
#'
#' @name SLURM-Resource-Configurations
#' @aliases tiny small medium large large_mem xlarge huge gpu_medium gpu_large
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
