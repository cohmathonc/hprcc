# GPU Partition Selection
# Dynamic selection between gpu-a100 and gpu-v100 based on queue state

#' Choose Best GPU Partition
#'
#' Dynamically selects between `gpu-a100` and `gpu-v100` partitions based on
#' current queue backlog and node availability. A100 GPUs are ~2.5x faster
#' than V100 for ML workloads (e.g., CellBender), so A100 is preferred unless
#' the V100 queue is significantly shorter.
#'
#' @return Character string: `"gpu-a100"` or `"gpu-v100"`.
#'
#' @details
#' The selection algorithm:
#' 1. Query `sinfo` for available/mixed nodes per partition
#' 2. Query `squeue` for pending jobs per partition
#' 3. Compute queue density (pending jobs / available nodes)
#' 4. Prefer A100 if its effective wait (density / 2.5) <= V100 density * 1.2
#'
#' Falls back to `"gpu-a100"` if partition info cannot be queried.
#'
#' @note Only available on Gemini cluster. Apollo does not have GPUs.
#'
#' @export
#' @importFrom cli cli_alert_info
choose_gpu_partition <- function() {
    sinfo_a100 <- tryCatch(
        system2("sinfo", c("-p", "gpu-a100", "-h", "-o", "%D %T"), stdout = TRUE),
        error = function(e) character(0)
    )
    sinfo_v100 <- tryCatch(
        system2("sinfo", c("-p", "gpu-v100", "-h", "-o", "%D %T"), stdout = TRUE),
        error = function(e) character(0)
    )

    if (length(sinfo_a100) == 0 || length(sinfo_v100) == 0) {
        return("gpu-a100")
    }

    squeue_out <- tryCatch(
        system2("squeue", c("-h", "-t", "PD", "-o", "%P"), stdout = TRUE),
        error = function(e) character(0)
    )

    pending_a100 <- sum(grepl("^gpu-a100", squeue_out))
    pending_v100 <- sum(grepl("^gpu-v100", squeue_out))

    nodes_a100 <- sum(as.integer(
        gsub("\\s+.*", "", sinfo_a100[grepl("idle|mix", sinfo_a100)])
    ))
    nodes_v100 <- sum(as.integer(
        gsub("\\s+.*", "", sinfo_v100[grepl("idle|mix", sinfo_v100)])
    ))

    if (nodes_a100 == 0) nodes_a100 <- 1
    if (nodes_v100 == 0) nodes_v100 <- 1

    density_a100 <- pending_a100 / nodes_a100
    density_v100 <- pending_v100 / nodes_v100

    if (density_a100 / 2.5 <= density_v100 * 1.2) {
        return("gpu-a100")
    }

    "gpu-v100"
}
