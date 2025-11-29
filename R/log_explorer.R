#' @import dplyr
#' @import ggplot2
#' @import shiny
#' @importFrom stats density
NULL

# Default controller for hprcc crew workers when no resources= specified
DEFAULT_CONTROLLER <- "small"

#' Remove hash suffix from phase name
#' 
#' @param phase Character vector of phase names with IDs
#' @return Character vector of cleaned phase names
#' @keywords internal
clean_phase_name <- function(phase) {
    # Remove hash suffix to group tasks by phase
    sub("_[0-9a-f]{16}$", "", phase)
}

autometric_hprcc_controllers <- function() {
    # Define the hard-coded controller parameters
    controllers <- list(
        list(
            name = "tiny", 
            cpus = 2L, 
            memory_gigabytes = 8L, 
            walltime_minutes = 60L
        ),
        list(
            name = "small", 
            cpus = 2L, 
            memory_gigabytes = 20L, 
            walltime_minutes = 360L
        ),
        list(
            name = "medium", 
            cpus = 4L, 
            memory_gigabytes = 40L, 
            walltime_minutes = 360L
        ),
        list(
            name = "large", 
            cpus = 8L, 
            memory_gigabytes = 80L, 
            walltime_minutes = 360L
        ),
        list(
            name = "large_mem", 
            cpus = 8L, 
            memory_gigabytes = 800L, 
            walltime_minutes = 360L
        ),
        list(
            name = "xlarge", 
            cpus = 20L, 
            memory_gigabytes = 200L, 
            walltime_minutes = 360L
        ),
        list(
            name = "huge", 
            cpus = 40L, 
            memory_gigabytes = 200L, 
            walltime_minutes = 120L
        )
    )
    
    return(controllers)
}

#' Find _targets.R file from logs path
#'
#' @param logs_path Path to the logs directory
#' @return Path to _targets.R file or NULL if not found
#' @keywords internal
find_targets_file <- function(logs_path) {
    # If logs_path is NULL, use current working directory
    if (is.null(logs_path)) {
        base_path <- getwd()
    } else {
        # logs are typically in _targets/logs, so go up two levels
        base_path <- normalizePath(file.path(logs_path, "..", ".."), mustWork = FALSE)
    }

    # Default: _targets.R in project root
    default_path <- file.path(base_path, "_targets.R")
    if (file.exists(default_path)) {
        return(default_path)
    }

    return(NULL)
}

#' Parse target-to-controller mapping from _targets.R
#'
#' @param targets_file Path to _targets.R file
#' @return Named character vector mapping target names to controller names
#' @keywords internal
parse_target_resources <- function(targets_file) {
    if (is.null(targets_file) || !file.exists(targets_file)) {
        return(character(0))
    }

    # Read file content
    content <- tryCatch(
        paste(readLines(targets_file, warn = FALSE), collapse = "\n"),
        error = function(e) ""
    )

    if (nchar(content) == 0) {
        return(character(0))
    }

    # Match tar_target(name, ..., resources = controller)
    # This regex captures target name and resource name
    # Uses [\s\S]*? to match across newlines (non-greedy)
    pattern <- "tar_target\\s*\\(\\s*(\\w+)[\\s\\S]*?resources\\s*=\\s*(\\w+)"

    matches <- gregexpr(pattern, content, perl = TRUE)

    if (matches[[1]][1] == -1) {
        return(character(0))
    }

    # Extract all matches
    match_strings <- regmatches(content, matches)[[1]]

    # Parse each match to get target name and controller
    result <- character(0)
    for (match_str in match_strings) {
        # Extract target name (first capture group)
        target_match <- regmatches(
            match_str,
            regexec("tar_target\\s*\\(\\s*(\\w+)", match_str)
        )[[1]]

        # Extract resource/controller name (after resources =)
        resource_match <- regmatches(
            match_str,
            regexec("resources\\s*=\\s*(\\w+)", match_str)
        )[[1]]

        if (length(target_match) >= 2 && length(resource_match) >= 2) {
            target_name <- target_match[2]
            controller_name <- resource_match[2]
            result[target_name] <- controller_name
        }
    }

    return(result)
}

#' Summarize resource usage and recommendations for targets
#'
#' Analyzes autometric logs and compares actual resource usage against configured
#' controllers to generate optimization recommendations for each target.
#'
#' @param path Path to _targets/logs directory. If NULL, uses default location.
#' @param targets_file Path to _targets.R file. If NULL, auto-detects from path.
#'
#' @return A data frame with columns:
#'   - target: Target name
#'   - current_controller: Configured controller (or "unknown")
#'   - peak_memory_gb: Peak memory usage in GB
#'   - peak_cpu_pct: Peak CPU usage percentage
#'   - duration_min: Duration in minutes
#'   - recommended_controller: Optimal controller based on usage
#'   - status: "ok", "underprovisioned", "overprovisioned", or "unknown"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get recommendations for all targets
#' summary <- summarize_resource_usage()
#' print(summary)
#'
#' # Filter to targets that need adjustment
#' summary[summary$status != "ok", ]
#' }
summarize_resource_usage <- function(path = NULL, targets_file = NULL) {
    # Load log data
    logs <- read_targets_logs(path)

    # Add clean phase names
    logs <- logs |>
        dplyr::mutate(clean_phase_name = clean_phase_name(phase))

    # Find and parse _targets.R
    if (is.null(targets_file)) {
        targets_file <- find_targets_file(path)
    }
    target_resources <- parse_target_resources(targets_file)

    # Get controller specs
    controllers <- autometric_hprcc_controllers()

    # Analyze each unique phase
    phases <- unique(logs$clean_phase_name)
    phases <- phases[phases != "__DEFAULT__"]

    results <- lapply(phases, function(phase_name) {
        phase_data <- logs[logs$clean_phase_name == phase_name, ]

        # Skip if no data
        if (nrow(phase_data) == 0) {
            return(NULL)
        }

        # Get current controller from mapping
        current_controller_name <- if (!is.na(phase_name) && phase_name %in% names(target_resources)) {
            target_resources[[phase_name]]
        } else {
            "unknown"
        }

        # Find current controller specs
        current_controller <- NULL
        if (current_controller_name != "unknown") {
            for (ctrl in controllers) {
                if (ctrl$name == current_controller_name) {
                    current_controller <- ctrl
                    break
                }
            }
            if (is.null(current_controller)) {
                warning(sprintf(
                    "Unrecognized controller '%s' for target '%s'",
                    current_controller_name, phase_name
                ))
            }
        }

        # Calculate resource usage
        analysis <- phase_data |>
            dplyr::group_by(slurm_job_id) |>
            dplyr::summarise(
                peak_mem_gb = max(resident) / 1024,
                peak_cpu = max(cpu),
                duration_min = diff(range(time)) / 60,
                .groups = "drop"
            )

        peak_memory <- max(analysis$peak_mem_gb)
        peak_cpu <- max(analysis$peak_cpu)
        duration <- max(analysis$duration_min)

        # Apply safety margin (1.3x) to account for:
        # - Parallel worker overhead (BiocParallel/SnowParam spawn child processes)
        # - Memory spikes between autometric samples
        # - General headroom to avoid OOM kills
        safety_margin <- 1.3
        safe_memory <- peak_memory * safety_margin

        # Find optimal controller
        # CPU percentage represents usage across all cores (e.g., 200% = 2 cores)
        required_cpus <- max(1L, ceiling(peak_cpu / 100))
        recommended <- controllers[[1]]
        for (ctrl in controllers) {
            if (ctrl$memory_gigabytes >= safe_memory &&
                ctrl$cpus >= required_cpus &&
                ctrl$walltime_minutes >= duration) {
                recommended <- ctrl
                break
            }
        }

        # Determine status
        default_controller <- DEFAULT_CONTROLLER

        status <- if (current_controller_name == "unknown") {
            # If not in _targets.R, check if default would be appropriate
            if (recommended$name == default_controller) {
                "ok"  # Default is sufficient, no action needed
            } else {
                "unknown"  # Needs explicit resources= set
            }
        } else if (is.null(current_controller)) {
            "unknown"
        } else if (recommended$name == current_controller_name) {
            "ok"
        } else {
            # Compare controller "sizes" based on memory
            current_mem <- current_controller$memory_gigabytes
            recommended_mem <- recommended$memory_gigabytes
            if (recommended_mem < current_mem) {
                "overprovisioned"
            } else {
                "underprovisioned"
            }
        }

        data.frame(
            target = phase_name,
            current_controller = current_controller_name,
            peak_memory_gb = round(peak_memory, 2),
            peak_cpu_pct = round(peak_cpu, 1),
            duration_min = round(duration, 1),
            recommended_controller = recommended$name,
            status = status,
            stringsAsFactors = FALSE
        )
    })

    # Filter out NULL results (targets that couldn't be processed)
    results <- results[!sapply(results, is.null)]

    # Handle case where no valid results
    if (length(results) == 0) {
        message("No targets could be analyzed. Check that log files contain valid autometric data.")
        return(data.frame(
            target = character(),
            current_controller = character(),
            peak_memory_gb = numeric(),
            peak_cpu_pct = numeric(),
            duration_min = numeric(),
            recommended_controller = character(),
            status = character(),
            stringsAsFactors = FALSE
        ))
    }

    # Combine results
    result <- do.call(rbind, results)

    # Sort by status (issues first) then by target name
    status_order <- c("underprovisioned", "overprovisioned", "unknown", "ok")
    result$status_order <- match(result$status, status_order)
    result <- result[order(result$status_order, result$target), ]
    result$status_order <- NULL

    rownames(result) <- NULL
    return(result)
}

#' Print resource usage recommendations
#'
#' Prints a formatted summary of resource usage and recommendations for targets.
#' Useful for reviewing before updating _targets.R.
#'
#' @param path Path to _targets/logs directory. If NULL, uses default location.
#' @param targets_file Path to _targets.R file. If NULL, auto-detects from path.
#' @param show_ok Logical, whether to show targets with status "ok". Default FALSE.
#'
#' @return Invisibly returns the summary data frame.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Print only targets needing adjustment
#' print_resource_recommendations()
#'
#' # Print all targets including those that are ok
#' print_resource_recommendations(show_ok = TRUE)
#' }
print_resource_recommendations <- function(path = NULL, targets_file = NULL, show_ok = FALSE) {
    summary <- summarize_resource_usage(path, targets_file)

    if (!show_ok) {
        summary <- summary[summary$status != "ok", ]
    }

    if (nrow(summary) == 0) {
        cat("All targets are appropriately provisioned.\n")
        return(invisible(summary))
    }

    cat("Resource Usage Recommendations\n")
    cat("==============================\n\n")

    for (i in seq_len(nrow(summary))) {
        row <- summary[i, ]

        # Status indicator
        status_icon <- switch(row$status,
            "underprovisioned" = "[!]",
            "overprovisioned" = "[>]",
            "unknown" = "[?]",
            "ok" = "[ok]"
        )

        cat(sprintf("%s %s\n", status_icon, row$target))
        cat(sprintf("    Current:     %s\n", row$current_controller))
        cat(sprintf("    Recommended: %s\n", row$recommended_controller))
        cat(sprintf("    Usage: %.1f GB memory, %.0f%% CPU, %.1f min\n",
                    row$peak_memory_gb, row$peak_cpu_pct, row$duration_min))

        # Suggestion for _targets.R
        default_controller <- DEFAULT_CONTROLLER
        if (row$status != "ok" && row$current_controller != "unknown") {
            if (row$recommended_controller == default_controller) {
                cat(sprintf("    -> Remove resources (default %s is sufficient)\n", default_controller))
            } else {
                cat(sprintf("    -> Change: resources = %s\n", row$recommended_controller))
            }
        } else if (row$current_controller == "unknown") {
            if (row$recommended_controller == default_controller) {
                cat(sprintf("    -> OK: default (%s) is sufficient\n", default_controller))
            } else {
                cat(sprintf("    -> Add: resources = %s\n", row$recommended_controller))
            }
        }
        cat("\n")
    }

    cat("Legend: [!] underprovisioned, [>] overprovisioned, [?] unknown, [ok] appropriate\n")
    cat(sprintf("Default controller: %s\n", DEFAULT_CONTROLLER))

    invisible(summary)
}

### globals
utils::globalVariables(c(
  "slurm_job_id", "time", "time_bin", "min_val", "max_val", "mean_val"
))
#' Create metric plot with max n min
#'
#' @param data Data frame with log data
#' @param metric Column name to plot
#' @param y_label Y-axis label
#' @param normalize_time Whether to normalize time axis
#' @import ggplot2
#' @importFrom dplyr mutate group_by ungroup summarise
#' @keywords internal
create_metric_plot <- function(data, metric, y_label, normalize_time = FALSE) {
  # Scale time axis
  if(normalize_time) {
    data <- data |>
      group_by(slurm_job_id) |>
      mutate(
        time = (time - min(time)) / (max(time) - min(time)) * 100
      ) |>
      ungroup()
  } else {
    data <- data |>
      mutate(time = time/60)  # Convert to minutes
  }
  
  # Calculate bin statistics
  stats <- data |>
    mutate(
      time_bin = cut(time, 
                    breaks = 100, 
                    labels = FALSE)
    ) |>
    group_by(time_bin) |>
    summarise(
      time = mean(time),
      mean_val = mean(.data[[metric]], na.rm = TRUE),
      min_val = min(.data[[metric]], na.rm = TRUE),
      max_val = max(.data[[metric]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Build plot
  ggplot(stats, aes(x = time)) +
    geom_ribbon(aes(ymin = min_val, 
                    ymax = max_val),
                fill = "#4B7BE5",
                alpha = 0.2) +
    geom_line(aes(y = mean_val),
              color = "#4B7BE5",
              linewidth = 1) +
    labs(x = if(normalize_time) "Time (%)" else "Time (minutes)",
         y = y_label) +
    theme_minimal() +
    theme(legend.position = "none")
}

### globals
utils::globalVariables(c(
  "slurm_job_id", "time", "time_bin", "min_val", "max_val",
  "mean_val", "phase", "completion_time", "resident", "cpu",
  "walltime", "clean_phase_name"
))
#' Run Shiny app to explore autometric log data
#'
#' Launch an interactive Shiny application for visualizing and analyzing resource usage logs created by
#' the [autometric](https://wlandau.github.io/autometric/) package. The app provides plots and statistics
#' for memory usage, CPU utilization, and task completion times, and resource request suggestions.
#'
#' @param path Path to _targets/logs directory. If NULL, will attempt to find logs in default location
#' @param targets_file Path to _targets.R file for reading target resource configurations.
#'   If NULL, will attempt to auto-detect from path.
#'
#' @return A Shiny application object
#'
#' @details
#' The application provides:
#' * Interactive plots for memory, CPU usage and wall time analysis
#' * Resource usage statistics by job
#' * Analysis of usage patterns
#' * Controller recommendations based on observed resource requirements
#'
#' The recommendations panel uses the `_targets.R` file to determine which controller
#' each target is configured to use, then compares actual resource usage against
#' controller specifications to suggest optimizations.
#'
#' @examples
#' \dontrun{
#' explore_logs()
#' explore_logs("path/to/logs")
#' explore_logs("path/to/logs", targets_file = "path/to/_targets.R")
#' }
#'
#' @export
explore_logs <- function(path = NULL, targets_file = NULL) {
  # Load log data
  logs <- read_targets_logs(path)

  # Find and parse _targets.R for target-to-controller mapping
  if (is.null(targets_file)) {
    targets_file <- find_targets_file(path)
  }
  target_resources <- parse_target_resources(targets_file)
  
  # Add clean phase names as a column
  logs <- logs |>
    mutate(clean_phase_name = clean_phase_name(phase))
  
  # Get unique cleaned phase names
  phases <- sort(unique(logs$clean_phase_name))
  
  # Add wall time
  logs <- logs |>
    group_by(slurm_job_id) |>
    mutate(walltime = time - min(time)) |>
    ungroup()
  
  # Define UI layout
  ui <- fluidPage(
    titlePanel("Resource Usage Explorer"),
    
    sidebarLayout(
      sidebarPanel(
        selectizeInput("phase", "Select Phase:",
                    choices = phases,
                    multiple = FALSE,
                    selected = phases[1]),
        
        hr(),
        
        sliderInput("time_window", "Time Window (minutes):",
                   min = 0, max = max(logs$time)/60,
                   value = c(0, max(logs$time)/60),
                   step = 1,
                   round = TRUE),
        
        checkboxInput("normalize_time", "Normalize Time Axis", value = FALSE),
        
        hr(),
        
        helpText("Select a phase to analyze resource usage patterns."),
        helpText("Use time window to focus on specific periods."),
        helpText("Normalize time to compare runs of different durations.")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Memory Usage",
            plotOutput("memory_plot", height = "400px"),
            div(style = "margin-top: 20px",
              h4("Memory Statistics by Job"),
              tableOutput("memory_stats")
            )
          ),
          
          tabPanel("CPU Usage",
            plotOutput("cpu_plot", height = "400px"),
            div(style = "margin-top: 20px",
              h4("CPU Statistics by Job"),
              tableOutput("cpu_stats")
            )
          ),

          tabPanel("Wall Time",
            plotOutput("wall_plot", height = "400px"),
            div(style = "margin-top: 20px",
              h4("Wall Time Statistics by Job"),
              tableOutput("wall_stats")
            )
          ),
          
          tabPanel("Analysis",
            h4("Resource Usage Patterns"),
            verbatimTextOutput("analysis_text"),
            h4("Recommendations"),
            uiOutput("recommendations")  # Changed from verbatimTextOutput
            )
        )
      )
    )
  )
  
  # Define server logic
  server <- function(input, output, session) {
    # Filter data reactively
    filtered_data <- reactive({
      req(input$phase)
      
      logs |>
        filter(clean_phase_name == input$phase,
               time >= input$time_window[1] * 60,
               time <= input$time_window[2] * 60)
    })
    
    # Update time range slider
    observe({
      req(input$phase)
      phase_data <- logs[logs$clean_phase_name == input$phase,]
      max_time <- max(phase_data$time)/60
      
      updateSliderInput(session, "time_window",
                       min = 0, 
                       max = ceiling(max_time),
                       value = c(0, ceiling(max_time)))
    })
    
    # Memory plot
    output$memory_plot <- renderPlot({
      data <- filtered_data()
      if(nrow(data) == 0) return(NULL)
      
      create_metric_plot(data, 
                     "resident", 
                     "Memory Usage (GB)",
                     input$normalize_time) +
        scale_y_continuous(labels = function(x) x/1024)
    })
    
    # CPU plot
    output$cpu_plot <- renderPlot({
      data <- filtered_data()
      if(nrow(data) == 0) return(NULL)
      
      create_metric_plot(data, 
                     "cpu", 
                     "CPU Usage (%)",
                     input$normalize_time)
    })

    # Wall time distribution plot
    output$wall_plot <- renderPlot({
      data <- filtered_data()
      if(nrow(data) == 0) {
        plot.new()
        title("No data available for wall time plot")
        return()
      }
      
      completion_times <- data |>
        group_by(slurm_job_id) |>
        summarise(completion_time = if(n() > 1) { diff(range(time, na.rm = TRUE))/60 } else { diff(range(time, na.rm = TRUE))/60 })
      
      if(nrow(completion_times) < 2) {
        plot.new()
        title(sprintf("Task completed in: %.1f minutes", completion_times$completion_time))
        return()
      }
      
      ggplot(completion_times, aes(x = completion_time)) +
        geom_histogram(aes(y = after_stat(density)), 
                      fill = "#4B7BE5", 
                      alpha = 0.6, 
                      bins = 30) +
        geom_density(color = "#3C5488", 
                    linewidth = 1) +
        labs(x = "Completion Time (minutes)", 
             y = "Density") +
        theme_minimal()
    })
    
    # Memory statistics
    output$memory_stats <- renderTable({
      data <- filtered_data()
      if(nrow(data) == 0) return(NULL)
      
      data |>
        group_by(slurm_job_id) |>
        summarise(
          "Min (GB)" = min(resident/1024) |> round(1),
          "Median (GB)" = median(resident/1024) |> round(1),
          "Max (GB)" = max(resident/1024) |> round(1),
          "95th Percentile (GB)" = quantile(resident/1024, 0.95) |> round(1),
          "Duration (min)" = diff(range(time))/60 |> round(1),
          .groups = "drop"
        )
    })
    
    # CPU statistics
    output$cpu_stats <- renderTable({
      data <- filtered_data()
      if(nrow(data) == 0) return(NULL)
      
      data |>
        group_by(slurm_job_id) |>
        summarise(
          "Median (%)" = median(cpu) |> round(1),
          "Max (%)" = max(cpu) |> round(1),
          "95th Percentile (%)" = quantile(cpu, 0.95) |> round(1),
          "High CPU Time (%)" = (sum(cpu > 90) / n() * 100) |> round(1),
          "Duration (min)" = diff(range(time))/60 |> round(1),
          .groups = "drop"
        )
    })

    # Wall time statistics
    output$wall_stats <- renderTable({
      data <- filtered_data()
      if(nrow(data) == 0) return(NULL)
      
      data |>
        group_by(slurm_job_id) |>
        summarise(
          "Duration (min)" = if (n() > 1) { diff(range(time, na.rm = TRUE))/60 |> round(1) } else { NA_real_ },
          "Peak Rate (min/min)" = if (n() > 1) { max(diff(walltime/60), na.rm = TRUE) |> round(2) } else { NA_real_ },
          "Average Rate (min/min)" = if (n() > 1) { mean(diff(walltime/60), na.rm = TRUE) |> round(2) } else { NA_real_ },
          .groups = "drop"
        )
    })
    
    # Analysis summary
    output$analysis_text <- renderText({
      data <- filtered_data()
      if(nrow(data) == 0) return("No data available for analysis")
      
      analysis <- data |>
        group_by(slurm_job_id) |>
        summarise(
          peak_mem_gb = max(resident)/1024,
          median_mem_gb = median(resident)/1024,
          peak_cpu = max(cpu),
          median_cpu = median(cpu),
          duration_min = diff(range(time))/60,
          .groups = "drop"
        )
      
      paste(
        sprintf("Phase: %s\n\n", input$phase),
        "Resource Usage Summary:\n",
        sprintf("Duration: %.1f minutes (avg)\n", mean(analysis$duration_min)),
        sprintf("Total jobs: %d\n\n", nrow(analysis)),
        "Memory Usage:\n",
        sprintf("Peak memory: %.1f GB\n", max(analysis$peak_mem_gb)),
        sprintf("Average memory: %.1f GB\n\n", mean(analysis$median_mem_gb)),
        "CPU Usage:\n",
        sprintf("Peak CPU: %.1f%%\n", max(analysis$peak_cpu)),
        sprintf("Average CPU: %.1f%%", mean(analysis$median_cpu))
      )
    })
    
# Resource recommendations
output$recommendations <- renderUI({
  data <- filtered_data()
  if (nrow(data) == 0) {
    return(tags$p("No data available for recommendations"))
  }

  controllers <- autometric_hprcc_controllers()

  # Get controller name from target_resources mapping using phase name
  phase_name <- unique(data$clean_phase_name)[1]
  current_controller_name <- if (!is.null(phase_name) && phase_name %in% names(target_resources)) {
    target_resources[[phase_name]]
  } else {
    NA_character_
  }

  # Find current controller specs
  current_controller <- NULL
  if (!is.na(current_controller_name)) {
    for (controller in controllers) {
      if (controller$name == current_controller_name) {
        current_controller <- controller
        break
      }
    }
  }

  # If controller not found, use a placeholder with NA values
  if (is.null(current_controller)) {
    current_controller <- list(
      name = "unknown",
      memory_gigabytes = NA_real_,
      cpus = NA_integer_,
      walltime_minutes = NA_real_
    )
    current_controller_name <- "unknown"
  }
  
  analysis <- data |>
    group_by(slurm_job_id) |>
    summarise(
      peak_mem_gb = max(resident)/1024,
      median_mem_gb = median(resident)/1024,
      peak_cpu = max(cpu),
      median_cpu = median(cpu),
      duration_min = diff(range(time))/60,
      .groups = "drop"
    )
  
  # Required resources
  required <- list(
    memory = max(analysis$peak_mem_gb),
    cpu = max(1, ceiling(max(analysis$peak_cpu) / 100 * 8)),
    time = max(analysis$duration_min)
  )
  
  # Find optimal controller
  optimal_controller <- controllers[[1]]
  for (controller in controllers) {
    if (controller$memory_gigabytes >= required$memory && 
        controller$cpus >= required$cpu &&
        controller$walltime_minutes >= required$time) {
      optimal_controller <- controller
      break
    }
  }
  
  # Resource warnings (only when current controller specs are known)
  warnings <- list()
  if (!is.na(current_controller$memory_gigabytes) &&
      !is.na(required$memory) &&
      required$memory > 0.9 * current_controller$memory_gigabytes) {
    warnings <- c(warnings, "Memory usage close to limit")
  }
  if (!is.na(current_controller$walltime_minutes) &&
      !is.na(required$time) &&
      required$time > 0.9 * current_controller$walltime_minutes) {
    warnings <- c(warnings, "Execution time close to limit")
  }

  # Build current controller display (handle unknown controller)
  controller_known <- current_controller_name != "unknown"

  current_controller_ui <- if (controller_known) {
    tags$div(
      class = "p-3 mb-3 bg-light rounded",
      tags$h4("Current Controller", class = "text-primary mb-3"),
      tags$p(tags$strong("Controller: "), current_controller_name),
      tags$p(tags$strong("Specifications: ")),
      tags$ul(
        tags$li(sprintf("Memory: %d GB", current_controller$memory_gigabytes)),
        tags$li(sprintf("CPUs: %d", current_controller$cpus)),
        tags$li(sprintf("Wall time: %d minutes", current_controller$walltime_minutes))
      ),
      tags$p(tags$strong("Peak Usage: ")),
      tags$ul(
        tags$li(sprintf("Memory: %.1f GB", required$memory)),
        tags$li(sprintf("CPU: %.1f cores", required$cpu)),
        tags$li(sprintf("Time: %.1f minutes", required$time))
      )
    )
  } else {
    tags$div(
      class = "p-3 mb-3 bg-light rounded",
      tags$h4("Current Controller", class = "text-primary mb-3"),
      tags$p(tags$strong("Controller: "), tags$em("Unknown (not found in _targets.R)")),
      tags$p(tags$strong("Peak Usage: ")),
      tags$ul(
        tags$li(sprintf("Memory: %.1f GB", required$memory)),
        tags$li(sprintf("CPU: %.1f cores", required$cpu)),
        tags$li(sprintf("Time: %.1f minutes", required$time))
      )
    )
  }

  # Warnings section (only show if there are warnings)
  warnings_ui <- if (length(warnings) > 0) {
    tags$div(
      class = "p-3 bg-warning rounded",
      tags$h4("Warnings", class = "text-danger"),
      tags$ul(
        lapply(warnings, function(w) {
          tags$li(
            tags$i(class = "fas fa-exclamation-triangle me-2"),
            w
          )
        })
      )
    )
  } else {
    NULL
  }

  # Recommendation section (compare optimal vs current, or just show optimal if unknown)
  show_recommendation <- !is.na(current_controller_name) &&
                         current_controller_name != "unknown" &&
                         optimal_controller$name != current_controller_name
  show_optimal_only <- current_controller_name == "unknown"

  recommendation_ui <- if (show_recommendation) {
    tags$div(
      class = "p-3 mb-3 bg-light rounded",
      tags$h4("Recommendation", class = "text-info"),
      tags$p(
        tags$strong(class = "text-warning", "Suggested Controller: "),
        tags$strong(class = "text-warning", optimal_controller$name)
      ),
      tags$p("Specifications:"),
      tags$ul(
        tags$li(sprintf("Memory: %d GB", optimal_controller$memory_gigabytes)),
        tags$li(sprintf("CPUs: %d", optimal_controller$cpus)),
        tags$li(sprintf("Wall time: %d minutes", optimal_controller$walltime_minutes))
      )
    )
  } else if (show_optimal_only) {
    tags$div(
      class = "p-3 mb-3 bg-light rounded",
      tags$h4("Recommended Controller", class = "text-info"),
      tags$p("Based on observed resource usage:"),
      tags$p(
        tags$strong(class = "text-warning", "Suggested: "),
        tags$strong(class = "text-warning", optimal_controller$name)
      ),
      tags$ul(
        tags$li(sprintf("Memory: %d GB", optimal_controller$memory_gigabytes)),
        tags$li(sprintf("CPUs: %d", optimal_controller$cpus)),
        tags$li(sprintf("Wall time: %d minutes", optimal_controller$walltime_minutes))
      )
    )
  } else {
    tags$div(
      class = "p-3 mb-3 bg-light rounded",
      tags$p(
        class = "text-success",
        tags$strong("Current controller is appropriate")
      )
    )
  }

  # Build final HTML output
  tagList(
    tags$h3("Resource Usage Analysis", class = "mt-4"),
    tags$p(tags$strong("Phase: "), input$phase),
    current_controller_ui,
    warnings_ui,
    recommendation_ui
  )
})
  }
# Launch app
shinyApp(ui = ui, server = server)
}
