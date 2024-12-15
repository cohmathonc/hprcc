#' @import dplyr
#' @import ggplot2
#' @import shiny
#' @importFrom stats density
NULL

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

#' Create metric plot with max n min
#'
#' @param data Data frame with log data
#' @param metric Column name to plot
#' @param y_label Y-axis label
#' @param normalize_time Whether to normalize time axis
#' @import ggplot2
#' @importFrom dplyr mutate group_by ungroup summarise
#' @keywords internal
utils::globalVariables(c(
  "slurm_job_id", "time", "time_bin", "min_val", "max_val", "mean_val"
))

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

#' Run Shiny app to explore autometric log data
#'
#' Launch an interactive Shiny application for visualizing and analyzing resource usage logs created by 
#' the [autometric](https://wlandau.github.io/autometric/) package. The app provides plots and statistics
#' for memory usage, CPU utilization, and task completion times, and resource request suggestions.
#'
#' @param path Path to _targets/logs directory. If NULL, will attempt to find logs in default location
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
#' @examples
#' \dontrun{
#' explore_logs()
#' explore_logs("path/to/logs")
#' }
#'
#' @export
utils::globalVariables(c(
  "slurm_job_id", "time", "time_bin", "min_val", "max_val", 
  "mean_val", "phase", "completion_time", "resident", "cpu",
  "walltime"
))

explore_logs <- function(path = NULL) {
  # Load log data
  logs <- read_targets_logs(path)
  
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
  worker_name <- unique(data$name)[1]
  current_controller_name <- regmatches(worker_name, regexec("crew_worker_([^_]+)", worker_name))[[1]][2]
  
  # Find current controller specs
  current_controller <- NULL
  for (controller in controllers) {
    if (controller$name == current_controller_name) {
      current_controller <- controller
      break
    }
  }
  
  if (is.null(current_controller)) {
    return(tags$p("Error: Could not determine current controller"))
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
  
  # Resource warnings
  warnings <- list()
  if (required$memory > 0.9 * current_controller$memory_gigabytes) {
    warnings <- c(warnings, "Memory usage close to limit")
  }
  if (required$time > 0.9 * current_controller$walltime_minutes) {
    warnings <- c(warnings, "Execution time close to limit")
  }
  
  # Build HTML output
  tagList(
    tags$h3("Resource Usage Analysis", class = "mt-4"),
    tags$p(tags$strong("Phase: "), input$phase),
    
    # Current controller section
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
    ),
        # Warnings section
    if (length(warnings) > 0) {
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
    },
    # Recommendation section
    if (optimal_controller$name != current_controller_name) {
      tags$div(
        class = "p-3 mb-3 bg-light rounded",
        tags$h4("Recommendation", class = "text-info"),
        tags$p(
          tags$strong(class = "text-warning", "Suggested Controller: "), 
          tags$strong(class = "text-warning", optimal_controller$name),
          tags$p("Specifications:"),
          tags$ul(
            tags$li(sprintf("Memory: %d GB", optimal_controller$memory_gigabytes)),
            tags$li(sprintf("CPUs: %d", optimal_controller$cpus)),
            tags$li(sprintf("Wall time: %d minutes", optimal_controller$walltime_minutes))
          )
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
  )
})
  }
# Launch app
shinyApp(ui = ui, server = server)
}
