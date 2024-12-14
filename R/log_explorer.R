#' @importFrom dplyr %>% mutate filter group_by ungroup summarise n
#' @importFrom stats median quantile sd diff
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel selectizeInput hr 
#' @importFrom shiny helpText checkboxInput plotOutput div tabsetPanel tabPanel verbatimTextOutput
#' @importFrom shiny tableOutput renderPlot renderTable renderText req updateSliderInput sliderInput
#' @importFrom shiny shinyApp
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line labs theme_minimal theme geom_histogram
#' @importFrom ggplot2 geom_density scale_y_continuous

#' Remove hash suffix from phase name
#' 
#' @param phase Character vector of phase names with IDs
#' @return Character vector of cleaned phase names 
clean_phase_name <- function(phase) {
  # Remove hash suffix to group tasks by phase
  sub("_[0-9a-f]{16}$", "", phase)
}

#' Create metric plot with confidence intervals
#'
#' @param data Data frame with log data
#' @param metric Column name to plot
#' @param y_label Y-axis label
#' @param normalize_time Whether to normalize time axis
create_metric_plot <- function(data, metric, y_label, normalize_time = FALSE) {
  # Scale time axis
  if(normalize_time) {
    data <- data %>%
      group_by(slurm_job_id) %>%
      mutate(
        time = (time - min(time)) / (max(time) - min(time)) * 100
      ) %>%
      ungroup()
  } else {
    data <- data %>%
      mutate(time = time/60)  # Convert to minutes
  }
  
  # Calculate bin statistics
  stats <- data %>%
    mutate(
      time_bin = cut(time, 
                    breaks = 100, 
                    labels = FALSE)
    ) %>%
    group_by(time_bin) %>%
    summarise(
      time = mean(time),
      mean_val = mean(.data[[metric]], na.rm = TRUE),
      sd_val = sd(.data[[metric]], na.rm = TRUE),
      ci_lower = mean_val - 1.96 * sd_val/sqrt(n()),
      ci_upper = mean_val + 1.96 * sd_val/sqrt(n()),
      .groups = "drop"
    )
  
  # Build plot
  ggplot(stats, aes(x = time)) +
    geom_ribbon(aes(ymin = ci_lower, 
                    ymax = ci_upper),
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

#' Run shiny app to explore log data
#'
#' @param path Path to _targets/logs directory
#' @export
explore_logs <- function(path = NULL) {
  # Load log data
  logs <- read_target_logs(path)
  
  # Add clean phase names as a column
  logs <- logs %>%
    mutate(clean_phase_name = clean_phase_name(phase))
  
  # Get unique cleaned phase names
  phases <- sort(unique(logs$clean_phase_name))
  
  # Add wall time
  logs <- logs %>%
    group_by(slurm_job_id) %>%
    mutate(walltime = time - min(time)) %>%
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
            verbatimTextOutput("recommendations")
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
      
      logs %>%
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
    # Wall time distribution plot
    output$wall_plot <- renderPlot({
      data <- filtered_data()
      if(nrow(data) == 0) {
        plot.new()
        title("No data available for wall time plot")
        return()
      }
      
      completion_times <- data %>%
        group_by(slurm_job_id) %>%
        summarise(completion_time = if(n() > 1) { diff(range(time, na.rm = TRUE))/60 } else { NA_real_ }) %>%
        filter(!is.na(completion_time))
      
      if(nrow(completion_times) < 2) {
        plot.new()
        title("Insufficient variation in completion times")
        return()
      }
      
      ggplot(completion_times, aes(x = completion_time)) +
        geom_histogram(aes(y = ..density..), 
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
      
      data %>%
        group_by(slurm_job_id) %>%
        summarise(
          "Min (GB)" = min(resident/1024) %>% round(1),
          "Median (GB)" = median(resident/1024) %>% round(1),
          "Max (GB)" = max(resident/1024) %>% round(1),
          "95th Percentile (GB)" = quantile(resident/1024, 0.95) %>% round(1),
          "Duration (min)" = diff(range(time))/60 %>% round(1),
          .groups = "drop"
        )
    })
    
    # CPU statistics
    output$cpu_stats <- renderTable({
      data <- filtered_data()
      if(nrow(data) == 0) return(NULL)
      
      data %>%
        group_by(slurm_job_id) %>%
        summarise(
          "Median (%)" = median(cpu) %>% round(1),
          "Max (%)" = max(cpu) %>% round(1),
          "95th Percentile (%)" = quantile(cpu, 0.95) %>% round(1),
          "High CPU Time (%)" = (sum(cpu > 90) / n() * 100) %>% round(1),
          "Duration (min)" = diff(range(time))/60 %>% round(1),
          .groups = "drop"
        )
    })

    # Wall time statistics
    output$wall_stats <- renderTable({
      data <- filtered_data()
      if(nrow(data) == 0) return(NULL)
      
      data %>%
        group_by(slurm_job_id) %>%
        summarise(
          "Duration (min)" = if (n() > 1) { diff(range(time, na.rm = TRUE))/60 %>% round(1) } else { NA_real_ },
"Peak Rate (min/min)" = if (n() > 1) { max(diff(walltime/60), na.rm = TRUE) %>% round(2) } else { NA_real_ },
"Average Rate (min/min)" = if (n() > 1) { mean(diff(walltime/60), na.rm = TRUE) %>% round(2) } else { NA_real_ },
          .groups = "drop"
        )
    })
    
    # Analysis summary
    output$analysis_text <- renderText({
      data <- filtered_data()
      if(nrow(data) == 0) return("No data available for analysis")
      
      analysis <- data %>%
        group_by(slurm_job_id) %>%
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
    output$recommendations <- renderText({
      data <- filtered_data()
      if(nrow(data) == 0) return("No data available for recommendations")
      
      analysis <- data %>%
        group_by(slurm_job_id) %>%
        summarise(
          peak_mem_gb = max(resident)/1024,
          median_mem_gb = median(resident)/1024,
          peak_cpu = max(cpu),
          median_cpu = median(cpu),
          duration_min = diff(range(time))/60,
          .groups = "drop"
        )
      
      recommendations <- character()
      
      # Check memory usage
      high_mem <- analysis$peak_mem_gb > 16
      if(any(high_mem)) {
        recommendations <- c(recommendations,
          sprintf("* High memory usage (>16GB). Peak: %.1f GB", 
                  max(analysis$peak_mem_gb)))
      }
      
      # Check CPU efficiency
      low_cpu <- analysis$median_cpu < 50
      if(any(low_cpu)) {
        recommendations <- c(recommendations,
          sprintf("* Low CPU utilization. Average: %.1f%%", 
                  mean(analysis$median_cpu)))
      }
      
      # Check duration consistency
      duration_var <- sd(analysis$duration_min)/mean(analysis$duration_min)
      if(duration_var > 0.2) {
        recommendations <- c(recommendations,
          sprintf("* High variation in completion times (CV: %.1f%%)", 
                  duration_var * 100))
      }
      
      # Format output
      if(length(recommendations) == 0) {
        "Resource usage appears optimal for all jobs."
      } else {
        paste("Recommendations:\n\n",
              paste(recommendations, collapse = "\n\n"))
      }
    })
  }
  
  # Launch app
  shinyApp(ui = ui, server = server)
}