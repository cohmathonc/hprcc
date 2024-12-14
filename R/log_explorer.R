#' Clean phase name by removing unique identifiers
#'
#' @param phase Character vector of phase names with IDs
#' @return Character vector of cleaned phase names
clean_phase_name <- function(phase) {
  # Remove hex hash at end of phase name
  sub("_[0-9a-f]{32}$", "", phase)
}

#' Create phase plot with confidence intervals
#'
#' @param data Data frame with log data
#' @param metric Column name to plot
#' @param y_label Y-axis label
#' @param normalize_time Whether to normalize time axis
create_phase_plot <- function(data, metric, y_label, normalize_time = FALSE) {
  # Normalize time if requested
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
  
  # Calculate statistics in bins
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
  
  # Create plot
  ggplot(stats, aes(x = time)) +
    # Confidence interval ribbon
    geom_ribbon(aes(ymin = ci_lower, 
                    ymax = ci_upper),
                fill = "#4B7BE5",  # NPG blue
                alpha = 0.2) +
    # Mean line
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
  # Read log data using existing function
  logs <- read_target_logs(path)
  
  # Clean phase names
  logs$clean_phase <- clean_phase_name(logs$phase)
  
  # Get unique cleaned phases
  phases <- sort(unique(logs$clean_phase))
  
  # Calculate wall time for each job
  logs <- logs %>%
    group_by(slurm_job_id) %>%
    mutate(walltime = time - min(time)) %>%
    ungroup()
  
  # Define UI
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
    # Reactive filtered dataset
    filtered_data <- reactive({
      req(input$phase)
      
      logs %>%
        filter(clean_phase == input$phase,
               time >= input$time_window[1] * 60,
               time <= input$time_window[2] * 60)
    })
    
    # Update time slider based on selected phase
    observe({
      req(input$phase)
      phase_data <- logs[logs$clean_phase == input$phase,]
      max_time <- max(phase_data$time)/60  # Convert to minutes
      
      updateSliderInput(session, "time_window",
                       min = 0, 
                       max = ceiling(max_time),
                       value = c(0, ceiling(max_time)))
    })
    
    # Memory usage plot
    output$memory_plot <- renderPlot({
      data <- filtered_data()
      if(nrow(data) == 0) return(NULL)
      
      create_phase_plot(data, 
                     "resident", 
                     "Memory Usage (GB)",
                     input$normalize_time) +
        scale_y_continuous(labels = function(x) x/1024)  # Convert MB to GB
    })
    
    # CPU usage plot
    output$cpu_plot <- renderPlot({
      data <- filtered_data()
      if(nrow(data) == 0) return(NULL)
      
      create_phase_plot(data, 
                     "cpu", 
                     "CPU Usage (%)",
                     input$normalize_time)
    })

    # Wall time plot
    output$wall_plot <- renderPlot({
      data <- filtered_data()
      if(nrow(data) == 0) return(NULL)
      
      create_phase_plot(data, 
                     "walltime", 
                     "Wall Time (minutes)",
                     input$normalize_time) +
        scale_y_continuous(labels = function(x) x/60)  # Convert seconds to minutes
    })
    
    # Memory statistics table
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
    
    # CPU statistics table
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

    # Wall time statistics table
    output$wall_stats <- renderTable({
      data <- filtered_data()
      if(nrow(data) == 0) return(NULL)
      
      data %>%
        group_by(slurm_job_id) %>%
        summarise(
          "Duration (min)" = diff(range(time))/60 %>% round(1),
          "Peak Rate (%/min)" = max(diff(walltime/60)) %>% round(1),
          "Average Rate (%/min)" = mean(diff(walltime/60)) %>% round(1),
          .groups = "drop"
        )
    })
    
    # Analysis text
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
        sprintf("- Duration: %.1f minutes (avg)\n", mean(analysis$duration_min)),
        sprintf("- Total jobs: %d\n\n", nrow(analysis)),
        "Memory Usage:\n",
        sprintf("- Peak memory: %.1f GB\n", max(analysis$peak_mem_gb)),
        sprintf("- Average memory: %.1f GB\n\n", mean(analysis$median_mem_gb)),
        "CPU Usage:\n",
        sprintf("- Peak CPU: %.1f%%\n", max(analysis$peak_cpu)),
        sprintf("- Average CPU: %.1f%%", mean(analysis$median_cpu))
      )
    })
    
    # Recommendations
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
      
      # Memory recommendations
      high_mem <- analysis$peak_mem_gb > 16
      if(any(high_mem)) {
        recommendations <- c(recommendations,
          sprintf("* High memory usage (>16GB) detected. Peak: %.1f GB", 
                  max(analysis$peak_mem_gb)))
      }
      
      # CPU recommendations
      low_cpu <- analysis$median_cpu < 50
      if(any(low_cpu)) {
        recommendations <- c(recommendations,
          sprintf("* Low CPU utilization (<50%%). Average: %.1f%%", 
                  mean(analysis$median_cpu)))
      }
      
      # Duration recommendations
      duration_var <- sd(analysis$duration_min)/mean(analysis$duration_min)
      if(duration_var > 0.2) {
        recommendations <- c(recommendations,
          sprintf("* High variation in job duration (CV: %.1f%%)", 
                  duration_var * 100))
      }
      
      # Merge and format recommendations
      if(length(recommendations) == 0) {
        "Resource usage appears optimal for all jobs."
      } else {
        paste("Recommendations:\n\n",
              paste(recommendations, collapse = "\n\n"))
      }
    })
  }
  
  # Run the app
  shinyApp(ui = ui, server = server)
}