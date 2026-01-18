library(shiny)
library(bslib)
library(googlesheets4)
library(googledrive)

# Authenticate with Google Sheets
gs4_auth(cache = ".secrets", email = TRUE)

# Define UI
ui <- page_sidebar(
  title = "Task Duration Timer",
  sidebar = sidebar(
    width = 300,
    h4("Task Setup"),
    textInput("task_name", 
              "Task Name:", 
              placeholder = "Enter task description"),
    
    selectInput("category", 
                "Category:",
                choices = c("Program", "Admin", "Billing", "Tools"),
                selected = "Program"),
    
    br(),
    actionButton("start_timer", 
                 "Start Timer", 
                 class = "btn-primary btn-lg",
                 style = "width: 100%;"),
    
    br(), br(),
    actionButton("stop_timer", 
                 "Stop & Record", 
                 class = "btn-success",
                 style = "width: 100%;"),
    
    br(), br(),
    actionButton("reset_timer", 
                 "Reset Timer", 
                 class = "btn-warning",
                 style = "width: 100%;"),
    
    br(), br(),
    div(
      style = "font-size: 0.9em; color: #666;",
      p("Note: Timer counts up to 25 minutes max. Stops automatically at limit.")
    ),
    
    # Google Sheets sync section
    hr(),
    h4("Google Sheets"),
    textInput("sheet_url", 
              "Google Sheet URL:", 
              placeholder = "Paste Google Sheet URL here"),
    
    actionButton("test_connection", 
                 "Test Connection", 
                 class = "btn-info",
                 icon = icon("check"),
                 style = "width: 100%; margin-bottom: 10px;"),
    
    checkboxInput("auto_sync", 
                  "Auto-save to Sheet", 
                  value = TRUE),
    
    div(
      style = "font-size: 0.85em; color: #666; margin-top: 10px;",
      p("Tasks will be appended to the sheet automatically when completed.")
    )
  ),
  
  # Main panel with timer display
  card(
    card_header("Timer Display"),
    div(
      style = "text-align: center; padding: 20px;",
      h1(textOutput("timer_display"), 
         style = "font-size: 4em; font-weight: bold; color: #2c3e50;"),
      h3(textOutput("current_task"), 
         style = "color: #34495e; margin-top: 20px;"),
      h4(textOutput("timer_status"), 
         style = "margin-top: 10px;"),
      br(),
      # Progress bar
      div(
        style = "width: 80%; margin: 0 auto;",
        uiOutput("timer_progress")
      )
    )
  ),
  
  card(
    card_header("Recent Activity"),
    div(
      style = "padding: 15px;",
      uiOutput("recent_tasks")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store timer state
  values <- reactiveValues(
    timer_active = FALSE,
    time_elapsed = 0,
    start_time = NULL,
    max_time = 25 * 60, # 25 minutes maximum
    sheet_id = NULL,
    recent_tasks = list() # Keep last 5 tasks for display
  )
  
  # Function to append task to Google Sheet
  append_to_sheet <- function(task_data) {
    if (is.null(input$sheet_url) || input$sheet_url == "") {
      showNotification("Google Sheet URL not configured. Task not saved to sheet.", 
                       type = "warning", 
                       duration = 5)
      return(FALSE)
    }
    
    tryCatch({
      sheet_id <- as_sheets_id(input$sheet_url)
      
      # Try to append to existing sheet, or create new one if it doesn't exist
      tryCatch({
        sheet_append(sheet_id, data = task_data, sheet = "Task History")
      }, error = function(e) {
        # If sheet doesn't exist, create it first
        sheet_write(task_data, ss = sheet_id, sheet = "Task History")
      })
      
      showNotification("✓ Task saved to Google Sheet!", 
                       type = "message", 
                       duration = 3)
      return(TRUE)
      
    }, error = function(e) {
      showNotification(paste("Error saving to Google Sheet:", e$message), 
                       type = "error", 
                       duration = 10)
      return(FALSE)
    })
  }
  
  # Test Google Sheets connection
  observeEvent(input$test_connection, {
    if (is.null(input$sheet_url) || input$sheet_url == "") {
      showNotification("Please enter a Google Sheet URL first.", type = "warning")
      return()
    }
    
    tryCatch({
      sheet_id <- as_sheets_id(input$sheet_url)
      sheet_info <- gs4_get(sheet_id)
      
      showNotification(paste0("✓ Connected to: ", sheet_info$name), 
                       type = "message", 
                       duration = 5)
      values$sheet_id <- sheet_id
      
    }, error = function(e) {
      showNotification(paste("Connection failed:", e$message), 
                       type = "error", 
                       duration = 10)
    })
  })
  
  # Timer that counts UP
  observe({
    if (values$timer_active) {
      invalidateLater(1000, session)
      
      current_time <- Sys.time()
      values$time_elapsed <- as.numeric(difftime(current_time, values$start_time, units = "secs"))
      
      if (values$time_elapsed >= values$max_time) {
        values$timer_active <- FALSE
        values$time_elapsed <- values$max_time
        
        end_time <- Sys.time()
        duration_mins <- round(values$time_elapsed / 60, 1)
        
        new_task <- data.frame(
          Task = input$task_name,
          Category = input$category,
          StartTime = format(values$start_time, "%Y-%m-%d %H:%M:%S"),
          EndTime = format(end_time, "%Y-%m-%d %H:%M:%S"),
          Duration = paste(duration_mins, "minutes"),
          Status = "Reached 25-min limit",
          stringsAsFactors = FALSE
        )
        
        # Add to recent tasks for display
        values$recent_tasks <- c(list(new_task), values$recent_tasks)
        if (length(values$recent_tasks) > 5) {
          values$recent_tasks <- values$recent_tasks[1:5]
        }
        
        showNotification("⏰ Timer reached 25-minute maximum!", 
                         type = "warning", 
                         duration = 10)
        
        # Save to Google Sheet if auto-sync is enabled
        if (input$auto_sync) {
          append_to_sheet(new_task)
        }
      }
    }
  })
  
  # Start timer event
  observeEvent(input$start_timer, {
    if (input$task_name == "" || is.null(input$task_name)) {
      showNotification("Please enter a task name before starting the timer.", 
                       type = "warning")
      return()
    }
    
    if (!values$timer_active) {
      values$timer_active <- TRUE
      values$start_time <- Sys.time()
      values$time_elapsed <- 0
      
      showNotification("Timer started! Tracking task duration.", 
                       type = "message")
    }
  })
  
  # Stop timer event
  observeEvent(input$stop_timer, {
    req(input$task_name)
    
    if (is.null(values$start_time) || values$time_elapsed == 0) {
      showNotification("No active timer to record. Please start the timer first.", 
                       type = "warning")
      return()
    }
    
    values$timer_active <- FALSE
    
    end_time <- Sys.time()
    duration_mins <- round(values$time_elapsed / 60, 1)
    
    tryCatch({
      new_task <- data.frame(
        Task = as.character(input$task_name),
        Category = as.character(input$category),
        StartTime = format(values$start_time, "%Y-%m-%d %H:%M:%S"),
        EndTime = format(end_time, "%Y-%m-%d %H:%M:%S"),
        Duration = paste(duration_mins, "minutes"),
        Status = "Completed",
        stringsAsFactors = FALSE
      )
      
      # Add to recent tasks for display
      values$recent_tasks <- c(list(new_task), values$recent_tasks)
      if (length(values$recent_tasks) > 5) {
        values$recent_tasks <- values$recent_tasks[1:5]
      }
      
      showNotification(paste0("Task recorded! Duration: ", duration_mins, " minutes"), 
                       type = "message")
      
      # Save to Google Sheet if auto-sync is enabled
      if (input$auto_sync) {
        append_to_sheet(new_task)
      }
      
      # Reset timer for next task
      values$time_elapsed <- 0
      values$start_time <- NULL
      
    }, error = function(e) {
      showNotification(paste("Error recording task:", e$message), type = "error")
    })
  })
  
  # Reset timer event
  observeEvent(input$reset_timer, {
    values$timer_active <- FALSE
    values$time_elapsed <- 0
    showNotification("Timer reset to 0:00.", type = "message")
  })
  
  # Timer display output
  output$timer_display <- renderText({
    minutes <- floor(values$time_elapsed / 60)
    seconds <- round(values$time_elapsed %% 60)
    sprintf("%02d:%02d", minutes, seconds)
  })
  
  # Progress bar output
  output$timer_progress <- renderUI({
    progress_value <- (values$time_elapsed / values$max_time) * 100
    progress_value <- min(progress_value, 100)
    
    bar_color <- if (progress_value < 80) {
      "#28a745"
    } else if (progress_value < 95) {
      "#ffc107"
    } else {
      "#dc3545"
    }
    
    div(
      class = "progress",
      style = "height: 20px;",
      div(
        class = "progress-bar",
        role = "progressbar",
        style = paste0("width: ", progress_value, "%; background-color: ", bar_color, ";"),
        paste0(round(progress_value, 1), "%")
      )
    )
  })
  
  # Current task display
  output$current_task <- renderText({
    if (values$timer_active && input$task_name != "") {
      paste("Current Task:", input$task_name)
    } else if (input$task_name != "") {
      paste("Ready to start:", input$task_name)
    } else {
      "No task selected"
    }
  })
  
  # Timer status display
  output$timer_status <- renderText({
    if (values$timer_active) {
      remaining_mins <- round((values$max_time - values$time_elapsed) / 60, 1)
      paste("Status: Running |", input$category, "| Time until 25-min limit:", remaining_mins, "min")
    } else if (values$time_elapsed > 0) {
      "Status: Stopped"
    } else {
      "Status: Ready to start"
    }
  })
  
  # Recent tasks display
  output$recent_tasks <- renderUI({
    if (length(values$recent_tasks) == 0) {
      return(div(
        style = "text-align: center; color: #999; padding: 20px;",
        p("No tasks completed yet in this session.")
      ))
    }
    
    task_items <- lapply(values$recent_tasks, function(task) {
      div(
        style = "border-left: 4px solid #28a745; padding: 10px; margin-bottom: 10px; background-color: #f8f9fa;",
        strong(task$Task),
        br(),
        span(style = "color: #666; font-size: 0.9em;",
             paste(task$Category, "•", task$Duration, "•", task$Status)
        ),
        br(),
        span(style = "color: #999; font-size: 0.85em;",
             paste("Completed:", task$EndTime)
        )
      )
    })
    
    div(
      h5("Last 5 completed tasks this session:"),
      task_items
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)