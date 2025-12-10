library(shiny)
library(bslib)
library(DT)
library(shinydashboard)

ui <- page_navbar(
  title = "Weekly Review",
  theme = bs_theme(bootswatch = "flatly"),

  nav_panel(
    "Get Clear",
    layout_columns(
      card(
        card_header("Capture & Clean Up"),
        checkboxInput("chat_cleared", "Chat/ VDC / messages cleared"),
        checkboxInput("inbox_emptied", "Physical inbox emptied"),
        checkboxInput(
          "notes_captured",
          "Notes from the week captured (notebooks, meeting minutes, loose notes)"
        )
      ),
      card(
        card_header("Loose Ends"),
        checkboxInput("promises_made", "Promises made to others"),
        checkboxInput("followups_owed", "Follow-ups owed to others"),
        checkboxInput(
          "approvals_pending",
          "Any approvals, reviews, or sign-offs pending"
        )
      )
    )
  ),

  nav_panel(
    "Get Current",
    layout_columns(
      card(
        card_header("Calendar Review - Last Week"),
        checkboxInput("last_week_wins", "Wins"),
        textAreaInput("wins_text", "Detail your wins:", rows = 3),
        checkboxInput("missed_commitments", "Missed commitments"),
        textAreaInput("missed_text", "Detail missed commitments:", rows = 3)
      ),
      card(
        card_header("Calendar Review - Next 2 Weeks"),
        checkboxInput("prep_needed", "Prep needed"),
        textAreaInput("prep_text", "What prep is needed:", rows = 3),
        checkboxInput("conversations_schedule", "Conversations to schedule"),
        textAreaInput(
          "conversations_text",
          "Conversations to schedule:",
          rows = 3
        )
      )
    ),
    layout_columns(
      card(
        card_header("Projects / Areas"),
        checkboxInput("review_projects", "Review all active projects"),
        checkboxInput("update_status", "Update status and next actions"),
        checkboxInput("identify_risks", "Identify projects at risk"),
        textAreaInput("projects_notes", "Project notes:", rows = 4)
      ),
      card(
        card_header("Processes / Areas"),
        checkboxInput("review_processes", "Review weekly processes"),
        checkboxInput("update_process", "Update status and next actions"),
        checkboxInput(
          "identify_process_risks",
          "Identify processes not being addressed"
        ),
        textAreaInput("process_notes", "Process notes:", rows = 4)
      ),
      card(
        card_header("Waiting For"),
        checkboxInput("stuck_items", "Items that are stuck"),
        checkboxInput("checkins_needed", "Check-ins needed"),
        checkboxInput("people_nudge", "People to nudge"),
        textAreaInput("waiting_notes", "Waiting for notes:", rows = 4)
      )
    ),
    card(
      card_header("Team / Leadership Responsibilities"),
      layout_columns(
        div(
          checkboxInput("staff_support", "Staff needing support"),
          checkboxInput("consultant_direction", "Consultants needing direction")
        ),
        div(
          checkboxInput("bottlenecks_clear", "Bottlenecks to clear"),
          checkboxInput("decisions_needed", "Decisions only you can make")
        )
      ),
      textAreaInput("leadership_notes", "Leadership notes:", rows = 3)
    )
  ),

  nav_panel(
    "Get Intentional",
    card(
      card_header("Key Outcomes for Next Week"),
      textInput("outcome1", "1.", placeholder = "Key outcome 1"),
      textInput("outcome2", "2.", placeholder = "Key outcome 2"),
      textInput("outcome3", "3.", placeholder = "Key outcome 3"),
      textInput("outcome4", "4.", placeholder = "Key outcome 4"),
      textInput("outcome5", "5.", placeholder = "Key outcome 5")
    ),
    layout_columns(
      card(
        card_header("Top Priorities by Context"),
        h6("Deep Work / Thinking"),
        textAreaInput("deep_work", "", rows = 2),
        h6("Manager Tasks (Approvals, reviews, decisions)"),
        textAreaInput("manager_tasks", "", rows = 2),
        h6("People / Conversations"),
        textAreaInput("people_conversations", "", rows = 2),
        h6("Quick Hits (<10 minutes)"),
        textAreaInput("quick_hits", "", rows = 2)
      ),
      card(
        card_header("Block Your Week"),
        checkboxInput("deep_work_scheduled", "Deep work blocks scheduled"),
        checkboxInput("oneones_scheduled", "1:1s & check-ins scheduled"),
        checkboxInput("admin_scheduled", "Administrative work scheduled"),
        checkboxInput("travel_scheduled", "Travel / field time scheduled"),
        checkboxInput(
          "no_meeting_blocked",
          "No-meeting time blocked (if possible)"
        )
      )
    )
  ),

  nav_panel(
    "Get Aligned",
    card(
      card_header("Bigger Picture Alignment"),
      layout_columns(
        div(
          checkboxInput("role_goals", "Alignment with role goals"),
          checkboxInput("team_goals", "Alignment with team goals")
        ),
        div(
          checkboxInput("program_goals", "Alignment with program goals"),
          checkboxInput("emerging_risks", "Emerging risks"),
          checkboxInput("opportunities", "Opportunities")
        )
      ),
      textAreaInput("alignment_notes", "Alignment notes:", rows = 4)
    )
  ),

  nav_panel(
    "Get Centered",
    card(
      card_header("Personal Reset"),
      textAreaInput("energized", "What energized me this week?", rows = 3),
      textAreaInput("drained", "What drained me?", rows = 3),
      textAreaInput(
        "stop_start_continue",
        "What should I stop / start / continue?",
        rows = 3
      ),
      textAreaInput(
        "successful_week",
        "What would make next week feel successful?",
        rows = 3
      )
    )
  ),

  nav_panel(
    "Finalize",
    card(
      card_header("Final Steps"),
      checkboxInput("system_reviewed", "System reviewed one final time"),
      textInput(
        "one_thing",
        "Choose 'One Thing' for next week:",
        placeholder = "Your most important focus"
      ),
      checkboxInput("gratitude", "Close out with gratitude"),
      textAreaInput(
        "gratitude_text",
        "What are you grateful for this week?",
        rows = 3
      ),
      br(),
      layout_columns(
        actionButton(
          "save_review",
          "Save Weekly Review",
          class = "btn-primary btn-lg"
        ),
        downloadButton(
          "download_report",
          "Download Report",
          class = "btn-success btn-lg"
        )
      )
    ),
    card(
      card_header("Review Summary"),
      verbatimTextOutput("summary")
    )
  )
)

server <- function(input, output, session) {
  # Calculate completion progress
  completion_progress <- reactive({
    # Get Clear section (6 items)
    get_clear <- sum(
      input$chat_cleared,
      input$inbox_emptied,
      input$notes_captured,
      input$promises_made,
      input$followups_owed,
      input$approvals_pending
    )

    # Get Current section (14 items)
    get_current <- sum(
      input$last_week_wins,
      input$missed_commitments,
      input$prep_needed,
      input$conversations_schedule,
      input$review_projects,
      input$update_status,
      input$identify_risks,
      input$review_processes,
      input$update_process,
      input$identify_process_risks,
      input$stuck_items,
      input$checkins_needed,
      input$people_nudge,
      input$staff_support,
      input$consultant_direction,
      input$bottlenecks_clear,
      input$decisions_needed
    )

    # Get Intentional section (5 scheduling items)
    get_intentional <- sum(
      input$deep_work_scheduled,
      input$oneones_scheduled,
      input$admin_scheduled,
      input$travel_scheduled,
      input$no_meeting_blocked
    )

    # Get Aligned section (5 items)
    get_aligned <- sum(
      input$role_goals,
      input$team_goals,
      input$program_goals,
      input$emerging_risks,
      input$opportunities
    )

    # Finalize section (2 items)
    finalize <- sum(input$system_reviewed, input$gratitude)

    total_checks <- get_clear +
      get_current +
      get_intentional +
      get_aligned +
      finalize
    total_possible <- 32

    list(
      get_clear = get_clear,
      get_current = get_current,
      get_intentional = get_intentional,
      get_aligned = get_aligned,
      finalize = finalize,
      total = total_checks,
      percentage = round(total_checks / total_possible * 100, 1)
    )
  })

  # Generate summary
  output$summary <- renderText({
    progress <- completion_progress()

    summary_text <- paste0(
      "=== WEEKLY REVIEW SUMMARY ===\n\n",
      "Overall Completion: ",
      progress$percentage,
      "% (",
      progress$total,
      "/35 items)\n\n",
      "Section Breakdown:\n",
      "• Get Clear: ",
      progress$get_clear,
      "/9 items\n",
      "• Get Current: ",
      progress$get_current,
      "/14 items\n",
      "• Get Intentional: ",
      progress$get_intentional,
      "/5 items\n",
      "• Get Aligned: ",
      progress$get_aligned,
      "/5 items\n",
      "• Finalize: ",
      progress$finalize,
      "/2 items\n\n"
    )

    if (input$one_thing != "") {
      summary_text <- paste0(
        summary_text,
        "ONE THING for next week: ",
        input$one_thing,
        "\n\n"
      )
    }

    if (nchar(input$gratitude_text) > 0) {
      summary_text <- paste0(summary_text, "GRATITUDE:\n", input$gratitude_text)
    }

    summary_text
  })

  # Generate comprehensive report content
  generate_report_content <- reactive({
    progress <- completion_progress()

    # Helper function to create checkbox indicator
    checkbox_status <- function(value) {
      ifelse(value, "[X]", "[ ]")
    }

    # Helper function to add content (always show label, content if exists)
    add_content <- function(label, content, always_show = FALSE) {
      content_trimmed <- trimws(content)
      if (nchar(content_trimmed) > 0) {
        paste0(
          "    ",
          label,
          ":\n    ",
          gsub("\n", "\n    ", content_trimmed),
          "\n\n"
        )
      } else if (always_show) {
        paste0("    ", label, ": [No content entered]\n\n")
      } else {
        ""
      }
    }

    report <- paste0(
      "WEEKLY REVIEW REPORT\n",
      paste(rep("=", 60), collapse = ""),
      "\n",
      "Date: ",
      format(Sys.Date(), "%B %d, %Y"),
      "\n",
      "Overall Completion: ",
      progress$percentage,
      "% (",
      progress$total,
      "/35 items)\n\n",

      "GET CLEAR (Capture & Clean Up)\n",
      paste(rep("-", 60), collapse = ""),
      "\n",
      "Capture & Clean Up:\n",
      "  ",
      checkbox_status(input$chat_cleared),
      " Chat messages cleared\n",
      "  ",
      checkbox_status(input$inbox_emptied),
      " Physical inbox emptied\n",
      "  ",
      checkbox_status(input$notes_captured),
      " Notes from week captured\n\n",

      "Loose Ends:\n",
      "  ",
      checkbox_status(input$promises_made),
      " Promises made to others\n",
      "  ",
      checkbox_status(input$followups_owed),
      " Follow-ups owed to others\n",
      "  ",
      checkbox_status(input$approvals_pending),
      " Approvals/reviews/sign-offs pending\n\n",

      "GET CURRENT (Review)\n",
      paste(rep("-", 60), collapse = ""),
      "\n",
      "Calendar Review - Last Week:\n",
      "  ",
      checkbox_status(input$last_week_wins),
      " Wins\n",
      add_content("Wins Details", input$wins_text, TRUE),
      "  ",
      checkbox_status(input$missed_commitments),
      " Missed commitments\n",
      add_content("Missed Commitments Details", input$missed_text, TRUE),

      "Calendar Review - Next 2 Weeks:\n",
      "  ",
      checkbox_status(input$prep_needed),
      " Prep needed\n",
      add_content("Prep Details", input$prep_text, TRUE),
      "  ",
      checkbox_status(input$conversations_schedule),
      " Conversations to schedule\n",
      add_content("Conversations Details", input$conversations_text, TRUE),

      "Projects / Areas:\n",
      "  ",
      checkbox_status(input$review_projects),
      " Review all active projects\n",
      "  ",
      checkbox_status(input$update_status),
      " Update status and next actions\n",
      "  ",
      checkbox_status(input$identify_risks),
      " Identify projects at risk\n",
      add_content("Project Notes", input$projects_notes, TRUE),

      "Process / Areas:\n",
      "  ",
      checkbox_status(input$review_processes),
      " Review all active projects\n",
      "  ",
      checkbox_status(input$update_process),
      " Update status and next actions\n",
      "  ",
      checkbox_status(input$identify_process_risks),
      " Identify proccesses at risk\n",
      add_content("Process Notes", input$process_notes, TRUE),

      "Waiting For:\n",
      "  ",
      checkbox_status(input$stuck_items),
      " Items that are stuck\n",
      "  ",
      checkbox_status(input$checkins_needed),
      " Check-ins needed\n",
      "  ",
      checkbox_status(input$people_nudge),
      " People to nudge\n",
      add_content("Waiting For Notes", input$waiting_notes, TRUE),

      "Team / Leadership Responsibilities:\n",
      "  ",
      checkbox_status(input$staff_support),
      " Staff needing support\n",
      "  ",
      checkbox_status(input$consultant_direction),
      " Consultants needing direction\n",
      "  ",
      checkbox_status(input$bottlenecks_clear),
      " Bottlenecks to clear\n",
      "  ",
      checkbox_status(input$decisions_needed),
      " Decisions only you can make\n",
      add_content("Leadership Notes", input$leadership_notes, TRUE),

      "GET INTENTIONAL (Planning)\n",
      paste(rep("-", 60), collapse = ""),
      "\n",
      "Key Outcomes for Next Week:\n"
    )

    # Add key outcomes
    outcomes <- c(
      input$outcome1,
      input$outcome2,
      input$outcome3,
      input$outcome4,
      input$outcome5
    )
    for (i in 1:5) {
      if (nchar(trimws(outcomes[i])) > 0) {
        report <- paste0(report, "  ", i, ". ", outcomes[i], "\n")
      }
    }
    report <- paste0(report, "\n")

    report <- paste0(
      report,
      "Top Priorities by Context:\n",
      add_content("Deep Work / Thinking", input$deep_work, TRUE),
      add_content("Manager Tasks", input$manager_tasks, TRUE),
      add_content("People / Conversations", input$people_conversations, TRUE),
      add_content("Quick Hits (<10 minutes)", input$quick_hits, TRUE),

      "Block Your Week:\n",
      "  ",
      checkbox_status(input$deep_work_scheduled),
      " Deep work blocks scheduled\n",
      "  ",
      checkbox_status(input$oneones_scheduled),
      " 1:1s & check-ins scheduled\n",
      "  ",
      checkbox_status(input$admin_scheduled),
      " Administrative work scheduled\n",
      "  ",
      checkbox_status(input$travel_scheduled),
      " Travel / field time scheduled\n",
      "  ",
      checkbox_status(input$no_meeting_blocked),
      " No-meeting time blocked\n\n",

      "GET ALIGNED (Bigger Picture)\n",
      paste(rep("-", 60), collapse = ""),
      "\n",
      "Alignment Check:\n",
      "  ",
      checkbox_status(input$role_goals),
      " Alignment with role goals\n",
      "  ",
      checkbox_status(input$team_goals),
      " Alignment with team goals\n",
      "  ",
      checkbox_status(input$program_goals),
      " Alignment with program goals\n",
      "  ",
      checkbox_status(input$emerging_risks),
      " Emerging risks\n",
      "  ",
      checkbox_status(input$opportunities),
      " Opportunities\n",
      add_content("Alignment Notes", input$alignment_notes, TRUE),

      "GET CENTERED (Personal Reset)\n",
      paste(rep("-", 60), collapse = ""),
      "\n",
      add_content("What energized me this week", input$energized, TRUE),
      add_content("What drained me", input$drained, TRUE),
      add_content("Stop / Start / Continue", input$stop_start_continue, TRUE),
      add_content(
        "What would make next week feel successful",
        input$successful_week,
        TRUE
      ),

      "FINALIZE\n",
      paste(rep("-", 60), collapse = ""),
      "\n",
      "  ",
      checkbox_status(input$system_reviewed),
      " System reviewed one final time\n",
      "  ",
      checkbox_status(input$gratitude),
      " Close out with gratitude\n\n"
    )

    if (nchar(trimws(input$one_thing)) > 0) {
      report <- paste0(
        report,
        "ONE THING for next week:\n  ",
        input$one_thing,
        "\n\n"
      )
    }

    if (nchar(trimws(input$gratitude_text)) > 0) {
      report <- paste0(report, "GRATITUDE:\n", input$gratitude_text, "\n\n")
    }

    report <- paste0(
      report,
      paste(rep("=", 60), collapse = ""),
      "\n",
      "End of Weekly Review\n"
    )

    return(report)
  })

  # Download handler
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("weekly_review_", format(Sys.Date(), "%Y%m%d"), ".txt")
    },
    content = function(file) {
      writeLines(generate_report_content(), file)
    }
  )

  # Save review action (could be enhanced to save to database)
  observeEvent(input$save_review, {
    showModal(modalDialog(
      title = "Review Saved",
      "Your weekly review has been saved! Use the Download Report button to export it.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
}

shinyApp(ui = ui, server = server)
