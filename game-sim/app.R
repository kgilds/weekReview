library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

ui <- fluidPage(
  titlePanel("NFL Game Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Model Type"),
      radioButtons("model_type", NULL,
                   choices = c("Spread/Total Model" = "spread",
                               "Team Stats Model" = "team"),
                   selected = "spread"),
      
      hr(),
      
      h4("Simulation Settings"),
      sliderInput("n_sims", "Number of Simulations",
                  min = 1000, max = 50000, value = 10000, step = 1000),
      
      hr(),
      
      conditionalPanel(
        condition = "input.model_type == 'spread'",
        h4("Spread/Total Parameters"),
        numericInput("spread", "Spread (negative favors favorite)", 
                     value = -7.5, step = 0.5),
        numericInput("total", "Total (O/U)", 
                     value = 48.5, step = 0.5)
      ),
      
      conditionalPanel(
        condition = "input.model_type == 'team'",
        h4("Team Statistics"),
        h5("Favorite Team"),
        numericInput("team_fav_scored", "Points Scored (avg)", 
                     value = 30.4, step = 0.1),
        numericInput("team_fav_allowed", "Points Allowed (avg)", 
                     value = 19.9, step = 0.1),
        h5("Underdog Team"),
        numericInput("team_dog_scored", "Points Scored (avg)", 
                     value = 20, step = 0.1),
        numericInput("team_dog_allowed", "Points Allowed (avg)", 
                     value = 20.4, step = 0.1)
      ),
      
      hr(),
      
      h4("Actual Game Result"),
      numericInput("actual_fav", "Favorite Score", value = 24),
      numericInput("actual_dog", "Underdog Score", value = 27)
    ),
    
    mainPanel(
      width = 9,
      
      fluidRow(
        column(4, 
               div(class = "well", style = "background-color: #EBF8FF;",
                   h5("Favorite Win Probability"),
                   h3(textOutput("fav_win_prob"), style = "color: #2B6CB0;")
               )
        ),
        column(4,
               div(class = "well", style = "background-color: #F0FFF4;",
                   h5("Favorite Cover Probability"),
                   h3(textOutput("fav_cover_prob"), style = "color: #2F855A;")
               )
        ),
        column(4,
               div(class = "well", style = "background-color: #FAF5FF;",
                   h5("Over Probability"),
                   h3(textOutput("over_prob"), style = "color: #6B46C1;")
               )
        )
      ),
      
      br(),
      
      fluidRow(
        column(12,
               div(class = "well",
                   h4("Most Common Score"),
                   h4(textOutput("common_score"))
               )
        )
      ),
      
      br(),
      
      fluidRow(
        column(12,
               div(class = "well",
                   h4("Actual Result Analysis"),
                   verbatimTextOutput("actual_analysis")
               )
        )
      ),
      
      br(),
      
      fluidRow(
        column(12,
               plotOutput("margin_plot", height = "300px")
        )
      ),
      
      br(),
      
      fluidRow(
        column(12,
               plotOutput("total_plot", height = "300px")
        )
      ),
      
      br(),
      
      fluidRow(
        column(12,
               plotOutput("cover_comparison", height = "300px")
        )
      ),
      
      br(),
      
      fluidRow(
        column(12,
               div(class = "well",
                   h4("Download Data"),
                   p("Export simulation results and summary statistics to CSV files."),
                   fluidRow(
                     column(4,
                            downloadButton("download_sims", "Download Simulations", 
                                           class = "btn-primary btn-block")
                     ),
                     column(4,
                            downloadButton("download_summary", "Download Summary Stats", 
                                           class = "btn-primary btn-block")
                     ),
                     column(4,
                            downloadButton("download_score_dist", "Download Score Distribution", 
                                           class = "btn-primary btn-block")
                     )
                   )
               )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive simulation data
  sim_data <- reactive({
    set.seed(123)
    n <- input$n_sims
    score_sd <- 10
    
    if (input$model_type == "spread") {
      fav_mean <- (input$total - input$spread) / 2
      dog_mean <- (input$total + input$spread) / 2
      
      tibble(
        fav = rnorm(n, fav_mean, score_sd),
        dog = rnorm(n, dog_mean, score_sd)
      ) %>%
        mutate(
          margin = fav - dog,
          total_pts = fav + dog
        )
    } else {
      team_fav_mean <- mean(c(input$team_fav_scored, input$team_dog_allowed))
      team_dog_mean <- mean(c(input$team_dog_scored, input$team_fav_allowed))
      
      tibble(
        fav = rnorm(n, team_fav_mean, score_sd),
        dog = rnorm(n, team_dog_mean, score_sd)
      ) %>%
        mutate(
          margin = fav - dog,
          total_pts = fav + dog
        )
    }
  })
  
  # Get spread value based on model type
  spread_val <- reactive({
    if (input$model_type == "spread") {
      input$spread
    } else {
      mean(c(input$team_fav_scored, input$team_dog_allowed)) - 
        mean(c(input$team_dog_scored, input$team_fav_allowed))
    }
  })
  
  # Get total value based on model type
  total_val <- reactive({
    if (input$model_type == "spread") {
      input$total
    } else {
      mean(c(input$team_fav_scored, input$team_dog_allowed)) + 
        mean(c(input$team_dog_scored, input$team_fav_allowed))
    }
  })
  
  # Probabilities
  output$fav_win_prob <- renderText({
    sim <- sim_data()
    prob <- mean(sim$fav > sim$dog)
    paste0(round(prob * 100, 1), "%")
  })
  
  output$fav_cover_prob <- renderText({
    sim <- sim_data()
    prob <- mean(sim$margin > abs(spread_val()))
    paste0(round(prob * 100, 1), "%")
  })
  
  output$over_prob <- renderText({
    sim <- sim_data()
    prob <- mean(sim$total_pts > total_val())
    paste0(round(prob * 100, 1), "%")
  })
  
  # Most common score
  output$common_score <- renderText({
    sim <- sim_data()
    
    score_mode <- sim %>%
      mutate(
        fav_i = round(fav),
        dog_i = round(dog)
      ) %>%
      filter(fav_i >= 0, dog_i >= 0) %>%
      count(fav_i, dog_i, name = "freq") %>%
      arrange(desc(freq)) %>%
      slice(1)
    
    paste0(score_mode$fav_i, " - ", score_mode$dog_i, 
           " (", score_mode$freq, " occurrences)")
  })
  
  # Actual result analysis
  output$actual_analysis <- renderText({
    sim <- sim_data()
    actual_margin <- input$actual_fav - input$actual_dog
    
    margin_percentile <- mean(sim$margin <= actual_margin)
    
    margin_label <- case_when(
      margin_percentile < 0.60 ~ "Completely normal",
      margin_percentile < 0.90 ~ "Unusual but expected sometimes",
      TRUE ~ "True tail event"
    )
    
    paste0(
      "Actual Margin: ", round(actual_margin, 1), "\n",
      "Percentile: ", round(margin_percentile * 100, 1), "%\n",
      "Assessment: ", margin_label
    )
  })
  
  # Margin plot
  output$margin_plot <- renderPlot({
    sim <- sim_data()
    actual_margin <- input$actual_fav - input$actual_dog
    
    ggplot(sim, aes(margin)) +
      geom_histogram(bins = 60, fill = "steelblue", alpha = 0.7) +
      geom_vline(xintercept = spread_val(), linetype = "dashed", linewidth = 1) +
      geom_vline(xintercept = actual_margin, color = "red", linewidth = 1.2) +
      labs(title = "Margin of Victory Distribution",
           subtitle = "Dashed line = spread, Red line = actual margin",
           x = "Favorite Margin",
           y = "Simulated Games") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"))
  })
  
  # Total plot
  output$total_plot <- renderPlot({
    sim <- sim_data()
    
    ggplot(sim, aes(total_pts)) +
      geom_density(fill = "darkgreen", alpha = 0.4) +
      geom_vline(xintercept = total_val(), linetype = "dashed", linewidth = 1) +
      labs(title = "Total Points Distribution",
           subtitle = "Dashed line = over/under",
           x = "Total Points",
           y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"))
  })
  
  # Cover comparison plot
  output$cover_comparison <- renderPlot({
    sim <- sim_data()
    sim_cover <- mean(sim$margin > abs(spread_val()))
    market_cover <- 0.504
    
    bind_rows(
      tibble(type = "Simulation", prob = sim_cover),
      tibble(type = "Market", prob = market_cover)
    ) %>%
      ggplot(aes(type, prob, fill = type)) +
      geom_col(width = 0.5) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = c("Simulation" = "steelblue", "Market" = "coral")) +
      labs(title = "Cover Probability: Model vs Market",
           y = "Probability",
           x = NULL) +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none")
  })
  
  # Download handlers
  
  # Download raw simulations
  output$download_sims <- downloadHandler(
    filename = function() {
      paste0("nfl_simulations_", Sys.Date(), ".csv")
    },
    content = function(file) {
      sim <- sim_data()
      write.csv(sim, file, row.names = FALSE)
    }
  )
  
  # Download summary statistics
  output$download_summary <- downloadHandler(
    filename = function() {
      paste0("nfl_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      sim <- sim_data()
      actual_margin <- input$actual_fav - input$actual_dog
      actual_total <- input$actual_fav + input$actual_dog
      margin_percentile <- mean(sim$margin <= actual_margin)
      
      margin_label <- case_when(
        margin_percentile < 0.60 ~ "Completely normal",
        margin_percentile < 0.90 ~ "Unusual but expected sometimes",
        TRUE ~ "True tail event"
      )
      
      summary_data <- tibble(
        metric = c("Model Type", "Spread", "Total", "Number of Simulations",
                   "Favorite Win Probability", "Favorite Cover Probability", 
                   "Over Probability", "Actual Favorite Score", "Actual Underdog Score",
                   "Actual Margin", "Actual Total", "Margin Percentile", 
                   "Margin Assessment", "Mean Simulated Margin", "SD Simulated Margin",
                   "Mean Simulated Total", "SD Simulated Total"),
        value = c(input$model_type, 
                  round(spread_val(), 2), 
                  round(total_val(), 2), 
                  input$n_sims,
                  round(mean(sim$fav > sim$dog), 4),
                  round(mean(sim$margin > abs(spread_val())), 4),
                  round(mean(sim$total_pts > total_val()), 4),
                  input$actual_fav,
                  input$actual_dog,
                  round(actual_margin, 2),
                  round(actual_total, 2),
                  round(margin_percentile, 4),
                  margin_label,
                  round(mean(sim$margin), 2),
                  round(sd(sim$margin), 2),
                  round(mean(sim$total_pts), 2),
                  round(sd(sim$total_pts), 2))
      )
      
      write.csv(summary_data, file, row.names = FALSE)
    }
  )
  
  # Download score distribution
  output$download_score_dist <- downloadHandler(
    filename = function() {
      paste0("nfl_score_distribution_", Sys.Date(), ".csv")
    },
    content = function(file) {
      sim <- sim_data()
      
      score_dist <- sim %>%
        mutate(
          fav_score = round(fav),
          dog_score = round(dog)
        ) %>%
        filter(fav_score >= 0, dog_score >= 0) %>%
        count(fav_score, dog_score, name = "frequency") %>%
        mutate(
          probability = frequency / sum(frequency),
          cumulative_prob = cumsum(probability)
        ) %>%
        arrange(desc(frequency))
      
      write.csv(score_dist, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)