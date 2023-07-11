# This Script creates a shiny web app that simulated the dice rolls from the table-top game Cthulhu: Death May Die.
# The shiny web app allows the user to visualize the probabilities associated with the dice rolls and can filter for sepcific outcomes.

# Load Dependencies
library(shiny)
library(highcharter)
library(tidyverse)
library(scales)
library(shinythemes)
source("cthulhu_dice_logic.R")

ui <- fluidPage(
    theme = shinytheme("darkly"),
    includeCSS("styles.css"),
    titlePanel("Cthulhu Death May Die: Dice Distribution Visualizer"),

    sidebarLayout(
        sidebarPanel(
          
          # Shiny Widget to choose the value n in roll_n() defined in cthulhu_dice_logic.R
          selectInput(
            inputId = "rolls",
            label = "Choose the number of simulated rolls.",
            choices = c(
              "100" = 100,
              "1,000" = 1000,
              "10,000" = 10000,
              "100,000" = 100000),
            selected = 10000),
          
          # Shiny Widget to choose the number of black dice to roll.
          selectInput(
            inputId = "black",
            label = "Choose the number of black dice to roll.",
            choices = c(0:9),
            selected = 3),
          
          # Shiny Widget to choose the number of green dice to roll.
          selectInput(
            inputId = "green",
            label = "Choose the number of green dice to roll.",
            choices = c(0:9),
            selected = 0),
          
          # Shiny Widget to choose the Y-Axis display for the histogram.
          radioButtons(
            inputId = "var", 
            label = "Y-Axis Display:",
            choices = c(
              "Likelihood" = "percent",
              "Frequency" = "count")
            ),
          
          # Shiny Widgets for sliders defined in server section.
          uiOutput("success"),
          uiOutput("stars"),
          uiOutput("tentacles"),
        ),
        
        mainPanel(
          # Outputs the plot defined in server section.
          highchartOutput("hc_plot"),
          # Outputs the text defined in the server section.
          h3(verbatimTextOutput("text"))
        )
    )
)

server <- function(input, output) {
  
  # Reactive value that contains the sum of the black and green dice to roll
  value <- reactive({
    black <- as.numeric(input$black)
    green <- as.numeric(input$green)
    sum(black,green)
  })
  
  # Shiny Widget containing the number of success outcomes the user chooses.
  output$success <- renderUI({
    sliderInput(
      inputId = "success",
      label = "Choose the number of Successes you want to roll.",
      min = 0,
      max = value(),
      value = c(0, value()),
      step = 1)
  })
  
  # Shiny Widget containing the number of Elder Sign Outcomes the user chooses.
  output$stars <- renderUI({
    sliderInput(
      inputId = "stars",
      label = "Choose the number of Elder Signs you want to roll.",
      min = 0,
      max = value(),
      value = c(0, value()),
      step = 1)
  })
  
  # Shiny Widget containing the number of Tentacle Outcomes the user chooses.
  output$tentacles <- renderUI({
    sliderInput(
      inputId = "tentacles",
      label = "Choose the number of Tentacles you want to roll.",
      min = 0,
      max = as.numeric(input$black),
      value = c(0, as.numeric(input$black)),
      step = 1)
  })
  
  # Function to update the sliders when the number of black or green dice changes.
  observe({
    freezeReactiveValue(input, "success")
    updateSliderInput(inputId = "success", min = 0, max = value(), value = c(0, value()))
    updateSliderInput(inputId = "stars", min = 0, max = value(), value = c(0, value()))
    updateSliderInput(inputId = "tentacles", min = 0, max = as.numeric(input$black), value = c(0, as.numeric(input$black)))
  }, 
  priority = 50)
  
  # Reactive function for re-simulating the data when the number of rolls or dice is changed.
  rerun_data <- reactive({
      roll_n(as.numeric(input$rolls),input$black, input$green) %>%
      as_tibble() %>%
      group_by_all() %>%
      count(name = "count") %>%
      ungroup() %>%
      arrange(-count) %>%
      mutate(dist = count / as.numeric(input$rolls))
  })
  
  # Render a high charter histogram of the outcomes.
  output$hc_plot <- renderHighchart({
    df <- rerun_data()
    
    # Filter the data based on user input.
    df <- df %>%
      filter(
        between(success, input$success[1], input$success[2]),
        between(star, input$stars[1], input$stars[2]),
        between(tentacle, input$tentacles[1], input$tentacles[2])) %>%
      mutate(
        percent = dist * 100,
        percent_label = percent(dist, accuracy = 0.01, scale = 100))
    
    # Error message when an impossible set is chosen by the user/
    validate(
      need(nrow(df) != 0, "This set of filters results in an impossible outcome."))
    
      hchart(df, "column",
           hcaes(x = (1:length(!!as.symbol(input$var))), y = !!as.symbol(input$var)),
            color = "#478A54",
           tooltip = list(
             pointFormat = paste0(
               "Successes: {point.success}<br>",
               "Elder Signs: {point.star}<br>",
               "Tentacles: {point.tentacle}<br>",
               "Aproximate probability of event: {point.percent_label}"))) %>%
      hc_plotOptions(
        column = list(
          pointPadding = 0,
          borderWidth = .5,
          groupPadding = 0,
          shadow = FALSE
         )
      ) %>%
      hc_yAxis(
        title = list(text = "", style = list(color = 'white')),
        labels = list(format = ifelse(input$var == 'percent', "{value}%", "{value}"),
                      style = list(color = 'white'))) %>%
      hc_xAxis(
        title = list(text = "Rank of Event Likelihood", style = list(color = 'white')),
                     labels = list(style = list(color = 'white')))
  })
  
  # Output the text explaining the probabilities of the set chosen by the user.
  output$text <- renderText({
    df <- rerun_data()
    
    filtered_df <- 
      df %>%
      filter(
        between(success, input$success[1], input$success[2]),
        between(star, input$stars[1], input$stars[2]),
        between(tentacle, input$tentacles[1], input$tentacles[2]))
    
    paste0(
      "The chance of this event is aproximately ", 
      filtered_df %>% pull() %>% sum() %>% percent(accuracy = 0.01, scale = 100),
      "\n",
      "This set contains:", "\n",
      print_range(input$success[1], input$success[2], "Successes", value()), "\n",
      print_range(input$stars[1], input$stars[2], "Elder Signs", value()), "\n",
      print_range(input$tentacles[1], input$tentacles[2], "Tentacles", as.numeric(input$black)))
  })
}

shinyApp(ui = ui, server = server)
