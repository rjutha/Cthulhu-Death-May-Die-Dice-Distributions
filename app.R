library(shiny)
library(highcharter)
library(tidyverse)
library(scales)
library(bslib)
library(shinythemes)

source("cthulhu dice.R")

ui <- fluidPage(
    theme = shinytheme("darkly"),
    # Application title
    includeCSS("styles.css"),
    titlePanel("Cthulhu Death May Die Dice Distribution"),

    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "rolls",
                         label = "Number of Simulated Rolls.",
                         choices = c("100" = 100,
                                     "1,000" = 1000,
                                     "10,000" = 10000,
                                     "100,000" = 100000),
                         selected = 10000),
            selectInput(inputId = "black",
                        label = "Number of black dice to roll.",
                        choices = c(0:9),
                        selected = 3),
            selectInput(inputId = "green",
                        label = "Number of green dice to roll.",
                        choices = c(0:9),
                        selected = 0),
          radioButtons("var", "Y-Axis Display:",
                       c("Likelihood" = "percent",
                         "Frequency" = "count")),
          sliderInput("success", "Filter number of Successes.",
                      min = 0, 
                      max = 3,
                      value = c(0,3)),
          sliderInput("stars", "Filter number of Stars.",
                      min = 0, 
                      max = 9,
                      value = c(0,9)),
          sliderInput("tentacles", "Filter number of Tentacles.",
                      min = 0, 
                      max = 9,
                      value = c(0,9))
        ),
        
        mainPanel(
          highchartOutput("hc_plot"),
          h3(verbatimTextOutput("text")),
          h3(htmlOutput("text2"))
        )
    )
)

server <- function(input, output) {
  
  observeEvent(input$black, {
    updateSliderInput(inputId = "success", min = 0, max = input$black)
    updateSliderInput(inputId = "success", value = c(0,input$black))
  })
  
  
  
  rerun_data <- reactive({
  
      roll_n(as.numeric(input$rolls),input$black, input$green) %>%
      as_tibble() %>%
      select(-probability) %>%
      group_by_all() %>%
      count(name = "count") %>%
      ungroup() %>%
      arrange(-count) %>%
      mutate(dist = count / as.numeric(input$rolls))
  })

  output$hc_plot <- renderHighchart({
    df <- rerun_data()
    
    df %>%
      filter(
        between(success, input$success[1], input$success[2]),
        between(star, input$stars[1], input$stars[2]),
        between(tentacle, input$tentacles[1], input$tentacles[2])) %>%
      mutate(
        percent = dist * 100,
        percent_label = percent(dist, accuracy = 0.01, scale = 100)) %>%
      hchart("column",
           hcaes(x = (1:length(dist)), y = !!as.symbol(input$var)),
            color = "#478A54",
           tooltip = list(
             pointFormat = paste0(
               "Successes: {point.success}<br>",
               "Stars: {point.star}<br>",
               "Tentacles: {point.tentacle}<br>",
               "Probability of event: {point.percent_label}"))) %>%
      hc_plotOptions(
        column = list(
          pointPadding = 0,
          borderWidth = .5,
          groupPadding = 0,
          shadow = FALSE
          #,crisp = FALSE # Come back to fix this
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
  
  output$text2 <- renderUI({
    df <- rerun_data()
    
    filtered_df <- 
      df %>%
      filter(
        between(success, input$success[1], input$success[2]),
        between(star, input$stars[1], input$stars[2]),
        between(tentacle, input$tentacles[1], input$tentacles[2]))
    
    HTML(
      paste0("The probabiilty of obtaining this set is ", 
           filtered_df %>% pull() %>% sum() %>% percent(accuracy = 0.01, scale = 100),
           "</br>",
           "This set contains:", "</br>",
           print_range(input$success[1], input$success[2], "Successes"), "</br>",
           print_range(input$stars[1], input$stars[2], "Stars"), "</br>",
           print_range(input$tentacles[1], input$tentacles[2], "Tentacles"))
    )
  })
  
  output$text <- renderText({
    df <- rerun_data()
    
    filtered_df <- 
      df %>%
      filter(
        between(success, input$success[1], input$success[2]),
        between(star, input$stars[1], input$stars[2]),
        between(tentacle, input$tentacles[1], input$tentacles[2]))
    
    paste0("The probabiilty of obtaining this set is ", 
           filtered_df %>% pull() %>% sum() %>% percent(accuracy = 0.01, scale = 100),
           "\n",
           "This set contains:", "\n",
           print_range(input$success[1], input$success[2], "Successes"), "\n",
           print_range(input$stars[1], input$stars[2], "Stars"), "\n",
           print_range(input$tentacles[1], input$tentacles[2], "Tentacles"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
