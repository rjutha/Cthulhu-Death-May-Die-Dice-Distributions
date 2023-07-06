library(shiny)
library(highcharter)
library(tidyverse)
library(scales)
library(shinythemes)

source("cthulhu dice.R")

ui <- fluidPage(
    theme = shinytheme("darkly"),
    includeCSS("styles.css"),
    titlePanel("Cthulhu Death May Die: Dice Distribution Visualizer"),

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
          uiOutput("success"),
          uiOutput("stars"),
          uiOutput("tentacles"),
        ),
        
        mainPanel(
          highchartOutput("hc_plot"),
          h3(verbatimTextOutput("text"))
        )
    )
)

server <- function(input, output) {
  
  value <- reactive({
    black <- as.numeric(input$black)
    green <- as.numeric(input$green)
    sum(black,green)
  })
  
  output$success <- renderUI({
    sliderInput("success", "Filter number of Successes.", min = 0, max = value(), value = c(0, value()), step = 1)
  })
  output$stars <- renderUI({
    sliderInput("stars", "Filter number of Elder Signs.", min = 0, max = value(), value = c(0, value()), step = 1)
  })
  output$tentacles <- renderUI({
    sliderInput("tentacles", "Filter number of Tentacles.", min = 0, max = as.numeric(input$black), value = c(0, as.numeric(input$black)), step = 1)
  })
  
  observe({
    freezeReactiveValue(input, "success")
    updateSliderInput(inputId = "success", min = 0, max = value(), value = c(0, value()))
    updateSliderInput(inputId = "stars", min = 0, max = value(), value = c(0, value()))
    updateSliderInput(inputId = "tentacles", min = 0, max = as.numeric(input$black), value = c(0, as.numeric(input$black)))
  }, priority = 50)
  
  rerun_data <- reactive({
      roll_n(as.numeric(input$rolls),input$black, input$green) %>%
      as_tibble() %>%
      group_by_all() %>%
      count(name = "count") %>%
      ungroup() %>%
      arrange(-count) %>%
      mutate(dist = count / as.numeric(input$rolls))
  })
  
  output$hc_plot <- renderHighchart({
    df <- rerun_data()
    
    df <- df %>%
      filter(
        between(success, input$success[1], input$success[2]),
        between(star, input$stars[1], input$stars[2]),
        between(tentacle, input$tentacles[1], input$tentacles[2])) %>%
      mutate(
        percent = dist * 100,
        percent_label = percent(dist, accuracy = 0.01, scale = 100))
    
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
               "Probability of event: {point.percent_label}"))) %>%
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
           print_range(input$success[1], input$success[2], "Successes", value()), "\n",
           print_range(input$stars[1], input$stars[2], "Elder Signs", value()), "\n",
           print_range(input$tentacles[1], input$tentacles[2], "Tentacles", as.numeric(input$black)))
  })
}

shinyApp(ui = ui, server = server)
