#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(highcharter)
library(tidyverse)
library(scales)
library(bslib)
library(shinythemes)
source("cthulhu dice.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    themeSelector(),
    # Application title
    titlePanel("Cthulhu Death May Die Dice Distribution"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "rolls",
                         label = "Number of Simulated Rolls",
                         choices = c("100" = 100,
                                     "1,000" = 1000,
                                     "10,000" = 10000,
                                     "100,000" = 100000),
                         selected = 10000),
            selectInput(inputId = "black",
                        label = "Number of black dice to roll",
                        choices = c(0:9),
                        selected = 3),
            selectInput(inputId = "green",
                        label = "Number of green dice to roll",
                        choices = c(0:9),
                        selected = 0),
          radioButtons("var", "Probability or Frequency?",
                       c("probability" = "percent",
                         "frequency" = "count")),
          sliderInput("success", "Number of Successes",
                      min = 0, 
                      max = 9,
                      value = c(0,9)),
          sliderInput("stars", "Number of Stars",
                      min = 0, 
                      max = 9,
                      value = c(0,9)),
          sliderInput("tentacles", "Number of Tentacles",
                      min = 0, 
                      max = 9,
                      value = c(0,9))
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
          highchartOutput("hc_plot"),
          textOutput("text")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  

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

  # output$distPlot <- renderPlot({
  #    df <- rerun_data()
  #       # generate bins based on input$bins from ui.R
  #       # draw the histogram with the specified number of bins
  #       print(plot(x = df$dist))
  #   })

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
             pointFormat = 
             "Number of Success: {point.success}<br>
             Number of Stars: {point.star}<br>
             Number of Tentacles: {point.tentacle}<br>
             Prob = {point.percent_label}")) %>%
      hc_plotOptions(
        column = list(
          pointPadding = 0,
          borderWidth = 0,
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
        title = list(text = "Roll Outcomes", style = list(color = 'white')),
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
           filtered_df %>% pull() %>% sum() %>% percent(accuracy = 0.01, scale = 100))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
