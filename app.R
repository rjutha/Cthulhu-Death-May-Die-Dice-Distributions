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
source("cthulhu dice.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

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
                        choices = c(0,
                                    1,
                                    2,
                                    3,
                                    4,
                                    5,
                                    6,
                                    7,
                                    8,
                                    9),
                        selected = 3),
            selectInput(inputId = "green",
                        label = "Number of green dice to roll",
                        choices = c(0,
                                    1,
                                    2,
                                    3,
                                    4,
                                    5,
                                    6,
                                    7,
                                    8,
                                    9),
                        selected = 0),
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
          highchartOutput("hc_plot")
           # plotOutput("distPlot")
          
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
    hchart(df,
           "column",
           hcaes(x = (1:length(dist)), y = dist),
           color = "#478A54") %>%
      hc_plotOptions(
        column = list(
          pointPadding = 0,
          borderWidth = 0,
          groupPadding = 0,
          shadow = FALSE
        )
      )
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
