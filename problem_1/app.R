library(shiny)
library(babynames)
library(tidyverse)
library(ggthemes)
library(stringr)

babynames$year <- as.integer(babynames$year)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearInput", "Year", min = 1880, max = 2014, value = c(1950, 2000)),
      textInput("nameInput", "Name", value = "Chris"),
      radioButtons("sexID", "Sex",  choices = c("Female only", "Male only", "Both"), selected = "Both")),
    mainPanel(
      plotOutput("main_plot"),
      tableOutput("results")
    )
  ),
  titlePanel("Baby Names")
)

server <- function(input, output, session) {
  reduced_df <- reactive({
    sex_vec <- switch(input$sexID,
                      `Female only` = "F",
                      `Male only` = "M",
                      Both = c("F", "M")
    )
    filter(
      babynames, 
      name == input$nameInput, 
      year >= input$yearInput[1] & year <= input$yearInput[2], 
      sex %in% sex_vec 
    )
  })

  output$main_plot <- renderPlot({
    ggplot(data = reduced_df(), 
           aes(year, n, colour = sex)) + 
      geom_line() + ggtitle(input$nameInput) + scale_color_manual(values = c(F = "Red", M = "Blue")) + theme_bw()
  })
  output$results <- renderTable({ 
    spread(select(reduced_df(), -prop), sex, n)
  })
}

shinyApp(ui = ui, server = server)