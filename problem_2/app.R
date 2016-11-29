library(shiny)
library(gapminder)
library(tidyverse)
library(ggthemes)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearInput", "Year", min = 1952, max = 2007, value = c(1960, 2000)
                  ),
      selectInput(inputId = "countryInput", label = "Country", choices = unique(gapminder$country)
                  )
      ),
    mainPanel(
      plotOutput("log10_plot"),
      tableOutput("selected_data")
    )
  )
)
server <- function(input, output, session) {
  log_df <- reactive({
    
    filter(
      gapminder, 
      country == input$countryInput, 
      year >= input$yearInput[1] & year <= input$yearInput[2]
    )
  })
  output$log10_plot <- renderPlot({
    ggplot(data = log_df(), 
           aes(log10(log_df()$gdpPercap), lifeExp)) + 
      geom_point() + ggtitle(input$countryInput)
  })
  output$selected_data <- renderTable({ 
    log_df()
  })
}
shinyApp(ui = ui, server = server)
