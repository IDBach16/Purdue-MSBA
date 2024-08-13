rsconnect::setAccountInfo(name='idbach16', token='34577AD7325F7CC63F977C84D2F83C24', secret='KUovGrvdkLxxrRrd0ar406UexpaYVBs26PMivHzG')

library(rsconnect)
rsconnect::deployApp('C:/Users/ibach/OneDrive - Terillium/Desktop')

library(shiny)
library(ggplot2)
library(DT)

ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Number of observations", 
              value = 25, min = 1, max = 100),
  sliderInput(inputId = "sd", 
              label = "Standard Deviation", 
              value = 1, min = 0.1, max = 10),
  plotOutput("hist"),
  dataTableOutput("data_table")
)

server <- function(input, output) {
  data <- reactive({
    rnorm(input$num, mean = 0, sd = input$sd)
  })
  
  output$hist <- renderPlot({
    ggplot(data.frame(x = data()), aes(x = x)) +
      geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
      labs(title = "Histogram of Normal Random Variables", x = "Value", y = "Frequency")
  })
  
  output$data_table <- renderDataTable({
    datatable(data.frame(Value = data()))
  })
}

shinyApp(ui = ui, server = server)

