library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("Example Table"),
  dataTableOutput("example_table")
)

server <- function(input, output) {
  # Define your data here as a data frame
  data <- data.frame(
    Name = c("Alice", "Bob", "Charlie"),
    Age = c(25, 30, 35),
    Gender = c("Female", "Male", "Male")
  )
  
  # Render the table using DT::renderDataTable
  output$example_table <- renderDataTable({
    datatable(data)
  })
}

shinyApp(ui, server)
