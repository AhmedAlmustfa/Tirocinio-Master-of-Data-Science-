library(shiny)
library(ggplot2)

## Modularizing the UI

plot_UI <- function(id, dataset) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(
          ns("select_var1"),
          "variable 1",
          choices = names(dataset)
        ),
        selectInput(
          ns("select_var2"),
          "variable 2",
          choices = names(dataset)
        ),
        actionButton(
          ns("draw_scatterplot"),
          "Draw scatterplot"
        )
      ),
      mainPanel(
        plotOutput(ns("scatterplot")),
        DT::dataTableOutput(ns("table"))
      )
    )
  )
}
# Modularizing server function
plot_server <- function(id, dataset) {
  moduleServer(
    id,
    function(input, output, session){
      output$scatterplot <- renderPlot({
        ggplot(dataset) + 
          geom_point(
            aes_string(input$select_var1, input$select_var2),
            size = 4,
            alpha = 0.5,
            col = "dodgerblue4"
          ) + 
          theme_minimal()
      }) |> bindEvent(input$draw_scatterplot)
      
      output$table <- DT::renderDataTable({
        dataset
      })
    }
  )
}

ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  tabsetPanel(
    tabPanel("Penguins", plot_UI("Penguin", palmerpenguins::penguins)),
    tabPanel("Iris",plot_UI("Iris", iris)),
    tabPanel("Diamonds", plot_UI("Diamonds", diamonds)),
    tabPanel("mpg",plot_UI("mpg", mpg)),
    tabPanel("mtcars",plot_UI("mtcars", mtcars))
  )
)

server <- function(input, output, session){
  plot_server("Penguin", palmerpenguins::penguins) 
  plot_server("Iris" , iris)
  plot_server("Diamonds", diamonds)
  plot_server("mpg", mpg)
  plot_server("mtcars", mtcars)
}

shinyApp(ui, server)