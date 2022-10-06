library(shiny)
library(ggplot2)


ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  tabsetPanel(
    tabPanel("Penguins",{
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "select_var1_penguins",
            "variable 1",
            choices = names(palmerpenguins::penguins)
          ),
          selectInput(
            "select_var2_penguins",
            "variable 2",
            choices = names(palmerpenguins::penguins)
          ),
          actionButton(
            "draw_scatterplot_penguins",
            "Draw scatterplot"
          )
        ),
        mainPanel(
          plotOutput("scatterplot_penguins"),
          DT::dataTableOutput("table_1")
        )
      )
    }),
    
    tabPanel("Iris",{
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "select_var_1_iris",
            "Variable 1",
            choices = names(iris)
          ),
          selectInput(
            "select_var_2_iris",
            "Variable 2",
            choices = names(iris)
          ),
          actionButton(
            "draw_scatterplot_iris",
            "Draw scatterplot"
          )
        ),
        mainPanel(
          plotOutput("scatterplot_iris"),
          DT::dataTableOutput("table_2")
        )
      )
    }),
    tabPanel("mtcars",{
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "select_var_1_mtcars",
            "Variable 1",
            choices = names(mtcars)
          ),
          selectInput(
            "select_var_2_mtcars",
            "Variable 2",
            choices = names(mtcars)
          ),
          actionButton(
            "draw_scatterplot_mtcars",
            "Draw scatterplot"
          )
        ),
        mainPanel(
          plotOutput("scatterplot_mtcars"),
          DT::dataTableOutput("table_3")
        )
      )
    })
  )

)


server <- function(input, output, session){
  output$scatterplot_penguins <- renderPlot({
    ggplot(palmerpenguins::penguins) + 
      geom_point(
        aes_string(input$select_var1_penguins, input$select_var2_penguins),
        size = 3,
        alpha = 0.5,
        col = "dodgerblue4"
      )  + 
    theme_minimal()
  }) |> bindEvent(input$draw_scatterplot_penguins)
  
  output$table_1 <- DT::renderDataTable({
    palmerpenguins::penguins
  })
  
  output$scatterplot_iris <- renderPlot({
    ggplot(iris) +
      geom_point(
        aes_string(input$select_var_1_iris, input$select_var_2_iris),
        size = 3,
        alhpa = 0.5,
        col = "dodgerblue4"
      )    + 
    theme_minimal()
  })|> bindEvent(input$draw_scatterplot_iris)
  
  output$table_2 <- DT::renderDataTable({
    iris
  })
  
  output$scatterplot_mtcars <- renderPlot({
    ggplot(mtcars) +
      geom_point(
        aes_string(input$select_var_1_mtcars, input$select_var_2_mtcars),
        size = 3,
        alhpa = 0.5,
        col = "dodgerblue4"
      )   + 
    theme_minimal()
  })|> bindEvent(input$draw_scatterplot_mtcars)
  
  output$table_3 <- DT::renderDataTable({
    mtcars
  })
}

shinyApp(ui, server)