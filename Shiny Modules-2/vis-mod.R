
rand_ui <- function(id, df){
  
  ns <- NS(id)
  
  tagList(
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      # Select variable for y-axis
      selectInput(inputId = ns("y"), 
                  label = "Y-axis:",
                  choices = c("X1",
                              "X2",
                              "X3",
                              "X4",
                              "X5",
                              "X6",
                              "X7",
                              "X7.1",
                              "X9",
                              "X10",
                              "X11",
                              "X12",
                              "X13",
                              "X14"), 
                  selected = "X7"),
      
      # Select variable for x-axis
      selectInput(inputId = ns("x"), 
                  label = "X-axis:",
                  choices = c("X1",
                              "X2",
                              "X3",
                              "X4",
                              "X5",
                              "X6",
                              "X7",
                              "X7.1",
                              "X9",
                              "X10",
                              "X11",
                              "X12",
                              "X13",
                              "X14"), 
                  selected = "X2"),
      
      # Set alpha level
      sliderInput(inputId = ns("alpha"), 
                  label = "Alpha:", 
                  min = 0, max = 1, 
                  value = 0.5),
      # Select variable for color
      selectInput(inputId = ns("z"), 
                  label = "Color by:",
                  choices = c("Y"),
                  selected = "Y")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = ns("scatterplot")),
      textOutput(outputId = ns("correlation"))
      
    )
  )) # Navbar 1, tabPanel
}

rand_server <- function(id,df){
  
  moduleServer(id, function(input, output, session){
    output$scatterplot <- renderPlot({
      ggplot(data = df, aes_string(x = input$x, y = input$y, color = input$z)) +
        geom_point(alpha = input$alpha)
    })
    
    # Create text output stating the correlation between the two ploted 
    output$correlation <- renderText({
      r <- round(cor(df[, input$x], df[, input$y], use = "pairwise"), 3)
      paste0("Correlation = ", r, ". Note: If the relationship between the two variables is not linear, the correlation coefficient will not be meaningful.")
    })
  })
}