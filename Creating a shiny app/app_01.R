####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(shinythemes)
library(DT)
library(data.table)
library(randomForest)

# Read in the RF model
model <- readRDS("model.rds")

# Load data ----
library(readr)
df <- read.csv("australian.csv")

# Source helper functions -----
#source("helpers.R")

  # Define UI
  ui <- fluidPage(theme = shinytheme("cerulean"),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "My first app",
      tabPanel("Random Forest Model",
        sidebarLayout(
               sidebarPanel(
        h2("Installation"),
        p("Shiny is available on CRAN, so you can install it in the usual way from your R console:"),
        code('install.packages("shiny")'),
        br(),
        br(),
        br(),
        br(),
        img(src = "rstudio.png", height = 70, width = 200),
        br(),
        "Shiny is a product of ", 
        span("RStudio", style = "color:blue")
      ),
      mainPanel(
        h1("Introducing Shiny"),
        p("Shiny is a new package from RStudio that makes it ", 
          em("incredibly easy "), 
          "to build interactive web applications with R."),
        br(),
        p("For an introduction and live examples, visit the ",
          a("Shiny homepage.", 
            href = "http://shiny.rstudio.com")),
        br(),
        h2("Features"),
        p("- Build useful web applications with only a few lines of code-no JavaScript required."),
        p("- Shiny applications are automatically 'live' in the same way that ", 
          strong("spreadsheets"),
          " are live. Outputs change instantly as users modify inputs, without requiring a reload of the browser.")
      )
      )),
      tabPanel("Visualization", 
               sidebarLayout(
                 
                 # Sidebar panel for inputs ----
                 sidebarPanel(
                   
                   # Input: Slider for the number of bins ----
                   # Select variable for y-axis
                   selectInput(inputId = "y", 
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
                   selectInput(inputId = "x", 
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
                   sliderInput(inputId = "alpha", 
                               label = "Alpha:", 
                               min = 0, max = 1, 
                               value = 0.5),
                   # Select variable for color
                   selectInput(inputId = "z", 
                               label = "Color by:",
                               choices = c("Y"),
                               selected = "Y")
                   
                 ),
                 
                 # Main panel for displaying outputs ----
                 mainPanel(
                   
                   # Output: Histogram ----
                   plotOutput(outputId = "scatterplot"),
                   textOutput(outputId = "correlation")
                   
                 )
               )), # Navbar 1, tabPanel
      tabPanel("Predicton Model", 
               sidebarLayout(
                 sidebarPanel(
                   
                   tags$label(h3('Input parameters')),
                   numericInput("X1", 
                                label = "X1", 
                                value = 1),
                   numericInput("X2", 
                                label = "X2", 
                                value = 29.6),
                   numericInput("X3", 
                                label = "X3", 
                                value = 1.75),
                   numericInput("X4", 
                                label = "X4", 
                                value = 2),
                   numericInput("X5", 
                                label = "X5", 
                                value = 4),
                   numericInput("X6", 
                                label = "X6", 
                                value = 4),
                   numericInput("X7", 
                                label = "X7", 
                                value = 1.25),
                   numericInput("X7.1", 
                                label = "X7.1", 
                                value = 0),
                   numericInput("X9", 
                                label = "X9", 
                                value = 1),
                   numericInput("X10", 
                                label = "X10", 
                                value = 11),
                   numericInput("X11", 
                                label = "X11", 
                                value = 1),
                   numericInput("X12", 
                                label = "X12", 
                                value = 2),
                   numericInput("X13", 
                                label = "X13", 
                                value = 280),
                   numericInput("X14", 
                                label = "X14", 
                                value = 1),
                   
                   actionButton("submitbutton", "Submit", 
                                class = "btn btn-primary")
                 ),
                 
                 mainPanel(
                   tags$label(h3('Status/Output')), # Status/Output Text Box
                   verbatimTextOutput('contents'),
                   tableOutput('tabledata') # Prediction results table
                 
               )
      )),
      tabPanel("Downloading Data", "Blank",
               
                   
                )
             
  
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output) {
    
    # Create scatterplot object the plotOutput function is expecting
    output$scatterplot <- renderPlot({
      ggplot(data = df, aes_string(x = input$x, y = input$y, color = input$z)) +
        geom_point(alpha = input$alpha)
    })
    
    # Create text output stating the correlation between the two ploted 
    output$correlation <- renderText({
      r <- round(cor(df[, input$x], df[, input$y], use = "pairwise"), 3)
      paste0("Correlation = ", r, ". Note: If the relationship between the two variables is not linear, the correlation coefficient will not be meaningful.")
    })
    # Input Data
    datasetInput <- reactive({  
      
      df <- data.frame(
        Name = c("X1",
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
        Value = as.character(c(input$X1,
                               input$X2,
                               input$X3,
                               input$X4,
                               input$X5,
                               input$X6,
                               input$X7,
                               input$X7.1,
                               input$X9,
                               input$X10,
                               input$X11,
                               input$X12,
                               input$X13,
                               input$X14)),
        stringsAsFactors = FALSE)
      
      Y <- 0
      df <- rbind(df, Y)
      input <- transpose(df)
      write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
      
      test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
      
      Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 2))
      print(Output)
      
    })
    
    # Status/Output Text Box
    output$contents <- renderPrint({
      if (input$submitbutton>0) { 
        isolate("Calculation complete.") 
      } else {
        return("Server is ready for calculation.")
      }
    })
    
    # Prediction results table
    output$tabledata <- renderTable({
      if (input$submitbutton>0) { 
        isolate(datasetInput()) 
      } 
    })
    

  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
