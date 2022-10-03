############################################

############################################

# Import libraries
library(shiny)
library(data.table)
library(randomForest)

# Read in the RF model
model <- readRDS("model.rds")


####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Iris Predictor'),
  
  # Input values
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
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
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
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)