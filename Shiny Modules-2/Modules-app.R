
library(shiny)
library(shinythemes)
library(DT)
library(data.table)
library(randomForest)
library(ggplot2)

df <- read.csv("australian.csv")

ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage('FirstApp',
                           tabPanel("Random Forest Model",
                                    sidebarLayout(
                                      sidebarPanel(
                                        h2("Installation"),
                                        p("Random forest is available on CRAN, so you can install it in the usual way from your R console:"),
                                        code('install.packages("randomForest")'),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        img(src = "forest.png", height = 70, width = 200),
                                        br(),
                                        "RandomForest is a package of ", 
                                        span("RStudio", style = "color:blue")
                                      ),
                                      mainPanel(
                                        h1("Introducing Random Forest"),
                                        p("Random forest, like its name implies, consists of a large number of individual
                                           decision trees that operate as an", 
                                          em("ensemble. "), 
                                          "Each individual tree in the random forest spits out a class prediction and the
                                          class with the most votes becomes our model’s prediction."),
                                        br(),
                                        p("For an introduction and live examples, visit the ",
                                          a("Towards Data Science.", 
                                            href = "https://towardsdatascience.com/understanding-random-forest-58381e0602d2")),
                                        br(),
                                        h4("The reason that the random forest model works so well is:"),
                                        p("- A large number of relatively uncorrelated models (trees) operating as 
                                          a committee will outperform any of the individual constituent models."),
                                        
                                        p("The low correlation between models is the key Just like how investments 
                                           with low correlations like stocks and bonds come together to form a portfolio 
                                           that is greater than the sum of its parts, uncorrelated models can produce 
                                           ensemble predictions that are more accurate than any of the individual predictions.", 
                                          strong("The reason for this wonderful effect is that the trees protect each other from their 
                                           individual errors"), "(as long as they don’t constantly all err in the same direction). 
                                           While some trees may be wrong, many other trees will be right, so as a group the 
                                           trees are able to move in the correct direction. So the prerequisites for random 
                                           forest to perform well are: "),
                                        
                                        p("- There needs to be some actual signal in our features so that models built using 
                                           hose features do better than random guessing."),
                                        p("- The predictions (and therefore the errors) made by the individual trees need to 
                                           have low correlations with each other.")
                                      )
                                    )),
                  tabPanel('Visualization',rand_ui('mod_vis')),
                  tabPanel('Prediction',pred_ui('mod_pred')),
                  tabPanel("Downloading Data", "Blank"),

))

server <- function(input, output, session){
  model <- readRDS("model.rds")
  rand_server('mod_vis', df = df)
  pred_server('mod_pred', df = model)
}

shinyApp(ui, server)