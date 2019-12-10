#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(datasets)
data(iris)
library(shiny)
library(umap)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("UMAP Projection"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("dims",
                        "Number of PCA Dims",
                        min = 1,
                        max = 50,
                        value = 10),
            sliderInput("neighbors",
                        "n-Neighbors",
                        min = 5,
                        max = 50,
                        value = 30),
            sliderInput("mindist",
                        "Minimum Distance",
                        min = 0.001,
                        max = 0.5,
                        value = 0.2),
              radioButtons(inputId = "distance", label="Distance", choices = c("Euclidean","Cosine","Manhattan"), selected="Euclidean")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        iris<-(datasets::iris)
       iris.data = iris[, grep("Sepal|Petal", colnames(iris))]
       iris.labels = iris[, "Species"]
       
       library(umap)
       iris.umap = umap(iris.data)
       plot.iris(iris.umap, iris.labels)
       
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
