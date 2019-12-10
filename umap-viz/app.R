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
library(plotly)
library(umapviz)

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
                        max = 30,
                        value = 2),
            sliderInput("neighbors",
                        "n-Neighbors",
                        min = 5,
                        max = 50,
                        value = 15),
            sliderInput("mindist",
                        "Minimum Distance",
                        min = 0.001,
                        max = 0.5,
                        value = 0.1),
              radioButtons(inputId = "distance", label="Distance", choices = c("euclidean","cosine","manhattan"), selected="euclidean")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
          # plotOutput("scatterPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        iris<-(datasets::iris)
       iris.data = iris[, grep("Sepal|Petal", colnames(iris))]
       iris.labels = iris[, "Species"]
       custom.config = umap.defaults 
       custom.config$n_components=input$dims #2
       custom.config$min_dist=input$mindist #0.1
       custom.config$n_neighbors=input$neighbors #15
       custom.config$metric=input$distance #euclidean
       library(umap)
       iris.umap = umap(iris.data,custom.config)
       plot.iris(iris.umap, iris.labels)
       
    })
    output$scatterPlot <- renderPlot({
        iris<-(datasets::iris)
        ggplot(iris, aes(x = Petal.Length, y = Sepal.Length, colour = Species)) + 
            geom_point() +
            ggtitle('Iris Species by Petal and Sepal Length')
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
