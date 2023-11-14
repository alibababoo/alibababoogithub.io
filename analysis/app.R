
library(shiny)
library(ggplot2)
library(reshape2)

# Reading the CSV file (replace "swiftdata.csv" with your file name)
swiftdata <- read.csv("swiftdata.csv")

# UI
ui <- fluidPage(
  titlePanel("Boxplot analysis of the different attributes of Taylor Swift's songs"),
  sidebarLayout(
    sidebarPanel(
      selectInput("attribute", "Select Attribute", 
                  choices = c("acousticness", "danceability", "energy", "liveness", "tempo", "valence"),
                  selected = "acousticness")
    ),
    mainPanel(
      plotOutput("boxplot")
    )
  )
)

# Server
server <- function(input, output) {
  output$boxplot <- renderPlot({
    # Subset data based on selected attribute
    selected_attr <- input$attribute
    melted_data <- melt(swiftdata[, selected_attr])
    
    # Create box plot
    ggplot(melted_data, aes(x = 1, y = value)) +
      geom_boxplot() +
      geom_jitter(color="pink", size=2, alpha=1) +
      labs(x = "songs", y = selected_attr, title = paste("", selected_attr)) +
      scale_x_discrete(breaks = NULL) + # Removes x-axis values
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
