#James Valles - Interactive app. Dataset is cat.csv, ensure the path is correct on line 8 before loading app. 
# You can launch the interactive app by visiting the following address: https://jamesvalles.shinyapps.io/Corona-19Stats/  

library(shiny)
library(readxl)
library(datasets)

#Import Covid19 Dataset
Coviddata<- read.csv("cat.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(    
    
    # Give the page a title
    titlePanel("COVID-19 Stats by Country"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
        
        # Define the sidebar with one input
        sidebarPanel(
            selectInput("Country", "Country:", 
                        choices=colnames(Coviddata)),
            hr(),
            helpText("This chart contains the latest number of deaths, patients in critical and serious condition, and those that have recovered. Source: BNO News ")
        ),
        
        # Create a spot for the barplot
        mainPanel(
            plotOutput("covidPlot")  
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Fill in the spot we created for a plot
    output$covidPlot <- renderPlot({ 
        
        # Render a barplot
        barplot(Coviddata[,input$Country],
                main=input$Country,
                ylab="Number",
                xlab="Current Statistics as of March 11, 2020",  names.arg = c("Death", "Serious", "Critical", "Recovered"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
