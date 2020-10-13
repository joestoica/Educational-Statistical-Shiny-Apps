library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    tabsetPanel(
        
        tabPanel(
            "Overview",
            withMathJax(includeMarkdown("breakdown.Rmd"))
        ),
        
        tabPanel(
            "What App is this",
            
            sidebarLayout(
                sidebarPanel(
                    # enter inputs here
                ),
                
            ),
            tabPanel(
                "Example Problems",
                withMathJax(includeMarkdown("practice_problems.Rmd"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$plot <- renderPlot({
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
