library(tidyverse)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    tabsetPanel(
        tabPanel(
            titlePanel("What is the Binomial?"),
            withMathJax(includeMarkdown("02-binomial.Rmd"))
        ),
        
        tabPanel(
            # Application title
            titlePanel("Visualizing Binomial Experiments"),
            
            # Sidebar with a slider input for number of bins 
            sidebarLayout(
                sidebarPanel(
                    selectInput("string",
                                "Inequality: ",
                                choices = c("<=", "<", "=", ">", ">=")
                    ),
                    numericInput("p",
                                 "Probability: ",
                                 value = 0.5,
                                 min = 0,
                                 max = 1
                    ),
                    numericInput("n",
                                 "Number of total trials",
                                 value = 10,
                                 min = 2,
                                 max = 100
                    ),
                    numericInput("x",
                                 "Number of Succesful events",
                                 value = 0,
                                 min = 0,
                                 max = 100
                    )
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                    plotOutput("distPlot")
                )
            )
        ),
        
        tabPanel(
            titlePanel("Example Problems"),
            withMathJax(includeMarkdown("problems.Rmd"))
        ),
        
        tabPanel(
            titlePanel("Example Solutions"),
            withMathJax(includeMarkdown("solutions.Rmd"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$formula <- renderUI({
        my_calculated_value <- 5
        withMathJax(paste0("Use this formula: $$\\hat{A}_{\\small{\\textrm{M€}}} =", my_calculated_value,"$$"))
    })
    
    output$distPlot <- renderPlot({
        
        n = 10
        x_i = 0:n
        p = 0.5
        x = 5
        string = ">="
        
        n = input$n 
        x = input$x 
        p = input$p 
        string = input$string
        
        x_i = 0:n
        col1 = "#8b0000"
        col2 = "#2d2d2d"
        
        color_who = function(str, target) {
            if (str == ">=") {
                res = ifelse(x_i >= target, col1, col2)    
                return(res)
            } else if (str == "<=") {
                res = ifelse(x_i <= target, col1, col2)    
                return(res)
            } else if (str == ">") {
                res = ifelse(x_i > target, col1, col2)    
                return(res)
            } else if (str == "<") {
                res = ifelse(x_i < target, col1, col2)    
                return(res)
            } else if (str == "=") {
                res = ifelse(x_i ==  target, col1, col2)    
                return(res)
            } else {
                res = "green" 
            }
        }
        
        probs = dbinom(x_i, n, p)
        cols = color_who(string, x)
        ans = sum(probs[cols == col1])
        
        data.frame(x_i = x_i, probs = probs) %>% 
            ggplot(aes(as.factor(x_i), probs)) +
            geom_bar(stat = "identity", fill = cols) +
            theme_minimal() +
            scale_x_discrete() +
            labs(x = "Value of X_i",
                 y = "Probability",
                 title = paste0("Binomial Distribution with x = ", x,
                                ", n = ", n, ", p = ", p),
                 subtitle = paste0("Formula: P(X", string, x, ") = ", ans)
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
