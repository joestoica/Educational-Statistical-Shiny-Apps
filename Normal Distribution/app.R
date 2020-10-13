library(tidyverse)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    tabsetPanel(
        
        tabPanel(
            titlePanel("Overview"),
            withMathJax(includeMarkdown("normal.Rmd"))
        ),
        
        tabPanel(
            # Application title
            titlePanel("Visualizing the Normal Distribution"),
            
            # Sidebar with a slider input for number of bins 
            sidebarLayout(
                sidebarPanel(
                    selectInput("sign",
                                "Inequality: ",
                                choices = c("<",">")
                    ),
                    numericInput("xbar",
                                 "Observed Value",
                                 value = 9,
                    ),
                    numericInput("mu",
                                 "Mu: ",
                                 value = 10,
                    ),
                    numericInput("sigma",
                                 "Standard Deviation",
                                 value = 2,
                                 min = 0
                    ),
                    
                ),
                # Show a plot of the generated distribution
                mainPanel(
                    textOutput("text"),
                    tags$head(tags$style("#text{color: black;
                                 font-size: 32px;
                                 }"
                    )),
                    plotOutput("normal"),
                    plotOutput("standard_normal")
                )
            ),
            
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
    
    output$text <- renderText(
        ifelse(input$sign == "<", 
               paste0("P(X ", input$sign, " ", input$xbar, ") = ", round(pnorm(input$xbar, input$mu, input$sigma), 4)),
               paste0("P(X ", input$sign, " ", input$xbar, ") = ", "1 - P(X < ", input$xbar, " ) = 1 - ",  round(pnorm(input$xbar, input$mu, input$sigma), 4), " = ", 1 - round(pnorm(input$xbar, input$mu, input$sigma), 4))
               )
    )
    
    output$normal <- renderPlot({
        
        xbar = input$xbar
        mu = input$mu
        sigma = input$sigma
        z = (xbar - mu) / sigma
        sign = input$sign
        
        upper = mu + 4*sigma
        lower = mu - 4*sigma
        
        x = seq(lower, upper, 0.01)
        y = dnorm(x, mu, sigma)
        
        
        if (sign == ">") {
            # upper
            data.frame(x,y) %>% 
                ggplot(aes(x,y)) +
                geom_area(mapping = aes(x = ifelse(x >= xbar, x, upper)), 
                          fill = "red", alpha = 0.5) +
                geom_segment(aes(x = xbar, y = 0, 
                                 xend = xbar, yend = dnorm(xbar, mu, sigma)),
                             color = "blue") +
                geom_segment(aes(x = mu, y = 0, 
                                 xend = mu, yend = dnorm(mu, mu, sigma))) +
                theme_minimal() +
                geom_line() +
                geom_hline(yintercept = 0) +
                scale_x_continuous(breaks = seq(lower, upper, sigma)) +
                ylim(c(0, max(y))) +
                labs(title = paste0("Normal Distribution with xbar = ", xbar, ", mean=", mu, ", and sigma = ", sigma))
            
        } else if (sign == "<") {
            # lower
            data.frame(x,y) %>% 
                ggplot(aes(x,y)) +
                geom_area(mapping = aes(x = ifelse(x <= xbar, x, lower)), 
                          fill = "red", alpha = 0.5) +
                geom_segment(aes(x = xbar, y = 0, 
                                 xend = xbar, yend = dnorm(xbar, mu, sigma)),
                             color = "blue") +
                geom_segment(aes(x = mu, y = 0, xend = mu, yend = dnorm(mu, mu, sigma))) +
                theme_minimal() +
                geom_line() +
                geom_hline(yintercept = 0) +
                scale_x_continuous(breaks = seq(lower, upper, sigma))+
                ylim(c(0, max(y))) +
                labs(title = paste0("Normal Distribution with xbar = ", xbar, ", mean=", mu, ", and sigma = ", sigma))
            
        }
    })
    
    output$standard_normal <- renderPlot({
        
        xbar = input$xbar
        mu = input$mu
        sigma = input$sigma
        z = (xbar - mu) / sigma
        sign = input$sign
        
        # create the upper and lower bounds that will be displayed (visual purposes only)
        upper = 4
        lower = -4
        
        # create the normal density
        x = seq(lower, upper, 0.01)
        y = dnorm(x, 0, 1)
        
        
        if (sign == ">") {
            # upper
            data.frame(x,y) %>% 
                ggplot(aes(x,y)) +
                
                # highlight the probability region
                geom_area(mapping = aes(x = ifelse(x >= z, x, upper)), 
                          fill = "red", alpha = 0.5) +
                
                # This draws a line for the z score
                geom_segment(aes(x = z, y = 0, 
                                 xend = z, yend = dnorm(z, 0, 1)),
                             color = "blue") +
                
                # this draws a line at the mean (standard normal = 0)
                geom_segment(aes(x = 0, y = 0, 
                                 xend = 0, yend = dnorm(0, 0, 1))) +
                theme_minimal() +
                
                # draws the density
                geom_line() +
                
                # just a line at 0 for visual purposes
                geom_hline(yintercept = 0) +
                
                # limit y axis to height of distribution
                ylim(c(0, max(y))) +
                
                labs(title = paste0("Standard Normal Distribution with z = ", "(", xbar, " - ", mu, ") / ", sigma, " = ", round(z,2)))+
                scale_x_continuous(breaks = lower:upper)
        } else if (sign == "<") {
            # lower
            
            data.frame(x,y) %>% 
                ggplot(aes(x,y)) +
                geom_area(mapping = aes(x = ifelse(x <= z, x, lower)), 
                          fill = "red", alpha = 0.5) +
                geom_segment(aes(x = z, y = 0, 
                                 xend = z, yend = dnorm(z, 0, 1)),
                             color = "blue") +
                geom_segment(aes(x = 0, y = 0, 
                                 xend = 0, yend = dnorm(0, 0, 1))) +
                theme_minimal() +
                geom_line() +
                geom_hline(yintercept = 0) +
                ylim(c(0, max(y))) +
                labs(title = paste0("Standard Normal Distribution with z = ", "(", xbar, " - ", mu, ") / ", sigma, " = ", round(z,2)))+
                scale_x_continuous(breaks = lower:upper)
            
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
