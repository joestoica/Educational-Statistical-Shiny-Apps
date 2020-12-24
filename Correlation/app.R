library(tidyverse)
library(shiny)

ui <- fluidPage(
    theme = "theme.css",
    navbarPage(
        "Correlation",
        tabPanel(
            "Overview",
            withMathJax(includeMarkdown("breakdown.Rmd"))
        ),
        tabPanel(
            "Visualizing Hypothesis Tests",
            sidebarLayout(
                sidebarPanel(
                    numericInput("rho",
                                 "Correlation",
                                 min = -1,
                                 max = 1,
                                 value = 0
                    ),
                    img(src = "301.png", align = "center", width="100%"),
                ),
                mainPanel(
                    plotOutput("plot"),
                )
            ),
        )
    )    
)

set.seed(301)

server <- function(input, output) {
    
    # Top plot in the first visual page of app
    output$plot <- renderPlot({
        
        rho = 0.5
        rho = input$rho
        mu <- c(0, 0)
        rmat = matrix(c(1, rho, rho, 1), nrow = 2, ncol = 2)
        
        mat <- MASS::mvrnorm(100, Sigma = rmat, mu = mu, empirical = TRUE) 
        df = data.frame(mat)
        
        df %>% 
            ggplot(aes(X1, X2)) +
            geom_point() + 
            theme_minimal() +
            labs(x = "X",
                 y = "Y") +
            geom_label(x=0, y=2, 
                       label = paste("R = ", round(cor(df$X1, df$X2), 2),
                                     ", R^2 = ", round(cor(df$X1, df$X2)^2, 2)),
                       size=5, color = "red") +
            xlim(c(-3,3)) + 
            ylim(c(-3,3))
    })
} 

shinyApp(ui = ui, server = server)
