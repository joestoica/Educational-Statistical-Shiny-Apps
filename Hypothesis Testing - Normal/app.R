library(tidyverse)
library(shiny)

ui <- fluidPage(
    theme = "theme.css",
    navbarPage(
        "Hypothesis Testing - Normal Distribution",
        tabPanel(
            "Overview",
            withMathJax(includeMarkdown("breakdown.Rmd"))
        ),
        tabPanel(
            "Visualizing Hypothesis Tests",
            sidebarLayout(
                sidebarPanel(
                    sliderInput("conf",
                                "Confidence Level",
                                min = 0,
                                max = 1,
                                value = 0.95
                    ),
                    sliderInput("xbar",
                                "Observed Sample Mean (xbar)",
                                value = 10,
                                min = -25,
                                max = 25
                    ),
                    sliderInput("mu",
                                "Population mean (mu): ",
                                value = 10,
                                min = -25,
                                max = 25
                    ),
                    selectInput("sign",
                                "Sign for Null Hypothesis",
                                choices = c("=","<=",">="),
                                multiple = FALSE
                    ),
                    sliderInput("sigma",
                                "Population Standard Deviation (sigma)",
                                value = 5,
                                min = 1,
                                max = 10
                    ),
                    sliderInput("n",
                                "Sample Size (n)",
                                value = 30,
                                min = 2,
                                max = 100
                    ),
                    img(src = "301.png", align = "center", width="100%"),
                ),
                mainPanel(div(textOutput("null"), align = "justify"),
                          div(textOutput("alt"), align = "justify"),
                          plotOutput("normal"))
            ),
        ),
        tabPanel(
            "Example Problems",
            withMathJax(includeMarkdown("problems.Rmd"))
        )
    )    
)

set.seed(301)

server <- function(input, output) {
    
    output$null = renderText({
        
        sign = input$sign
        mu = input$mu
        
        if (sign == "=") {
            paste0("H0: mu ", sign, mu)
        } else if (sign == "<=") {
            paste0("H0: mu ", sign, " ", mu)
        } else {
            paste0("H0: mu ", sign, " ", mu)
        }
        
    })
    
    output$alt = renderText({
        
        sign = input$sign
        mu = input$mu
        
        if (sign == "=") {
            paste0("H1: mu != ", mu)
            
        } else if (sign == "<=") {
            paste0("H1: mu > ", mu)
        } else {
            paste0("H1: mu < ", mu)
        }
        
    })
    
    # Top plot in the first visual page of app
    output$normal <- renderPlot({
        
        mu = 10
        xbar = 10
        sigma = 2
        alpha = 0.05
        n = 30
        sign = "<="
        
        # Getting inputs from UI code
        conf = input$conf
        xbar = input$xbar
        mu = input$mu
        sign = input$sign
        sigma = input$sigma
        n = input$n
        
        alpha = 1 - conf
        
        # calculate SE
        se = sigma / sqrt(n)
        
        # Calculate me
        me = qnorm(1 - alpha/2) * se
        
        # Confidence Interval
        upper = mu + me
        lower = mu - me
        
        # This is the upper and lower portion of the geom_dist that we want to 
        # draw on the ggplot to make sure everything is visualized nicely
        upper_draw = mu + 4 * se
        lower_draw = mu - 4 * se
        
        # This makes the breaks on the x-axis scaled to match the ME 
        x_axis_breaks = round(seq(lower - 3*me, upper + 3 * me, me), 2)
        
        # Calculate the data to draw geom_dist() with
        x = seq(lower_draw, upper_draw, 0.01)
        # use the normal density to draw the shape
        y = dnorm(x, mu, se)
        
        # drawing correct rejection region
        if (sign == ">=") { #lower
            rr_cv = qnorm(alpha, mu, se)
        } else { #upper
            rr_cv = qnorm(1-alpha, mu, se)
        }
                
        # temp dataframe to draw ggplot with
        df = data.frame(x, y)
        if(sign == "="){
            df %>% 
                ggplot(aes(x,y)) +
                
                # fills the tail(s) pertaining to the hypothesis
                geom_area(data = df %>% filter(x <= lower),
                          mapping = aes(x), 
                          fill = "red", alpha = 0.5)+
                
                geom_area(data = df %>% filter(x >= upper),
                          mapping = aes(x), 
                          fill = "red", alpha = 0.5)+
                # lower CV line
                geom_segment(aes(x = lower, y = 0, 
                                 xend = lower, yend = dnorm(lower, mu, se))) +
                # upper CV line
                geom_segment(aes(x = upper, y = 0, 
                                 xend = upper, yend = dnorm(upper, mu, se))) +
                # xbar line
                geom_vline(xintercept =  xbar, color = "blue") +
                theme_minimal() +
                # draw the density plot
                geom_line() +
                geom_hline(yintercept = 0) +
                # scale x_axis
                scale_x_continuous(breaks = x_axis_breaks,
                                   limits = c(lower_draw, upper_draw))+
                # shorten y-axis, ran into some errors without this.
                ylim(c(0, max(y)+0.1)) +
                labs(x = "",
                     y = "")
        }else if(sign == "<="){
            df %>% ggplot(aes(x,y)) +
                
                # fills the tail(s) pertaining to the hypothesis
                geom_area(data = df %>% filter(x >= rr_cv),
                          mapping = aes(x), 
                          fill = "red", alpha = 0.5)+
                # lower CV line
                geom_segment(aes(x = rr_cv, y = 0, 
                                 xend = rr_cv, yend = dnorm(rr_cv, mu, se))) +
                # Population mean line
                geom_vline(xintercept =  xbar, color = "blue") +
                theme_minimal() +
                # draw the density plot
                geom_line() +
                geom_hline(yintercept = 0) +
                # scale x_axis
                scale_x_continuous(breaks = x_axis_breaks,
                                   limits = c(lower_draw, upper_draw))+
                # shorten y-axis, ran into some errors without this.
                ylim(c(0, max(y)+0.1)) +
                labs(x = "",
                     y = "")
        } else {
            df %>% ggplot(aes(x,y)) +
                
                # fills the tail(s) pertaining to the hypothesis
                geom_area(data = df %>% filter(x <= rr_cv),
                          mapping = aes(x), 
                          fill = "red", alpha = 0.5)+
                # upper CV line
                geom_segment(aes(x = rr_cv, y = 0, 
                                 xend = rr_cv, yend = dnorm(rr_cv, mu, se))) +
                geom_vline(xintercept =  xbar, color = "blue") +
                theme_minimal() +
                # draw the density plot
                geom_line() +
                geom_hline(yintercept = 0) +
                # scale x_axis
                scale_x_continuous(breaks = x_axis_breaks,
                                   limits = c(lower_draw, upper_draw))+
                # shorten y-axis, ran into some errors without this.
                ylim(c(0, max(y)+0.1)) +
                labs(x = "",
                     y = "")
        }
    } 
    )
    
    output$Description = renderText({
        
        sign = input$sign
        mu = input$mu
        input
        
        
        if (sign == "=") {
            paste0("H1: mu != ", mu)
            
        } else if (sign == "<=") {
            paste0("H1: mu > ", mu)
        } else {
            paste0("H1: mu < ", mu)
        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
