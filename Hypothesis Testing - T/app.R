library(tidyverse)
library(shiny)

ui <- fluidPage(
    theme = "theme.css",
    navbarPage(
        "Hypothesis Testing",
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
                    sliderInput("s",
                                "Sample Standard Deviation (s)",
                                value = 5,
                                min = 1,
                                max = 10
                    ),
                    
                    selectInput("sign",
                                "Sign for Null Hypothesis",
                                choices = c("=","<=",">="),
                                multiple = FALSE
                    ),
                    
                    sliderInput("n",
                                "Sample Size (n)",
                                value = 30,
                                min = 2,
                                max = 100
                    ),
                    img(src = "301.png", align = "center", width="100%"),
                ),
                mainPanel(div(textOutput("null"), align = "justify", style="background-color: white;"),
                          div(textOutput("alt"), align = "justify", style="background-color: white;"),
                          plotOutput("normal"),
                          div("", style="padding-top: 16px; background-color: white;"),
                          div(textOutput("stat"), align = "justify", style="background-color: white;"),
                          div(textOutput("cv"), align = "justify", style="background-color: white;"),
                          div(textOutput("alpha"), align = "justify", style="background-color: white;"),
                          div(textOutput("pval"), align = "justify", style="background-color: white;"))
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
        
        # Getting inputs from UI code
        conf = input$conf
        xbar = input$xbar
        mu = input$mu
        sign = input$sign
        s = input$s
        n = input$n
        alpha = 1 - conf
        
        # calculate SE
        se = s / sqrt(n)
        z = (xbar - mu) / se
        
        # Calculate me
        me = qt(1 - alpha/2, n-1)
        
        # Confidence Interval
        upper = me
        lower = -me
        
        # This is the upper and lower portion of the geom_dist that we want to 
        # draw on the ggplot to make sure everything is visualized nicely
        upper_draw = 4 * me
        lower_draw = -4 * me
        
        # This makes the breaks on the x-axis scaled to match the ME 
        x_axis_breaks = round(seq(lower_draw, upper_draw, me), 2)
        
        # Calculate the data to draw geom_dist() with
        x = seq(lower_draw, upper_draw, 0.01)
        
        y = dt(x, n-1)
        
        # drawing correct rejection region
        if (sign == ">=") { #lower
            rr_cv = qt(alpha, n-1)
        } else { #upper
            rr_cv = qt(1-alpha, n-1)
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
                                 xend = lower, yend = dt(lower, n-1))) +
                # upper CV line
                geom_segment(aes(x = upper, y = 0, 
                                 xend = upper, yend = dt(upper, n-1))) +
                # xbar line
                geom_vline(xintercept = z, color = "blue") +
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
                                 xend = rr_cv, yend = dt(rr_cv, n-1))) +
                # Population mean line
                geom_vline(xintercept = z, color = "blue") +
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
                                 xend = rr_cv, yend = dt(rr_cv, n-1))) +
                geom_vline(xintercept = z, color = "blue") +
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
    
    output$stat = renderText({
        
        xbar = input$xbar
        mu = input$mu
        s = input$s
        n = input$n
        
        # calculate SE
        se = s / sqrt(n)
        z = (xbar - mu) / se
        
        paste0("z = (", xbar, "-", mu,") / (", s, " / ", "sqrt(", n, ") = ", round(z, 2))
        
    })
    
    output$cv = renderText({
        
        n = input$n 
        sign = input$sign
        conf = input$conf
        alpha = 1 - conf
        
        if (sign == "=") {
            cv = qt(1-alpha/2, n-1)
            cv = round(cv, 2)
            paste0("Critical values: (", -cv, ", ", cv, ")")
        } else if (sign == ">=") { #lower
            cv = qt(alpha, n-1)
            cv = round(cv, 2)
            paste0("Critical value: ", cv)
        } else { #upper
            cv = qt(1-alpha, n-1)
            cv = round(cv, 2)
            paste0("Critical value: ", cv)
        }
        
    })
    
    output$alpha = renderText({
        
        conf = input$conf
        alpha = 1 - conf
        paste0("alpha = ", alpha)
        
    })
    
    output$pval = renderText({
        
        xbar = input$xbar
        mu = input$mu
        s = input$s
        n = input$n
        sign = input$sign
        
        # calculate SE
        se = s / sqrt(n)
        z = (xbar - mu) / se
        
        if (sign == "=") {
            p = 2 * (1 - pt(abs(z), n -1))
        } else if (sign == "<=") {
            p = 1-pt(z, n -1)
        } else {
            p = pt(z, n - 1)
        }
        
        paste0("p-value = ", round(p, 4))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
