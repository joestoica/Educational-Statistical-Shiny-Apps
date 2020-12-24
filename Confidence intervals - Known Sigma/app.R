library(tidyverse)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = "theme.css",
    navbarPage(
        "Confidence Intervals",
        tabPanel(
            "Overview",
            withMathJax(includeMarkdown("breakdown.Rmd"))
        ),
        tabPanel(
            "Visualizing Confidence Intervals",
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
                mainPanel(
                    plotOutput("normal"),
                    plotOutput("standard_normal"))
            ),
        ),
        tabPanel(
            # Application title
            "Confidence Intervals",
            
            # Sidebar with a slider input for number of bins 
            sidebarLayout(
                sidebarPanel(
                    numericInput("mean",
                                 "Mean:",
                                 value = 0),
                    numericInput("stdev",
                                 "Standard Deviation:",
                                 value = 1,
                                 min = 1e-6),
                    numericInput("n_interval",
                                 "Number of Intervals:",
                                 value = 10,
                                 min = 1,
                                 max = 100),
                    sliderInput("conf_lvl",
                                "Confidence Level",
                                value = 0.95,
                                min = 0, max = 1),
                    numericInput("ci_n",
                                 "Sample Size",
                                 value = 30),
                    selectInput("int_type",
                                "Do you know the population SD?",
                                choices = c("Yes", "No"),
                                multiple = FALSE,
                    ),
                    img(src = "301.png", align = "center", width="100%")
                ),
                mainPanel(plotOutput("CI_plot")),
            )
        ),
        tabPanel(
            "Example Problems",
            withMathJax(includeMarkdown("problems.Rmd"))
        )
    )    
)

server <- function(input, output) {
    
    output$normal <- renderPlot({
        
        conf = input$conf
        xbar = input$xbar
        mu = input$mu
        sigma = input$sigma
        n = input$n
        se = sigma / sqrt(n)
        me = qnorm(1 - (1- conf) / 2) * se
        
        upper = xbar + me
        lower = xbar - me
        
        upper_draw = xbar + 4 * se
        lower_draw = xbar - 4 * se
        
        x_axis = round(seq(lower - 3*me, upper + 3 * me, me),2)
        
        x = seq(lower_draw, upper_draw, 0.01)
        y = dnorm(x, xbar, se)
        
        df = data.frame(x, y)
        
        df %>% 
            ggplot(aes(x,y)) +
            geom_area(data = df %>% filter(x >= lower,
                                           x <= upper),
                      mapping = aes(x), 
                      fill = "#1790D2") +
            # lower line
            geom_segment(aes(x = lower, y = 0, 
                             xend = lower, yend = dnorm(lower, xbar, se))) +
            # upper line
            geom_segment(aes(x = upper, y = 0, 
                             xend = upper, yend = dnorm(upper, xbar, se))) +
            # mu
            geom_segment(aes(x = mu, y = 0, 
                             xend = mu, yend = dnorm(mu, xbar, se)),
                         color = "#FC2B1C", size = 2) +
            theme_minimal() +
            geom_line() +
            geom_hline(yintercept = 0) +
            scale_x_continuous(breaks = x_axis,
                               limits = c(lower_draw, upper_draw))+
            ylim(c(0, max(y)+0.1)) +
            labs(title = paste0(conf*100, "% confidence interval:", paste0("[", round(lower,2), ", ", round(upper, 2),"]")),
                 
                 x = "",
                 y = "")
    } 
    )
    
    output$standard_normal <- renderPlot({
        
        conf = input$conf
        xbar = input$xbar
        mu = input$mu
        sigma = input$sigma
        n = input$n
        se = sigma / sqrt(n)
        me = qnorm(1 - (1- conf) / 2) * se
        
        upper = xbar + me
        lower = xbar - me
        
        upper_draw = xbar + 4 * se
        lower_draw = xbar - 4 * se
        
        x_axis = round(seq(lower - 10 * me, upper + 10 * me, me),2)
        
        x = seq(lower_draw, upper_draw, 0.01)
        y = dnorm(x, xbar, se)
        
        df = data.frame(x, y)
        
        df %>% 
            ggplot(aes(x,y)) +
            geom_area(data = df %>% filter(x >= lower,
                                           x <= upper),
                      mapping = aes(x), 
                      fill = "#1790D2") +
            # lower line
            geom_segment(aes(x = lower, y = 0, 
                             xend = lower, yend = dnorm(lower, xbar, se))) +
            # upper line
            geom_segment(aes(x = upper, y = 0, 
                             xend = upper, yend = dnorm(upper, xbar, se))) +
            # mu
            geom_segment(aes(x = mu, y = 0, 
                             xend = mu, yend = dnorm(mu, xbar, se)),
                         color = "#FC2B1C", size = 2) +
            theme_minimal() +
            geom_line() +
            geom_hline(yintercept = 0) +
            scale_x_continuous(breaks = seq(-50, 50, 10),
                               limits = c(-50, 50))+
            ylim(c(0, max(y)+0.1)) +
            labs(title = paste0(conf*100, "% confidence interval:", paste0("[", round(lower,2), ", ", round(upper, 2),"]")),
                 x = "",
                 y = "")
    })
    
    # Confidence Interval    
    make_row <- function(n, mean, stdev) {
        data <- rnorm(n, mean, stdev)
        xbar <- mean(data)
        s <- sd(data)
        return(list(xbar=xbar, s=s))
    }
    
    make_interval <- function(df, stdev, conf_lvl, n, int_type){
        
        xbar = df$xbar
        
        int_type <- ifelse(int_type == "Yes", "z", "t")
        
        if (int_type == "z") {
            se <- input$stdev / sqrt(n)
            cv <- abs(qnorm((1 - conf_lvl) / 2))
        } else {
            se <- df$s / sqrt(n)
            cv <- abs(qt((1 - conf_lvl) / 2, df = n - 1))    
        }
        
        lower <- xbar - cv * se
        upper <- xbar + cv * se
        return(c(lower, upper, xbar))
    }
    
    output$CI_plot <- renderPlot({
        
        # ci_n = 30
        # mean = 0
        # stdev = 1
        # conf_lvl = 0.95 
        # int_type = "t"
        # n_interval = 10
        
        ci_n = input$ci_n
        mean = input$mean
        stdev = input$stdev
        conf_lvl = input$conf_lvl
        int_type = input$int_type
        n_interval = input$n_interval
        
        set.seed(301)
        sample_stats <- t(replicate(100,  make_row(ci_n, mean, stdev)))
        
        df <- data.frame(t(apply(sample_stats, 1, make_interval, 
                                 conf_lvl = conf_lvl, 
                                 n = ci_n,
                                 int_type = int_type)))
        
        names(df) <- c("lower", "upper", "xbar")
        
        df$cover <- ifelse(df$lower <= mean & mean <= df$upper, "1", "0")
        colors <- c("1" = "#1790D2", "0" = "#FC2B1C")
        df$row <- 1:nrow(df)
        df = df[1:n_interval, ]
        
        ggplot(df, aes(color = cover)) +
            geom_vline(xintercept = mean) +
            geom_segment(aes(x = lower, xend = upper, y = row, yend = row), size = 2) +
            geom_point(aes(x = xbar, y = row), size = 3) +
            theme_minimal() +
            scale_color_manual(values = colors) +
            guides(color = FALSE) +
            xlim(c(mean - 2*stdev, mean + 2*stdev)) +
            labs(x = "",
                 y = "")
        
    },
    height = 800) 
    
    output$desc = renderText({
        int_type <- ifelse(input$int_type == "Yes", "z", "t")
        paste0(100*input$conf_lvl, "% ", int_type, "-interval with mean ",
               input$mean, " and standard deviation ", input$stdev, ".")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
