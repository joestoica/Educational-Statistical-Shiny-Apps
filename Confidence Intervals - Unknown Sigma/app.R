library(tidyverse)
library(shiny)

ui <- fluidPage(
    theme = "theme.css",
    navbarPage(
        "Confidence intervals with unknown population standard devation",
        tabPanel(
            "Overview",
            withMathJax(includeMarkdown("breakdown.Rmd"))
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
                    numericInput("n_intervals",
                                 "Number of Intervals:",
                                 value = 10,
                                 min = 1,
                                 max = 1000),
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

set.seed(301)

server <- function(input, output) {
    
    # Top plot in the first visual page of app
    output$normal <- renderPlot({
        
        # testing values - uncomment and run these to store them in the 
        # environment to do testing so you don't have to reload the app 
        # everytime
        
        # Getting inputs
        conf = 0.95
        xbar = 10
        mu = 0
        s = 5
        n = 30
        
        # Getting inputs from UI code
        conf = input$conf
        xbar = input$xbar
        mu = input$mu
        # TODO change to t
        s = input$s
        n = input$n
        
        # calculate SE
        se = s / sqrt(n)
        
        # Calculate me
        cv = qt(1 - (1 - conf) / 2, df = n-1)
        me = cv * se
        
        # Confidence Interval
        upper = xbar + me
        lower = xbar - me
        # c(lower, upper)
        
        # This is the upper and lower portion of the geom_dist that we want to 
        # draw on the ggplot to make sure everything is visualized nicely
        upper_draw = xbar + 4
        lower_draw = xbar - 4
        c(lower_draw, upper_draw)
        
        # This makes the breaks on the x-axis scaled to match the ME 
        x_axis_breaks = -4:4
        
        # Calculate the data to draw geom_dist() with
        x = seq(lower_draw, upper_draw, 0.01)
        # use the normal density to draw the shape
        y = dt(x, df = n-1)

        plot(x,y)
                
        # temp dataframe to draw ggplot with
        df = data.frame(x, y)
        df %>% 
            ggplot(aes(x,y)) +
            # fills the area within the CI
            geom_area(data = df %>% filter(x >= lower,
                                           x <= upper),
                      mapping = aes(x), 
                      fill = "red", alpha = 0.5) +
            # lower CV line
            geom_segment(aes(x = lower, y = 0, 
                             xend = lower, yend = dt(lower, df = n-1))) +
            # upper CV line
            geom_segment(aes(x = upper, y = 0, 
                             xend = upper, yend = dt(upper, df= n-1))) +
            # Population mean line
            geom_segment(aes(x = mu, y = 0, 
                             xend = mu, yend = dt(mu, df = n-1)),
                         color = "#8080FF", size = 2) +
            theme_minimal() +
            # draw the density plot
            geom_line() +
            geom_hline(yintercept = 0) +
            # scale x_axis
            scale_x_continuous(breaks = x_axis_breaks,
                               limits = c(lower_draw, upper_draw))+
            # shorten y-axis, ran into some errors without this.
            ylim(c(0, max(y)+0.1)) +
            # TODO make a better title
            labs(title = paste0(conf*100, "% confidence interval:", paste0("[", round(lower,2), ", ", round(upper, 2),"]")),
                 
                 x = "",
                 y = "")
    } 
    )
    
    output$standard_normal <- renderPlot({
        
        # this function is very similar to the one above, except this function
        # scales the x axis to illustrate how the CI changes when n, sd, or conf
        # level changes.
        
        # TODO - maybe there is a way to speed up this calculation by combining 
        # with the function above, low priority for now.
        
        conf = input$conf
        xbar = input$xbar
        mu = input$mu
        s = input$s
        n = input$n
        # TODO change to t
        se = s / sqrt(n)
        me = qt(1 - (1- conf) / 2, df = n-1) * se
        
        upper = xbar + me
        lower = xbar - me
        
        upper_draw = xbar + 4 * se
        lower_draw = xbar - 4 * se
        
        x_axis_breaks = round(seq(lower - 10 * me, upper + 10 * me, me),2)
        
        x = seq(lower_draw, upper_draw, 0.01)
        # TODO change to t
        y = dt(x, n-1)
        
        df = data.frame(x, y)
        
        df %>% 
            ggplot(aes(x,y)) +
            geom_area(data = df %>% filter(x >= lower,
                                           x <= upper),
                      mapping = aes(x), 
                      fill = "red", alpha = 0.5) +
            # lower line
            geom_segment(aes(x = lower, y = 0, 
                             xend = lower, yend = dt(lower, df = n-1))) +
            # upper line
            geom_segment(aes(x = upper, y = 0, 
                             xend = upper, yend = dt(upper, df = n-1))) +
            # mu
            geom_segment(aes(x = mu, y = 0, 
                             xend = mu, yend = dt(mu, df = n-1)),
                         color = "#8080FF", size = 2) +
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
    
    
    # --------------------------------------
    # Confidence Interval
    # --------------------------------------
    
    # honestly this method isn't great and I would like to redo it.
    # I would like to make this additive instead of random every draw.
    
    # Creates a sample from normal with specified mean and sd
    # returns a list with the sample mean and sample standard deviation
    make_row <- function(n, mean, stdev) {
        data <- rnorm(n, mean, stdev)
        xbar <- mean(data)
        s <- sd(data)
        return(list(xbar=xbar, s=s))
    }
    
    # This function calculates the % CI and returns the lower, upper, and sample
    # mean for that interval. Calculates z or t interval depending on input.
    make_interval <- function(df, stdev, conf_lvl, n, int_type){
        int_type <- ifelse(int_type == "Yes", "z", "t")
        
        xbar = df$xbar
        stdev = df$s
        
        if(int_type == "z") {
            se <- input$stdev / sqrt(n)
            cv <- abs(qnorm((1 - conf_lvl) / 2))
        } else {
            se <- stdev / sqrt(n)
            cv <- abs(qt((1 - conf_lvl) / 2, df = n - 1))    
        }
        
        lower <- xbar - cv * se
        upper <- xbar + cv * se
        return(c(lower, upper, xbar))
    }
    
    output$CI_plot <- renderPlot({
        
        # The number of simulations to run
        sims <- input$n_intervals
        
        # Creates the specificed number of sample means and SDs
        sample_stats <- t(replicate(sims,  make_row(input$ci_n, input$mean, input$stdev)))
        
        # Makes the data.frame used in plotting that contains the upper, lower,
        # and xbar to draw each interval
        df <- data.frame(t(apply(sample_stats, 1, make_interval, 
                                 conf_lvl = input$conf_lvl, 
                                 n = input$ci_n,
                                 int_type = input$int_type)))
        
        
        names(df) <- c("lower", "upper", "xbar")
        
        # logic for coloring the geom_segment for the CI
        df$cover <- ifelse(df$lower <= input$mean & input$mean <= df$upper, "1", "0")
        colors <- c("1" = "#1790D2", "0" = "#FC2B1C")
        df$row <- 1:nrow(df)
        
        # draws plot
        ggplot(df, aes(color = cover)) +
            geom_vline(xintercept = input$mean)+
            geom_segment(aes(x = lower, xend = upper, y = row, yend = row))+
            geom_point(aes(x = xbar, y = row))+
            theme_minimal()+
            scale_color_manual(values = colors)+
            guides(color = FALSE)+
            xlim(c(input$mean - 4*input$stdev, input$mean + 4*input$stdev))
        
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
