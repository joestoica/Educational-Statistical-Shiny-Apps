# Huge shoutout to https://gallery.shinyapps.io/CLT_mean/ for supplying a lot of
# the code.

library(shiny)
library(tidyverse)
library(gridExtra)

ui <- fluidPage(
    theme = "theme.css",
    navbarPage(
        "Central Limit Theorem",
        tabPanel(
            title = "Overview",
            withMathJax(includeMarkdown("breakdown.Rmd"))
        ),
        
        tabPanel(
            title = "Visualizing The CLT",
            sidebarLayout(
                sidebarPanel(
                    radioButtons("dist", "Parent distribution (population):",
                                 c("Normal" = "rnorm",
                                   "Uniform" = "runif",
                                   "Right skewed" = "rlnorm",
                                   "Left skewed" = "rbeta"),
                                 selected = "rnorm"),
                    
                    uiOutput("mu"),
                    uiOutput("sd"),
                    uiOutput("minmax"),
                    uiOutput("skew"),
                    
                    numericInput("n",
                                "Sample size:", 
                                value = 2,
                                min = 2,
                                max = 500),
                    # Number of samples ----
                    numericInput("k",
                                 "Number of samples:",
                                 value = 2,
                                 min = 2,
                                 max = 1000),
                    img(src = "301.png", align = "center", width="100%"),
                ),
                mainPanel(
                    plotOutput("pop.dist"),
                    plotOutput("sampling.dist"),
                )
            ),
            
        ),
        
        tabPanel(
            title = "Example Problems",
            withMathJax(includeMarkdown("problems.Rmd"))
        ),
        
        tabPanel(
            title = "Example Solutions",
            withMathJax(includeMarkdown("solutions.Rmd"))
        )
    )
)

seed <- as.numeric(Sys.time())

server <- function(input, output, session) {
    
    observeEvent(input[["tabset"]], {
        if(input[["tabset"]] == "Overview") {
            hideElement(selector = "#sidebar")
            removeCssClass("main", "col-sm-8")
            addCssClass("main", "col-sm-12")
        }else{
            showElement(selector = "#sidebar")
            removeCssClass("main", "col-sm-12")
            addCssClass("main", "col-sm-8")
        }
    })
    
    # Mean slider for Normal distribution ----
    output$mu = renderUI(
        {
            if (input$dist == "rnorm")
            {
                sliderInput("mu",
                            "Mean:",
                            value = 0,
                            min = -40,
                            max = 50)
            }
        })
    
    # SD slider for Normal distribution ----
    output$sd = renderUI(
        {
            if (input$dist == "rnorm")
            {
                sliderInput("sd",
                            "Standard deviation:",
                            value = 1,
                            min = 1,
                            max = 30)
            }
        })
    
    # Minmax slider for Uniform distribution ----
    output$minmax = renderUI(
        {
            
            if (input$dist == "runif")
            {
                sliderInput("minmax",
                            "Lower and Upper Bounds",
                            value = c(5, 15),
                            min = 0,
                            max = 20)
            }
        })
    
    # Making sure range for uniform distribution != 0 ----
    observeEvent(input$minmax, {
        
        req(input$minmax)
        
        if (input$minmax[1] == input$minmax[2]) {
            if (input$minmax[1] == 0) {
                updateSliderInput(session, "minmax", value = c(0, 1))
            } else if (input$minmax[2] == 20) {
                updateSliderInput(session, "minmax", value = c(19, 20))
            } else {
                updateSliderInput(session, "minmax", value = c(input$minmax[2], input$minmax[2] + 1))
            }
        }
    })
    
    # skew slider for rlnorm and rbeta ----
    output$skew = renderUI(
        {
            
            if (input$dist == "rlnorm" | input$dist == "rbeta") {
                selectInput(inputId = "skew",
                            label = "Skew:",
                            choices = c("Low skew" = "low",
                                        "Medium skew" = "med",
                                        "High skew" = "high"),
                            selected = "low")
            }
        })
    
    # generating random samples ----
    rand_draw <- function(dist, n, mu, sd, min, max, skew) {
        
        vals = NULL
        
        if (dist == "rbeta") {
            req(skew)
            if (skew == "low") {
                vals = do.call(dist, list(n=n, shape1=5, shape2=2))
            }
            else if (skew == "med") {
                vals = do.call(dist, list(n=n, shape1=5, shape2=1.5))
            }
            else if (skew == "high") {
                vals = do.call(dist, list(n=n, shape1=5, shape2=1)) 
            }
        }
        
        else if (dist == "rnorm") {
            req(mu, sd)
            vals = do.call(dist, list(n=n, mean=mu, sd=sd))
        }
        
        else if (dist == "rlnorm") {
            req(skew)
            if (skew == "low") {
                vals = do.call(dist, list(n=n, meanlog=0, sdlog=.25))
            }
            else if (skew == "med") {
                vals = do.call(dist, list(n=n, meanlog=0, sdlog=.5))
            }
            else if (skew == "high") {
                vals = do.call(dist, list(n=n, meanlog=0, sdlog=1))
            }
        }
        
        else if (dist == "runif") {
            req(min, max)
            vals = do.call(dist, list(n=n, min=min, max=max))
        }
        return(vals)
    }
    
    rep_rand_draw = repeatable(rand_draw)
    
    # Defining some reactive variables to use later ----
    parent = reactive({
        
        n_sample = 1e5
        
        return(rep_rand_draw(input$dist, n_sample, input$mu, input$sd,
                             input$minmax[1], input$minmax[2], input$skew))
    })
    
    samples = reactive({
        
        pop = parent()
        n = input$n
        k = input$k
        
        return(replicate(k, sample(pop, n, replace=TRUE)))
    })
    
    u_min = reactive({
        req(input$minmax)
        return(input$minmax[1])
    })
    
    u_max = reactive({
        req(input$minmax)
        return(input$minmax[2])
    })
    
    # plot 1 a) ----
    output$pop.dist = renderPlot({
        
        distname = switch(input$dist,
                          rnorm = "Population distribution: Normal",
                          rlnorm = "Population distribution: Right skewed",
                          rbeta = "Population distribution: Left skewed",
                          runif = "Population distribution: Uniform")
        
        pop = parent()
        
        m_pop =  round(mean(pop), 2)
        sd_pop = round(sd(pop), 2)
        
        pop = tibble(samples = pop)
        pdens = density(pop$samples)
        
        x_range = max(pop$samples) - min(pop$samples)
        y_pos = max(pdens$y) - 0.2*max(pdens$y)
        
        if (input$dist == "rnorm") {
            
            req(input$mu)
            mu = input$mu
            
            x_pos = ifelse(mu > 0, min(-100, min(pop$samples)) + 20,
                           max(100, max(pop$samples)) - 20)
            
            ggplot(data = pop, aes(x = samples, y = ..density..)) + 
                geom_density( color = "#1790D2", fill = "#1790D2", size = 1) +
                scale_x_continuous(limits = c(min(-100, pop$samples), max(100, pop$samples))) +
                labs(title = distname, x = "x",
                     subtitle = paste("mean of x = ", bquote(.(m_pop)), "\n", "SD of x = ", bquote(.(sd_pop)))) +
                theme_minimal() + 
                geom_vline(xintercept = m_pop) + 
                xlim(min(pop$samples), max(pop$samples))
            
        } else if (input$dist == "runif") {
            
            if (u_min() == u_max()) {
                "  " # this is to temporarily prevent graph from displaying while 
                # observeEvent is fixing the range.
            } else {
                
                x_pos = max(pop$samples) - 0.1*x_range
                
                ggplot(data = pop, aes(x = samples, y = ..density..)) +
                    geom_density(color = "#1790D2", fill = "#1790D2", size = 1) +
                    scale_y_continuous(expand = expand_scale(mult = c(0, .3))) +
                    labs(title = distname, x = "x",
                         subtitle = paste0("mean of x = ", bquote(.(m_pop)), "\n", "SD of x = ", bquote(.(sd_pop)))) +
                    theme_minimal() +
                    geom_vline(xintercept = m_pop) + 
                    xlim(min(pop$samples), max(pop$samples))}
            
        } else if (input$dist == "rlnorm") {
            
            x_pos = max(pop$samples) - 0.1*x_range
            
            ggplot(data = pop, aes(x = samples, y = ..density..)) + 
                geom_density( color = "#1790D2", fill = "#1790D2", size = 1) +
                labs(title = distname, x = "x",
                     subtitle = paste0("mean of x = ", bquote(.(m_pop)), "\n", "SD of x = ", bquote(.(sd_pop)))) +
                theme_minimal() +
                geom_vline(xintercept = m_pop) + 
                xlim(min(pop$samples), max(pop$samples))
            
        } else if (input$dist == "rbeta") {
            
            x_pos = min(pop$samples) + 0.1*x_range
            
            ggplot(data = pop, aes(x = samples, y = ..density..)) + 
                geom_density( color = "#1790D2", fill = "#1790D2",size = 1) +
                labs(title = distname, x = "x",
                     subtitle = paste0("mean of x = ", bquote(.(m_pop)), "\n", "SD of x = ", bquote(.(sd_pop)))) +
                theme_minimal() +
                geom_vline(xintercept = m_pop) + 
                xlim(min(pop$samples), max(pop$samples))
            
        }
    })
    
    # plot 2 ----
    output$sampling.dist = renderPlot({
        
        distname = switch(input$dist,
                          rnorm = "normal population",
                          rlnorm  = "right skewed population",
                          rbeta = "left skewed population",
                          runif = "uniform population")
        
        n = input$n
        k = input$k
        
        pop = parent()
        
        ndist = tibble(means = colMeans(samples()))
        pop = tibble(samples = pop)
        m_samp =  round(mean(ndist$means),2)
        sd_samp = round(sd(ndist$means),2)
        
        ndens = density(ndist$means)
        nhist = hist(ndist$means, plot = FALSE)
        
        x_range = max(ndist$means) - min(ndist$means)
        
        y_pos = max(ndens$y) - 0.1*max(ndens$y)
        x_pos = ifelse(m_samp > 0, min(ndist$means) + 0.1*x_range, 
                       max(ndist$means) - 0.1*x_range)
        
        p = ggplot(data = ndist) +
            #geom_histogram(color = "white", fill = "#009499") +
            geom_density(aes(x = means, y = ..density..), color = "#FC2B1C", size = 1, fill = "#FC2B1C") +
            geom_rug(aes(means)) +
            labs(title = paste("Sampling Distribution*"),
                 subtitle = paste0("mean of x_bar = ", bquote(.(m_samp)), "\n", "SE of x_bar =", bquote(.(sd_samp))),
                 x = "Sample means",
                 y = "") +
            geom_vline(xintercept = m_samp) + 
            theme_minimal() +
            xlim(min(pop$samples), max(pop$samples))
        
        if (input$dist == "runif") {
            
            if (u_min() == u_max()) {
                " "
            } else {
                p
            }
        } else {
            p
        }
    })
    
}
shinyApp(ui = ui, server = server)
