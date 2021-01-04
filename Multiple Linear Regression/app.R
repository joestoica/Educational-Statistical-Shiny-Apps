library(tidyverse)
library(shiny)

ui <- fluidPage(
    theme = "theme.css",
    navbarPage(
        "Multiple Linear Regression",
        tabPanel(
            "Overview",
            withMathJax(includeMarkdown("breakdown.Rmd"))
        ),
        tabPanel(
            "Multiple Regression Example",
            sidebarLayout(
                sidebarPanel(
                    selectInput("data",
                                "Select Data",
                                choices = c("Iris", "Cars"),
                                selected = "Cars"
                    ),
                    selectInput("remove",
                                "Remove variable with highest p-value?",
                                choices = c("yes", "no"),
                                selected = "no"
                    ),
                    img(src = "301.png", align = "center", width="100%"),
                ),
                mainPanel(
                    tableOutput("table1"),
                    tableOutput("table2"),
                    tableOutput("table3"),
                    style = "background-color: #f3f3f3; border-color: #e3e3e3;"
                )
            ),
        )
    )    
)

server <- function(input, output) {
    
    fit <- reactive({
        data = input$data
        remove = input$remove
        
        if (data == "Iris") {
            df = iris
            if (remove == "yes") {
                lm(Sepal.Width ~ Sepal.Length + Petal.Length + Petal.Width, iris)
            } else {
                lm(Sepal.Width ~ Sepal.Length + Petal.Length + Petal.Width + Species, iris)
            }
        } else {
            cars = mtcars
            names(cars) = c("mpg", "cylinders", "dispacement", "horsepower", 
                            "drat", "weight", "quarter_mile_time", "vs", 
                            "transmission_type", "gear", "carb")
            df = cars
            
            if (remove == "yes") {
                lm(mpg ~ weight + quarter_mile_time + transmission_type, cars)
            } else {
                lm(mpg ~ horsepower + weight + quarter_mile_time + transmission_type, cars)
            }
        }  
    })
    
    format_output <- function(chr){
        r <- format(round(chr, 3), scientific = FALSE)
        if (sum(r == "0") > 0) {
            return(format(chr, scientific = TRUE))
        } else {
            return(r)
        }
    }
    
    output$table1 <- renderTable({
        mod = fit()
        
        table_1_names =c("Multiple R",
                         "R Square",
                         "Adjusted R Square",
                         "Standard Error",
                         "Observations")
        
        Values = c(format_output(sqrt(summary(mod)$r.squared)),
                   format_output(summary(mod)$r.squared),
                   format_output(summary(mod)$adj.r.squared),
                   format_output(summary(mod)$sigma),
                   format_output(nrow(mod$model)))

        data.frame(table_1_names, Values) %>%
            rename(`Regression Statistics` = "table_1_names")
        
    })
    
    output$table2 <- renderTable({
        mod = fit()
        dfr <- format_output(anova(mod)$Df[1])
        dfe <- format_output(anova(mod)$Df[2])
        dft <- format_output(sum(anova(mod)$Df))
        ssr <- format_output(anova(mod)$Sum[1])
        sse <- format_output(anova(mod)$Sum[2])
        sst <- format_output(sum(anova(mod)$Sum))
        msr <- format_output(anova(mod)$Mean[1])
        mse <- format_output(anova(mod)$Mean[2])
        Fanova <- format_output(anova(mod)$F[1])
        panova <- format_output(anova(mod)$Pr[1])

        ANOVA = c("Regression", "Residual", "Total")
        df = c(dfr, dfe, dft)
        SS = c(ssr, sse, sst)
        MS = c(msr, mse, "")
        F_col = c(Fanova, "", "")
        Significance = c(panova, "", "")

        data.frame(ANOVA, df, SS, MS, F_col, Significance) %>%
            rename(`F Stat.` = "F_col")

    })

    output$table3 <- renderTable({
        mod = fit()
        coef_info <- as.data.frame(cbind(summary(mod)$coef, confint(mod, level = .95)))
        names(coef_info) <- c("Coefficients", "Standard Error", "t Stat", "P-value", "Lower 95%", "Upper 95%")
        coef_info$`P-value` <- format_output(coef_info$`P-value`)
        coef_info[,-4] <- round(coef_info[,-4],3)

        coef_info %>%
            rownames_to_column() %>%
            rename("Variable" = "rowname")
    })

} 

shinyApp(ui = ui, server = server)
