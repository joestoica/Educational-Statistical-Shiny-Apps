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
                                choices = c("iris", "diamonds"),
                                selected = "iris"
                    ),
                    selectInput("remove",
                                "Remove column?",
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

set.seed(301)

server <- function(input, output) {
    
    format_output <- function(chr){
        r <- format(round(chr, 3), scientific = FALSE)
        if(sum(r == "0") > 0){
            return(format(chr, scientific = TRUE))
        }else{
            return(r)
        }
    }
    
    #    output$table1 <- function() {
    output$table1 <- renderTable({
        
        data = input$data
        remove = input$remove
        
        if (data == "iris") {
            df = iris
            if (remove == "yes") {
                fit = lm(Sepal.Width ~ Sepal.Length + Petal.Length + Petal.Width, iris)
            } else {
                fit = lm(Sepal.Width ~ Sepal.Length + Petal.Length + Petal.Width + Species, iris)
            }
        } else {
            df = diamonds
            if (remove == "yes") {
                fit = lm(price ~ carat + cut + clarity, diamonds)    
            } else {
                fit = lm(price ~ carat + cut + clarity + table, diamonds)    
            }
            
        }
        
        table_1_names =c("Multiple R",
                         "R Square",
                         "Adjusted R Square",
                         "Standard Error",
                         "Observations")
        
        Values = c(format_output(sqrt(summary(fit)$r.squared)),
                   format_output(summary(fit)$r.squared),
                   format_output(summary(fit)$adj.r.squared), 
                   format_output(summary(fit)$sigma),
                   format_output(nrow(df)))
        
        data.frame(table_1_names, Values) %>% 
            rename(`Regression Statistics` = "table_1_names")
        
    })
    
#    output$table2 <- function(){
    output$table2 <- renderTable({
        
        data = input$data
        remove = input$remove
        
        if (data == "iris") {
            df = iris
            if (remove == "yes") {
                fit = lm(Sepal.Width ~ Sepal.Length + Petal.Length + Petal.Width, iris)
            } else {
                fit = lm(Sepal.Width ~ Sepal.Length + Petal.Length + Petal.Width + Species, iris)
            }
        } else {
            df = diamonds
            if (remove == "yes") {
                fit = lm(price ~ carat + cut + clarity, diamonds)    
            } else {
                fit = lm(price ~ carat + cut + clarity + table, diamonds)    
            }
            
        }
        
        dfr <- format_output(anova(fit)$Df[1])
        dfe <- format_output(anova(fit)$Df[2])
        dft <- format_output(sum(anova(fit)$Df))
        ssr <- format_output(anova(fit)$Sum[1])
        sse <- format_output(anova(fit)$Sum[2])
        sst <- format_output(sum(anova(fit)$Sum))
        msr <- format_output(anova(fit)$Mean[1])
        mse <- format_output(anova(fit)$Mean[2])
        Fanova <- format_output(anova(fit)$F[1])
        panova <- format_output(anova(fit)$Pr[1])
        
        
        ANOVA = c("Regression", "Residual", "Total")
        df = c(dfr, dfe, dft)
        SS = c(ssr, sse, sst)
        MS = c(msr, mse, "")
        F_col = c(Fanova, "", "")
        Significance = c(panova, "", "")
        
        
        data.frame(ANOVA, df, SS, MS, F_col, Significance) %>% 
            rename(`F Stat.` = "F_col")
            
    })
    
    #output$table3 <- function(){
    output$table3 <- renderTable({
        
        # data = "iris"
        # remove = "no"
        data = input$data
        remove = input$remove
        
        if (data == "iris") {
            df = iris
            if (remove == "yes") {
                fit = lm(Sepal.Width ~ Sepal.Length + Petal.Length + Petal.Width, iris)
            } else {
                fit = lm(Sepal.Width ~ Sepal.Length + Petal.Length + Petal.Width + Species, iris)
            }
        } else {
            df = diamonds
            if (remove == "yes") {
                fit = lm(price ~ carat + cut, diamonds)    
            } else {
                fit = lm(price ~ carat + cut + clarity, diamonds)    
            }
            
        }
        
        coef_info <- as.data.frame(cbind(summary(fit)$coef, confint(fit, level = .95)))
        names(coef_info) <- c("Coefficients", "Standard Error", "t Stat", "P-value", "Lower 95%", "Upper 95%")
        coef_info$`P-value` <- format_output(coef_info$`P-value`)
        coef_info[,-4] <- round(coef_info[,-4],3)
        
        coef_info %>% 
            rownames_to_column() %>% 
            rename("Variable" = "rowname")
    })
    
} 

shinyApp(ui = ui, server = server)
