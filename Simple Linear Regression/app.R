library(tidyverse)
library(shiny)
library(kableExtra)

ui <- fluidPage(
    theme = "theme.css",
    navbarPage(
        "Simple Linear Regression",
        tabPanel(
            "Overview",
            withMathJax(includeMarkdown("breakdown.Rmd"))
        ),
        tabPanel(
            "Simple Linear Regression Visualization",
            # sidebarLayout(
            #     sidebarPanel(
            #         selectInput("col1",
            #                     "First column from example data",
            #                     choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
            #                     selected = "Sepal.Length"
            #         ),
            #         selectInput("col2",
            #                     "Second column from example data",
            #                     choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
            #                     selected = "Sepal.Width"
            #         ),
            #     ),
            # )
            mainPanel(
                tableOutput("table1"),
                tableOutput("table2"),
                tableOutput("table3"),
                plotOutput("plot"),
                style="background-color: #f3f3f3; 
                    border-color: #e3e3e3;
                    padding-bottom: 32px"  
            )
        )
    )    
)

set.seed(301)

server <- function(input, output) {
    
    fit = reactive({
        lm(price ~ carat, diamonds)
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
        
        fit = fit()
        table_1_names =c("Multiple R",
                         "R Square",
                         "Adjusted R Square",
                         "Standard Error",
                         "Observations")
        
        Values = c(format_output(sqrt(summary(fit)$r.squared)),
                   format_output(summary(fit)$r.squared),
                   format_output(summary(fit)$adj.r.squared), 
                   format_output(summary(fit)$sigma),
                   format_output(nrow(fit$model)))
        
        data.frame(table_1_names, Values) %>% 
            rename(`Regression Statistics` = "table_1_names") 
        
    })
    
    output$table2 <- renderTable({
        
        fit = fit()
        
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
    
    output$table3 <- renderTable({
        fit = fit()
        coef_info <- as.data.frame(cbind(summary(fit)$coef, confint(fit, level = .95)))
        names(coef_info) <- c("Coefficients", "Standard Error", "t Stat", "P-value", "Lower 95%", "Upper 95%")
        row.names(coef_info) <- c("Intercept", "Var1")
        coef_info$`P-value` <- format_output(coef_info$`P-value`)
        coef_info[,-4] <- round(coef_info[,-4],3)
        
        coef_info %>% 
            rownames_to_column() %>% 
            rename("Variable" = "rowname")
    })
    
    output$plot <- renderPlot({
        
        fit = fit()
        fit$model %>%
            ggplot(aes(carat, price)) +
            geom_point() +
            geom_smooth(method = "lm", color = "red", se = FALSE) +
            theme_minimal() +
            labs(x = "Carat",
                 y = "Price",
                 title = paste0("Simple Linear Regression Model for predicting Diamond prices using Carat"))
    })
} 

shinyApp(ui = ui, server = server)
