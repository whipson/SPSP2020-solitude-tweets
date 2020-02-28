#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(MASS)
library(tidyverse)
library(ggrepel)
library(plotly)

lonely_v_alone <- read_csv("lonely_v_alone.csv")
solitude_v_alone <- read_csv("solitude_v_alone.csv")
solitude_v_lonely <- read_csv("solitude_v_lonely.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),

    # Application title
    titlePanel("Distinguishing Positive and Negative Aspects of Solitude in Tweets"),

    sidebarLayout(
        sidebarPanel(
          h5("Is the word 'solitude' associated with more positive, calming emotional states? Is it worse to be 'lonely' than 'alone'?
             These are questions we sought to answer using over 19M Tweets."),
          h6("Display shows the relation between word co-occurrence and dimensions of affect. Use the options below
             to select which dimension of affect to look at. By default, analysis is limited to words that occurred at least 500 times."),
            sliderInput("cutoff",
                        "Minimum Word Count:",
                        min = 100,
                        max = 5000,
                        value = 500),
            radioButtons("dimension", 
                         h3("Dimension of Affect"), 
                         choices = list("Valence (unpleasant vs. pleasant)" = "valence", 
                                        "Arousal (deactivated vs. activated)" = "arousal", 
                                        "Dominance (weak vs. powerful)" = "dominance"),
                         selected = "valence"),
          h5("Hipson, Kiritchenko, Mohammad, & Coplan (2020)")),

        mainPanel(
          
          tabsetPanel(type = "tabs",
                      tabPanel("Solitude vs. Lonely", plotlyOutput("plot1"), DT::dataTableOutput("results1")),
                      tabPanel("Solitude vs. Alone", plotlyOutput("plot2"), DT::dataTableOutput("results2")),
                      tabPanel("Lonely vs. Alone", plotlyOutput("plot3"), DT::dataTableOutput("results3"))
                      )

        )
    )
)

server <- function(input, output) {

  output$plot1 <- renderPlotly({
    solitude_v_lonely %>%
      filter(frequency >= input$cutoff) %>%
      rename(x = input$dimension) %>%
      ggplot(aes(x = x, y = pmi, label = Term)) +
      geom_point(aes(alpha = abs(pmi)), color = "dodgerblue2", position = "jitter") +
      geom_smooth(method = 'lm', color = "black") +
      scale_y_continuous(breaks = c(-3, 0,  6),
                         labels = c("Lonely", 0, "Solitude")) +
      labs(x = paste(str_to_title(input$dimension)),
           y = "Tendency of Co-occurrence") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = .5),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 12, face = "bold"))
    })
    
    output$plot2 <- renderPlotly({
      solitude_v_alone %>%
        filter(frequency >= input$cutoff) %>%
        rename(x = input$dimension) %>%
        ggplot(aes(x = x, y = pmi, label = Term)) +
        geom_point(aes(alpha = abs(pmi)), color = "#00CD66", position = "jitter") +
        geom_smooth(method = 'lm', color = "black") +
        scale_y_continuous(breaks = c(-3, 0,  5),
                           labels = c("Alone", 0, "Solitude")) +
        labs(x = paste(str_to_title(input$dimension)),
             y = "Tendency of Co-occurrence") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = .5),
              axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 12, face = "bold"))
    })
    
    output$plot3 <- renderPlotly({
      lonely_v_alone %>%
        filter(frequency >= input$cutoff) %>%
        rename(x = input$dimension) %>%
        ggplot(aes(x = x, y = pmi, label = Term)) +
        geom_point(aes(alpha = abs(pmi)), color = "#EE3B3B", position = "jitter") +
        geom_smooth(method = 'lm', color = "black") +
        scale_y_continuous(breaks = c(-4, 0,  3.5),
                           labels = c("Alone", 0, "Lonely")) +
        labs(x = paste(str_to_title(input$dimension)),
             y = "Tendency of Co-occurrence") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = .5),
              axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 12, face = "bold"))
    })
    
    output$results1 <- DT::renderDataTable({
      fit1 <- solitude_v_lonely %>%
        filter(frequency >= input$cutoff) %>%
        rename(x = input$dimension) %>%
        lm(pmi ~ x, data = .)
      results1 <- summary(fit1)$coefficients
      results1 <- data.frame(round(results1, 2), row.names = c("Intercept", "Affect"))
      colnames(results1) <- c("Estimate", "Std. Error", "t-value", "p-value")
      results1$`p-value` <- ifelse(results1$`p-value` < .001, "< .001", results1$`p-value`)
      results1
    },
    options = list(bFilter = FALSE,
                   bInfo = 0,
                   blengthChange = 0,
                   bPaginate = FALSE))
    
    output$results2 <- DT::renderDataTable({
      fit2 <- solitude_v_alone %>%
        filter(frequency >= input$cutoff) %>%
        rename(x = input$dimension) %>%
        lm(pmi ~ x, data = .)
      results2 <- summary(fit2)$coefficients
      results2 <- data.frame(round(results2, 2), row.names = c("Intercept", "Affect"))
      colnames(results2) <- c("Estimate", "Std. Error", "t-value", "p-value")
      results2$`p-value` <- ifelse(results2$`p-value` < .001, "< .001", results2$`p-value`)
      results2
    },
    options = list(bFilter = FALSE,
                   bInfo = 0,
                   blengthChange = 0,
                   bPaginate = FALSE))
    
    output$results3 <- DT::renderDataTable({
      fit3 <- lonely_v_alone %>%
        filter(frequency >= input$cutoff) %>%
        rename(x = input$dimension) %>%
        lm(pmi ~ x, data = .)
      results3 <- summary(fit3)$coefficients
      results3 <- data.frame(round(results3, 2), row.names = c("Intercept", "Affect"))
      colnames(results3) <- c("Estimate", "Std. Error", "t-value", "p-value")
      results3$`p-value` <- ifelse(results3$`p-value` < .001, "< .001", results3$`p-value`)
      results3
    },
    options = list(bFilter = FALSE,
                   bInfo = 0,
                   blengthChange = 0,
                   bPaginate = FALSE))
}

# Run the application 
shinyApp(ui = ui, server = server)
