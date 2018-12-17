library(shiny)
library(tidyverse)
#library(DT)

source("data.R")

ui <- fluidPage(
   
   titlePanel("United Nations TFR"),
   
   sidebarLayout(
     sidebarPanel(
       selectInput("country", "Select Country", choices = as.character(unique(tfr_p$Country.or.area))),
       
       hr(),
       
       radioButtons("model", "Model Selection",
                    choices = c("Polynomial" = "wpoly",
                                "Local Quadratic Imputation" = "impute"),
                    selected = "wpoly"),
       
       conditionalPanel(condition = "input.model == 'impute'",
                        checkboxInput("unpred", "Show UN predction?", value = T)
       ),
       
       actionButton("runButton", "Go")
     ),
     
     mainPanel(
       h4("Plot"),
       plotlyOutput("plot1")
       # h4("Data"),
       # DT::dataTableOutput("dtable")
     )
   )
)

server <- function(input, output) {
  
  fit1 <- eventReactive(input$runButton, {
    req(input$country)
    if (input$model == "wpoly") {
      m1 <- best_model(input$country, tfr_data = tfr_p %>% bind_rows(wpp))
    }
    if (input$model == "impute") {
      m1 <- best_model(input$country, tfr_data = imputed_p, imputed = TRUE, show.wpp = input$unpred)
    }
    m1
  })
  
  # output$dtable <- DT::renderDataTable({
  #   req(fit1())
  #   return(as.data.frame(fit1()$data))
  # })
  
  output$plot1 <- renderPlotly({
    req(fit1())
    fit1()$plot
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

