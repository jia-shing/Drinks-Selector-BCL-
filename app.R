#Loading libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(shinythemes)
library(shinydisconnect)

#IGNORE: Metadata for social sharing

#Reading in the data
bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

# UI Specification
ui <- fluidPage(
  disconnectMessage2(),
  theme = shinytheme("paper"),
  titlePanel("BC Liquor Store Prices", "BCLS Prices"),
  sidebarLayout(
    sidebarPanel(
      h5("🥱 Had a long day?"),
      h5("🍻 Let us help you find the right drink for tonight!"), 
      br(),
      sliderInput("priceInput", "Price", min = 0, max = 100, 
                             value = c(25,40), pre = "$"),
      uiOutput("typeSelectOutput"),
      checkboxInput("filterCountry", 'Filter by country', FALSE),
      conditionalPanel(
        condition = "input.filterCountry",
        uiOutput("countrySelectorOutput")
      ),
      hr(),
      span("Data source:",
           tags$a("Open Data BC (Cleaned)", href = "https://deanattali.com/files/bcl-data.csv")),
      br(),
      span("Credit:",
           tags$a("Dean Attali", href = "https://deanattali.com/")),
      br(), br(),
      em(
        span("Created by", a(href = "https://jia-shing.com/", "Jia Shing Wee")),
        HTML("&bull;"),
        span("Code available", a(href = "https://github.com/jia-shing", "on my Github"))
      ),
    ),
    mainPanel(
      h3(textOutput("summaryText")),
      downloadButton("download", 'Download Results'),
      br(),
      br(),
      plotOutput("plot"),
      br(), br(),
      dataTableOutput("prices")
    )
  )
)

# Server Specification
server <- function(input, output, session) {
  output$countrySelectorOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })
  
  output$typeSelectOutput <- renderUI({
    selectInput("typeInput", "Product type",
                sort(unique(bcl$Type)),
                multiple = TRUE,
                selected = c("BEER", "WINE"))
  })
  
  output$summaryText <- renderText({
    numOptions <- nrow(prices())
    if (is.null(numOptions)) {
      numOptions <- 0
    }
    paste0("We found ", numOptions, " options for you")
  })
  
  prices <- reactive({
    prices <- bcl
    
    if (is.null(input$countryInput)) {
      return(NULL)
    }
    
    prices <- dplyr::filter(prices, Type %in% input$typeInput)
    if (input$filterCountry) {
      prices <- dplyr::filter(prices, Country == input$countryInput)
    }
    prices <- dplyr::filter(prices, Price >= input$priceInput[1],
                            Price <= input$priceInput[2])
    
    if(nrow(prices) == 0) {
      return(NULL)
    }
    prices
  })
  
  output$plot <- renderPlot({
    if (is.null(prices())) {
      return(NULL)
    }
    
    ggplot(prices(), aes(Alcohol_Content, fill = Type)) +
      geom_histogram(colour = "black") +
      theme_classic(20)
  })
  
  output$prices <- DT::renderDataTable({
    prices()
  })
  
  output$download <- downloadHandler(
    filename = function() {
      "bcl-results.csv"
    },
    content = function(con) {
      write.csv(prices(), con)
    }
  )
}

shinyApp(ui = ui, server = server)