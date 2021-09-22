# Shiny App for WCM Script woRdcomplex-2.1

library(shiny)
library(DT)
library(stringr)

ui <- fluidPage(
  
  # App title 
  headerPanel("Word Complexity Measure"),
  
  # Sidebar panel with inputs 
  sidebarPanel(
    textAreaInput("sample", "Transcript:", placeholder="Paste English orthography transcript here...", height = '250px', width = "100%"),
    DT::dataTableOutput("word_by_word", "auto", "auto")
  )
)

server <- function(input, output) {
  output$word_by_word <- renderDataTable(
    as.data.frame(strsplit(input$sample, "[ ?\r?\n]")),  # each space or newline creates a new entry in DT 
    TRUE
    
  )
}

shinyApp(ui, server)
