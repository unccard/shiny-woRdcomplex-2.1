# Shiny App for WCM Script woRdcomplex-2.1

library(shiny)
library(tidyr)
library(DT)
library(stringr)
source("functions.R")

ui <- fluidPage(
  # App title 
  headerPanel("Word Complexity Measure"),
  
  # Sidebar panel with inputs 
  sidebarPanel(
    textAreaInput("sample", "Transcript:", placeholder="Paste English orthography transcript here...", height = '250px', width = "100%"), 
    actionButton("submit", "Calculate WCM")
  ),
  
  mainPanel(
    DT::dataTableOutput("word_by_word", "auto", "auto"), 
    DT::dataTableOutput("average", "auto", "auto")
  )
)

server <- function(input, output) {
  word_db <- read.csv('UNCWordDB-2021-10-08.csv', na.strings=c("", "NA"))
  tibbletest <- tibble(word_db$Word, word_db$KlatteseSyll, word_db$Zipf.value)
  
  vals <- reactiveValues()
  
  vals$avg_data <- data.frame(
    Total_Words_in_Tscript=NA,
    Total_Words_Found_in_DB=NA,
    Avg_WCM_Score=NA,
    Avg_WF_Score=NA
  )
  
  vals$word_by_word <- data.frame(
    English=NA,
    Klattese=NA,
    WCM_Score=NA,
    Word_Frequency=NA
  )
  
  vals$wbw_row <- 1  # keep track of which row of word by word output we are on 
  vals$all_word_info <- c()  # vector where we will track all info for all words 
  
  observeEvent(input$submit,{
    req(input$sample)  # verify input is not empty
    wbw_english <- strsplit(input$sample, "[ ?\r?\n]") # split reactive input on any space or newline 
    for(word in 1:length(wbw_english)) {
      vals$all_word_info <- append(vals$all_word_info, retrieveDBInfo(vals, wbw_english[[1]][word], tibbletest))  # add info from database to collection
    }
    vals$word_by_word <- updateWordByWord(vals)  # perform wcm calculations and store in word by word df 
    vals$avg_data <- updateAverage(vals)  # perform average calculations and store in average df 
  })

  output$word_by_word <- renderDataTable(
    vals$word_by_word, caption = "Word by Word",
    server = TRUE
  )

  output$average <- renderDataTable (
      vals$avg_data, caption = "Average",
      server = TRUE
  )
}

shinyApp(ui, server)
