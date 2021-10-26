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
    textAreaInput("sample", "Transcript:", placeholder="Paste English orthography transcript here...", height = '250px', width = "100%")
  ),
  
  actionButton("submit", "Calculate WCM"),
  
  mainPanel(
    DT::dataTableOutput("word_by_word", "auto", "auto"), 
    DT::dataTableOutput("average", "auto", "auto")
  )
)

server <- function(input, output) {
  word_db <- read.csv('UNCWordDB-2021-10-08.csv', na.strings=c("", "NA"))
  tibbletest <-tibble(word_db$Word, word_db$KlatteseSyll, word_db$Zipf.value)
  
  # set up data frame to store average results  
  data <- data.frame(matrix(vector(), ncol=4))  # data frame to store avg output  
  header_names <- list("Total_Words_in_Tscript", "Total_Words_Found_in_DB","Avg_WCM_Score","Avg_WF_Score")  # column headers for avg output df 
  colnames(data) <- header_names
  
  # set up data frame to store word by word results 
  word_by_word <- data.frame(matrix(vector(), ncol=4))  # data frame to store info ab individual words from each transcript
  names <- list("English", "Klattese", "WCM_Score", "Word_Frequency")  # column headers for word by word df 
  colnames(word_by_word) <- names
  wbw_row = 1  # count number of rows in word by word db 
  
  # initialize cumulative points & vectors for each file 
  #phon_total <- wf_total <- wbw_english_length <- wbw_found_in_db_length <- 0 
  #wbw_found_in_DB <- wbw_klattese <- wbw_wf <- c()
  
  vals <- reactiveValues()
  
  observeEvent(input$submit,{
    req(input$sample)  # verify input is not empty
    vals$wbw_english <- strsplit(input$sample, "[ ?\r?\n]") # split reactive input on any space or newline 
    retrieveDBInfo(vals)  # add info from database to collection
    asDataFrame(vals) # transform reactive vectors into data frames 
    updateWordByWord(vals, word_by_word, wbw_row)  # perform wcm calculations and store in word by word df 
    updateAverage(vals, data)  # perform average calculations and store in average df 
  })

  output$word_by_word <- renderDataTable(
    word_by_word, TRUE
  )

  output$average <- renderDataTable (
      data, TRUE
  )
}

shinyApp(ui, server)
