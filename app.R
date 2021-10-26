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

  # ensure at least one entry, then perform calculations 
  output$word_by_word <- reactive({
    
    req(input$sample)  # verify input is not empty 
    
    # split reactive input on any space or newline 
    wbw_english <- strsplit(input$sample, "[ ?\r?\n]")
    wbw_english_length = length(wbw_english)
    
    # retrieve information from word db 
    for(i in 1:wbw_english_length) {
      word <- wbw_english[[1]][i]
      row <- which(tibbletest[,1] == word)
      if(!identical(toString(tibbletest[row, 2]),"character(0)")) {  # omit words not found in word_db
        wbw_found_in_DB <- append(wbw_found_in_DB, toString(tibbletest[row, 1]))
        wbw_klattese <- append(wbw_klattese, toString(tibbletest[row, 2]))
        wbw_wf <- append(wbw_wf, toString(tibbletest[row, 3]))
      }
    }
    
    # transform vectors into data frames
    as.data.frame(wbw_found_in_DB)
    as.data.frame(wbw_klattese)
    as.data.frame(wbw_wf)
    
    wbw_found_in_db_length = length(wbw_found_in_DB)
    
    # calculate wcm for each word 
    for(word in 1:wbw_found_in_db_length) {
      klattese <- wbw_klattese[word, 1]
      phon_points <- calculateWCM(klattese)
      
      # store results in word by word df 
      word_by_word[wbw_row, 1] = wbw_found_in_DB[word, 1]
      word_by_word[wbw_row, 2] = klattese
      word_by_word[wbw_row, 3] = phon_points
      word_by_word[wbw_row, 4] = wbw_wf[word, 1]
      
      wbw_row = wbw_row + 1  # move to next row in the word by word df 
      
      # add data for this word to cumulative total 
      phon_total = phon_total + phon_points
      wf_total = wf_total + wbw_wf[word, 1]
    }
    
    output$word_by_word <- renderDataTable(
      word_by_word, TRUE
    )
  })
  
  output$average <- reactive({
    req(input$word_by_word)
    
    data[1,1] = wbw_english_length
    data[1,2] = wbw_found_in_db_length
    data[1,3] = phon_total/wbw_found_in_db_length
    data[1,4] = wf_total/wbw_found_in_db_length
    
    output$average <- renderDataTable (
      data, TRUE
    )
  })
  
}

shinyApp(ui, server)
