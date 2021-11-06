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
  
  # Main panel with outputs
  mainPanel(
    DT::dataTableOutput("word_by_word", "auto", "auto"), 
    downloadButton("downloadWBW", "Download"), 
    DT::dataTableOutput("average", "auto", "auto"), 
    downloadButton("downloadAVG", "Download")
  )
)

server <- function(input, output) {
  word_db <- read.csv('UNCWordDB-2021-10-08.csv', na.strings=c("", "NA"))
  tibbletest <- tibble(word_db$Word, word_db$KlatteseSyll, word_db$Zipf.value)
  
  # Stores all values that are changed during the program 
  # Index anything stored here using vals$var_name
  vals <- reactiveValues()
  
  # When the submit button is clicked... 
  observeEvent(input$submit,{
    req(input$sample)  # verify input is not empty
    # initialize reactive values
    vals$wbw_english <- c()  # clear previous inputs before adding new 
    vals$all_word_info <- c()  # vector where we will track all info for all words
    vals$phon_total <- vals$wf_total <- 0 
    vals$wbw_row <- 1  # keep track of which row of word by word output we are on
    # clean data and perform calculations 
    sample <- gsub('[[:punct:] ]+',' ',tolower(input$sample))  # strip punctuation and use lowercase
    english <- strsplit(sample, "[ ?\r?\n]") # split reactive input on any space or newline
    sample_clean <- c()
    for(word in 1:length(english[[1]])) {  # remove empty values from sample 
      if(english[[1]][word] != "") sample_clean <- append(sample_clean, english[[1]][word])
    }
    vals$wbw_english <- sample_clean  # store in reactive values 
    for(word in 1:length(vals$wbw_english)) {  # loop through input to gather info on each word
      this_word_info <- retrieveDBInfo(vals, vals$wbw_english[word], tibbletest)
      vals$all_word_info <- append(vals$all_word_info, this_word_info)
    }
    print("retrieve Db Info success")
    vals$word_by_word <- updateWordByWord(vals)  # perform word by word calculations and store in wbw df
    print("update word by word success")
    vals$avg_data <- updateAverage(vals)  # perform average calculations and store in average df
    print("update average data success")
  })

  # display the word by word output
  output$word_by_word <- renderDataTable(
    vals$word_by_word, caption = "Word by Word",
    server = TRUE
  )

  # display the average output
  output$average <- renderDataTable (
      vals$avg_data, caption = "Average",
      server = TRUE
  )
  
  output$downloadWBW <- downloadHandler(
    filename = function() {
      "word_by_word.csv"
    },
    content = function(file) {
      write.csv(vals$word_by_word, file)
    }
  )
  
  output$downloadAVG <- downloadHandler(
    filename = function() {
      "avg_data.csv"
    },
    content = function(file) {
      write.csv(vals$avg_data, file)
    }
  )
  
}

shinyApp(ui, server)
