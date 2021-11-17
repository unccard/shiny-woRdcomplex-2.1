# Shiny App for WCM Script woRdcomplex-2.1

library(shiny)
library(tidyr)
library(DT)
library(stringr)
source("functions.R")

ui <- fluidPage(
  
  navbarPage(
    tags$a(href="https://www.med.unc.edu/ahs/sphs/card/", "UNC Center for Aphasia and Related Disorders")
  ),
  
  # App title 
  headerPanel("Word Complexity Measure"),
  
  # Sidebar panel with inputs 
  sidebarPanel(
    textAreaInput("sample", "Transcript:", placeholder="Paste English orthography transcript here...", height = '250px', width = "100%"), 
    actionButton("submit", "Calculate WCM"), 
    HTML("<hr>"),
    fluidRow("Notes:", 
             HTML(
               "<ul>
                 <li>Your input may be separated by space or newline characters.</li>
                 <li>This app does not save data between calculations. Be sure to use the download buttons if you need to save your data.</li>
                 <li>Recommended to open downloaded files in a text editor other than Excel, which can't read the Klattese stress marker.</li>
                 <li>For more information on WCM, Zipf Frequency, and our database, refer to our <a href=\"https://github.com/unccard\">GitHub</a>.</li>
               </ul>"
             ))
  ),
  
  # Main panel with outputs
  mainPanel(
    DT::dataTableOutput("word_by_word", "auto", "auto"), 
    downloadButton("downloadWBW", "Download"), 
    HTML("<hr>"),
    DT::dataTableOutput("average", "auto", "auto"),
    downloadButton("downloadAVG", "Download"),
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
    vals$phon_total <- vals$wf_total <-  vals$is_contraction <- vals$words_in_db <- 0
    vals$has_data <- vals$wbw_row <- 1
    sample <- gsub('[[:punct:] ]+',' ',tolower(input$sample))  # strip punctuation and use lowercase
    english <- strsplit(sample, "[ ?\r?\n]") # split reactive input on any space or newline
    sample_clean <- c()
    for(word in 1:length(english[[1]])) {  # remove empty values from sample 
      if(english[[1]][word] != "") sample_clean <- append(sample_clean, english[[1]][word])
    }
    vals$wbw_english <- sample_clean # store in reactive values 
    for(word in 1:length(vals$wbw_english)) {  # loop through input to gather info on each word
      if(vals$is_contraction == 1) {  # if this is a contracted bit 
        vals$is_contraction = 0  # reset the flag 
        next  # skip the contraction 
      }
      this_word_info <- retrieveDBInfo(vals, vals$wbw_english[word], tibbletest)
      print(vals$wbw_english[word])
      # if(length(this_word_info) < 3) {  # if the word has no entry in the database
      #   # word not found do stuff
      # }
      if(word <= length(vals$wbw_english)-1) {  # if there is a next element 
        if(vals$wbw_english[word+1] %in% c("s", "d", "ve", "ll")) {  # if the element is a contraction 
          vals$is_contraction = 1
          this_word_info <- rescueContraction(vals, this_word_info, word)
        }
      }
      vals$all_word_info <- append(vals$all_word_info, this_word_info)
    }
    # pass in flag
    vals$word_by_word <- updateWordByWord(vals)  # perform word by word calculations and store in wbw df
    vals$avg_data <- updateAverage(vals)  # perform average calculations and store in average df
    
    
    if(nrow(vals$word_by_word) > 0) {
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
    } else {
      output$word_by_word <- renderText("No table to display")
    }
  })
  
}

shinyApp(ui, server)
