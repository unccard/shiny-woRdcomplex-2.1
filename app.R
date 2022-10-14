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
    textAreaInput("specifyKlattese", "Enter alternate/not found in DB transcriptions:", value = "eldridge,EldrIJ\ntejada,tehadx", placeholder="Write comma-separated English,Klattese pairs, with a newline or space between pairs", height="100px"),
    actionButton("submit", "Calculate WCM"), 
    HTML("<hr>"),
    fluidRow("Notes:", 
             HTML(
               "<ul>
                 <li>Your input may be separated by space or newline characters.</li>
                 <li>Use the English,Klattese input for pairs that may not be in the database or that you want to write a specific pronunciation for.</li>
                 <li>This app does not save data between calculations. Be sure to use the download buttons if you need to save your data.</li>
                 <li>Recommended to open downloaded files in a text editor other than Excel, which can't read the Klattese stress marker.</li>
                 <li>For more information on WCM, Zipf Frequency, and our database, refer to our <a href=\"https://github.com/unccard/shiny-woRdcomplex-2.1\">Github</a>.</li>
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
  ), 
  
  title = "Word Complexity Measure"
)

server <- function(input, output) {
  word_db <- read.csv('UNCWordDB-2022-02-07.csv', na.strings=c("", "NA"))
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
    vals$substitutions <- c()  # vector to store any special klattese inputs 
    vals$phon_total <- vals$wf_total <-  vals$words_in_db <- vals$is_nt_contraction <- 0
    vals$has_data <- vals$wbw_row <- 1
    contractions <- c("s","d","t","ve","re","ll")
    
    # process and clean the inputs
    sample <- gsub('[[:punct:] ]+',' ',tolower(input$sample))  # strip punctuation and use lowercase
    english <- strsplit(sample, "[ ?\r?\n]") # split reactive input on any space or newline
    sample_clean <- c()
    for(index in 1:length(english[[1]])) {  # remove empty values from sample 
      if(english[[1]][index] != "") sample_clean <- append(sample_clean, english[[1]][index])
    }
    vals$wbw_english <- sample_clean # store in reactive values 
    if(isTruthy(input$specifyKlattese)) {
      special_klattese <- strsplit(input$specifyKlattese, "[ ?\r?\n]")  # break up the special klattese inputs
      vals$substitutions <- processSpecialInput(vals, special_klattese)  # parse the english and klattese pairs
    }
    # loop through input to gather info on each word
    
    for(index in 1:length(vals$wbw_english)) {  
      if(vals$wbw_english[index] %in% contractions) {  # if this element is a contraction suffix
        next  # skip the contraction suffix 
      }
      
      # Identify --n't contractions
      word <- vals$wbw_english[index]
      next_word <- vals$wbw_english[index+1]
      if(final_phoneme(word) == "n" && next_word == "t") vals$is_nt_contraction = 1
      
      this_word_info <- retrieveDBInfo(vals, word, tibbletest)
      
      if(index <= length(vals$wbw_english)-1) {  # if there is a next element 
        if(vals$wbw_english[index+1] %in% contractions) {  # if the next element is a contraction suffix
          this_word_info <- rescueContraction(vals, this_word_info, index)
        }
      }
      vals$all_word_info <- append(vals$all_word_info, this_word_info)
    }
    
    # set up database outputs 
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
