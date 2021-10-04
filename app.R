# Shiny App for WCM Script woRdcomplex-2.1

library(shiny)
library(tidyr)
library(DT)
library(stringr)

calculateWCM <- function(klattese) {  # calculate WCM score for the word 
  phon_points <- 0 
  syllables <- 1
  nonInitPrimStress <- 0
  
  # if the word ends in a consonant 
  len <- str_length(klattese)
  final_phoneme <- substr(klattese, len, len)
  if (final_phoneme %in% engl_voiced_cons | final_phoneme %in% engl_voiceless_cons) { 
    phon_points=phon_points+1  # syllable structures (1)
  } 
  
  # if the word has consonant clusters 
  split <- strsplit(klattese, "([iIEe@aWY^cOoUuRx|X\\ˈ]+|-+)+")  # regular expression to isolate consonants 
  for(i in 1:length(split[[1]])) {
    if(str_length(split[[1]][i]) > 1) { 
      phon_points = phon_points + 1  # syllable structures (2)
    }
  }
  
  # for loop to assign points for sound classes, and find stress and syllables 
  for (i in 1:str_length(klattese)) {
    phoneme <- substr(klattese, i, i)
    if(phoneme == '-') syllables=syllables+1
    if(phoneme == 'ˈ' && syllables >= 2) nonInitPrimStress = 1
    # WCM rules for sound classes 
    if (phoneme %in% engl_velars) phon_points=phon_points+1  # sound classes (1)
    if (phoneme %in% engl_liquids) phon_points=phon_points+1  # sound classes (2)
    if (phoneme %in% engl_fricatives | phoneme %in% engl_affricates) {
      phon_points=phon_points+1  # sound classes (3)
      if (phoneme %in% engl_voiced_cons) {
        phon_points=phon_points+1  # sound classes (4)
      }
    }
  }
  # WCM rules for word patterns 
  if (syllables > 2) phon_points=phon_points+1  # word patterns (1)
  if (nonInitPrimStress == 1) phon_points=phon_points+1  # word patterns (2)
  
  return(phon_points) 
}

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
  word_db <- read.csv('/Users/lindsaygreene/Desktop/programming/woRdcomplex-2.1/UNCCombWordDB.csv', na.strings=c("", "NA"))
  tibbletest <-tibble(word_db$word, word_db$phon_klattese, word_db$SUBTLWF0to10)
  
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
  phon_total <- wf_total <- 0 
  wbw_found_in_DB <- wbw_klattese <- wbw_wf <- c()
  
  output_wbw <- eventReactive(input$submit, {
    runif(input$sample)
  })
  
  # split reactive input on any space or newline 
  wbw_english <- reactive(strsplit(input$sample, "[ ?\r?\n]"))
  
  # retrieve information from word db 
  for(english in 1:length(wbw_english)) {
    word <- toString(text_df[i,1])
    row <- which(tibbletest[,1] == word)
    if(!identical(toString(tibbletest[row, 2]),"character(0)")){  # omit words not found in word_db
      wbw_found_in_DB <- append(wbw_found_in_DB, toString(tibbletest[row, 1]))
      wbw_klattese <- append(wbw_klattese, toString(tibbletest[row, 2]))
      wbw_wf <- append(wbw_wf, toString(tibbletest[row, 3]))
    }
  }
  
  # transform vectors into data frames
  as.data.frame(wbw_found_in_DB)
  as.data.frame(wbw_klattese)
  as.data.frame(wbw_wf)
  
  # calculate wcm for each word 
  for(word in 1:length(wbw_found_in_DB)) {
    klattese <- wbw_klattese[word, 1]
    phon_points <- calculateWCM(klattese)
    
    # store results in wbw df 
    word_by_word[wbw_row, 1] = wbw_found_in_DB[word, 1]
    word_by_word[wbw_row, 2] = klattese
    word_by_word[wbw_row, 3] = phon_points
    word_by_word[wbw_row, 4] = wbw_wf[word, 1]
    
    wbw_row = wbw_row + 1  # move to next row in the word by word df 
    
    # add data for this word to cumulative total 
    phon_total = phon_total + phon_points
    wf_total = wf_total + wbw_wf[word, 1]
  }
  
  data[1,1] = length(wbw_english)
  data[1,2] = length(wbw_found_in_DB)
  data[1,3] = phon_total/nrow(wbw_found_in_DB)
  data[1,4] = wf_total/nrow(wbw_wf)
  
  output$word_by_word <- renderDataTable(
    word_by_word, TRUE
  )
  
  output$average <- renderDataTable (
    data, TRUE
  )
}

shinyApp(ui, server)
