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
    DT::dataTableOutput("word_by_word", "auto", "auto")
  )
)

server <- function(input, output) {
  word_db <- read.csv('/Users/lindsaygreene/Desktop/programming/woRdcomplex-2.1/UNCCombWordDB.csv', na.strings=c("", "NA"))
  tibbletest <-tibble(word_db$word, word_db$phon_klattese)
  
  output_wbw <- eventReactive(input$submit, {
    runif(input$sample)
  })
  
  wbw_english <- reactive(strsplit(input$sample, "[ ?\r?\n]"))
  wbw_klattese <- c()
  for(english in 1:length(wbw_english)) {
    for(i in 1:nrow(text_df)) {
      word <- toString(text_df[i,1])
      row <- which(tibbletest[,1] == word)
      if(!identical(toString(tibbletest[row, 2]),"character(0)")){  # omit words not found in word_db
        wbw_klattese <- append(wbw_klattese, toString(tibbletest[row, 2]))
      }
    }
  }
  wbw_vectors <- c(wbw_english, wbw_klattese)
  output$word_by_word <- renderDataTable(
    #as.data.frame(strsplit(input$sample, "[ ?\r?\n]")),  # each space or newline creates a new entry in DT 
    as.data.frame(wbw_vectors),
    TRUE
    
  )
}

shinyApp(ui, server)
