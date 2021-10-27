calculateWCM <- function(klattese) {  # calculate WCM score for the word 
  # phoneme categories 
  engl_voiceless_cons <- c("C","f","h","k","p","s","S","t","T")
  engl_voiced_cons <- c("b","d","D","F","g","J","l","M","m","N","n","G","r","v","w","y","z","Z")  # word final M and N? 
  # engl_syll_cons <- c("L", "M", "N", "R") 
  engl_fricatives <- c("D","f","h","s","S","T","v","z","Z")
  engl_affricates <- c("C","J")
  engl_velars <- c("k","g","G")
  engl_liquids <- c("l","L","r","R","X")
  
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
  if (syllables >= 2) phon_points=phon_points+1  # word patterns (1)
  if (nonInitPrimStress == 1) phon_points=phon_points+1  # word patterns (2)
  
  return(phon_points) 
}

retrieveDBInfo <- function(vals, word, tibbletest) {
  this_word_info <- c()  # contains info for the current word 
  row <- as.integer(which(tibbletest[,1] == word))
  # if(length(row) == 0)  # if word not found in db, handle error 
  if(!identical(toString(tibbletest[row, 2]),"character(0)")) {  # omit words not found in word_db
    this_word_info <- append(this_word_info, toString(tibbletest[row, 1]))  # first element is english word
    this_word_info <- append(this_word_info, toString(tibbletest[row, 2]))  # second element is klattese
    this_word_info <- append(this_word_info, toString(tibbletest[row, 3]))  # third element is wf 
  }
  return(this_word_info)
}

updateWordByWord <- function(vals) {
  for(word in 1:length(vals$all_word_info)) {
    this_word_info <- vals$all_word_info[[word]]
    phon_points <- calculateWCM(vals$this_word_info[[2]])  # calculate wcm using klattese of this word
    
    # store results in word by word df 
    vals$word_by_word[wbw_row, 1] = this_word_info[[1]]  # english orthography of this word 
    vals$word_by_word[wbw_row, 2] = this_word_info[[2]]  # klattese of this word 
    vals$word_by_word[wbw_row, 3] = phon_points
    vals$word_by_word[wbw_row, 4] = this_word_info[[3]]  # word frequency of this word 
    
    vals$wbw_row = vals$wbw_row + 1  # move to next row in the word by word df 
    
    # add data for this word to cumulative total 
    vals$phon_total <- vals$phon_total + phon_points
    vals$wf_total <- vals$wf_total + as.double(vals$wbw_wf[word, 1])
  }
  return(vals$word_by_word)
}

updateAverage <- function(vals) {
  vals$avg_data[1,1] = length(vals$wbw_english_df)
  vals$avg_data[1,2] = length(vals$wbw_found_in_DB_df)
  vals$avg_data[1,3] = vals$phon_total/length(vals$wbw_found_in_DB_df)
  vals$avg_data[1,4] = vals$wf_total/length(vals$wbw_found_in_DB_df)
  return(vals$avg_data)
}

