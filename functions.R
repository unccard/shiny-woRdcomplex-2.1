# This function calculates the WCM score for a Klattese word 
calculateWCM <- function(klattese) { 
  # phoneme categories 
  engl_voiceless_cons <- c("C","f","h","k","p","s","S","t","T")
  engl_voiced_cons <- c("b","d","D","F","g","J","l","M","m","N","n","G","r","v","w","y","z","Z")  # word final M and N? 
  engl_syll_cons <- c("L", "M", "N", "R") 
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
  if (final_phoneme %in% engl_voiced_cons | final_phoneme %in% engl_voiceless_cons | final_phoneme %in% engl_syll_cons) { 
    phon_points=phon_points+1  # syllable structures (1)
  } 
  # if the word has consonant clusters 
  split <- strsplit(klattese, "([iIEe@aWY^cOoUuRx|XLMNR\\ˈ]+|-+)+")  # regular expression to isolate consonants 
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

processSpecialInput <- function(vals, substitutions) {
  for(word in 1:length(substitutions[[1]])) {
    pair <- strsplit(substitutions[[1]][word], ",")
    vals$substitutions <- append(vals$substitutions, pair[[1]][1])
    vals$substitutions <- append(vals$substitutions, pair[[1]][2])
  }
  return(vals$substitutions)
}

# This function verifies that the English orthography is in the database and retrieves its Klattese and wf values 
retrieveDBInfo <- function(vals, word, tibbletest) {
  this_word_info <- c()  # vector to contain info for the current word 
  this_word_info <- append(this_word_info, word)  # first element is English word
  is_nt_contraction <- 0
  
  print(word)
  
  # Look for and handle the special case contractions 
  nt_contractions <- c("couldn" = "could", "shouldn" = "should", "wouldn" = "would", "didn" = "did", "wasn" = "was")
  if(word %in% names(nt_contractions)) {
    print("in nt contract")
    word <- nt_contractions[word]
    is_nt_contraction = 1
  }
  
  row <- as.integer(which(tibbletest[,1] == word))
  if(length(vals$substitutions) > 0 && word %in% vals$substitutions) {  # word has a special klattese input 
    index <- which(vals$substitutions == word)
    this_word_info <- append(this_word_info, vals$substitutions[index+1])
    if(length(row) > 0) { # if word is also in db, use Zipf val 
      this_word_info <- append(this_word_info, toString(tibbletest[row, 3]))  # third element is word frequency
      vals$words_in_db = vals$words_in_db + 1
    } else {
      this_word_info <- append(this_word_info, NA)  # Zipf val is not found 
    }
  } else {  # word does not have special klattese input 
    if(length(row) > 0) {  # if word is in db 
      klatt <- toString(tibbletest[row, 2])
      if(is_nt_contraction) klatt <- paste(klatt, "N", sep="") # Replace the N in nt contractions 
      this_word_info <- append(this_word_info, klatt)  # second element is Klattese
      this_word_info <- append(this_word_info, toString(tibbletest[row, 3]))  # third element is word frequency
      vals$words_in_db = vals$words_in_db + 1
    } else {  # word is not in db 
      this_word_info <- append(this_word_info, NA)  # Klattese is not found
      this_word_info <- append(this_word_info, NA)  # Zipf val is not found 
    }
  }
  print(this_word_info)
  return(this_word_info)
}

rescueContraction <- function(vals, this_word_info, index) {
  isVoiced <- 1
  engl_voiceless_cons <- c("C","f","h","k","p","s","S","t","T")
  engl <- this_word_info[1]
  base <- this_word_info[2]  # base in klattese
  contraction <- vals$wbw_english[index+1]  # english orthography contraction
  print(contraction)
  final_phoneme <- substr(base, str_length(base), str_length(base))  # last sound in base
  if(final_phoneme %in% engl_voiceless_cons) isVoiced <- 0
  # add the correct pronunciation of the contraction to the klattese, and format english 
  if(contraction == "s") {
    engl <- paste(engl, "'s", sep="")
    if(isVoiced == 1 && !(is.na(base))) base <- paste(base, "z", sep="")
    else if(!is.na(base)) base <- paste(base, "s", sep = "")
  } else if(contraction == "d") {
    engl <- paste(engl, "'d", sep="")
    if(!(is.na(base))) base <- paste(base, "d", sep="")
  } else if(contraction == "t") {
    engl <- paste(engl, "'t", sep="")
    if(!(is.na(base))) base <- paste(base, "t", sep="")
  } else if(contraction == "ve") {
    engl <- paste(engl, "'ve", sep="")
    if(!(is.na(base))) base <- paste(base, "v", sep="")
  } 
  else {  # contraction is "ll"
    engl <- paste(engl, "'ll", sep="")
    if(!(is.na(base))) base <- paste(base, "L", sep="")  
  }
  this_word_info[1] = engl
  this_word_info[2] = base
  return(this_word_info)
}

calculateDenominator <- function(vals, col) {
  denom <- 0
  for(val in 1:length(col)) {
    if(!(col[val] == "NA")) denom = denom + 1
  }
  return(denom)
}

# This function concatenates all the data for each word in the data frame 
updateWordByWord <- function(vals) {
  vals$word_by_word <- data.frame(
    English=NA,
    Klattese=NA,
    WCM_Score=NA,
    Zipf_Word_Frequency=NA
  )
  for(word in 1:((length(vals$all_word_info))/3)) {
    english <- vals$all_word_info[[(word*3)-2]]
    klattese <- vals$all_word_info[[(word*3)-1]]
    wf <- as.double(vals$all_word_info[[word*3]])
    phon_points <- 0
    # store results in word by word df 
    vals$word_by_word[vals$wbw_row, 1] = english  
    if(is.na(klattese)) {
      vals$word_by_word[vals$wbw_row, 2] = "NA"  # Klattese is unknown
      vals$word_by_word[vals$wbw_row, 3] = "NA"  # WCM is unknown
      vals$word_by_word[vals$wbw_row, 4] = "NA"  # Zipf val is unknown
    } else {
      vals$word_by_word[vals$wbw_row, 2] = klattese  
      phon_points = calculateWCM(klattese)  # calculate WCM using Klattese of this word
      vals$word_by_word[vals$wbw_row, 3] = toString(phon_points)  # WCM score of this word
      if(is.na(wf)) {
        vals$word_by_word[vals$wbw_row, 4] = "NA"  # Zipf val is unknown
      } else vals$word_by_word[vals$wbw_row, 4] = toString(round(wf, 3))  # word frequency of this word
    }
    # move to next row in the word by word data frame
    vals$wbw_row = vals$wbw_row + 1  
    # add data for this word to cumulative total 
    vals$phon_total = vals$phon_total + phon_points
    if(!is.na(wf)) vals$wf_total = vals$wf_total + wf
  }
  return(vals$word_by_word)
}

# This function concatenates the average data in the data frame 
updateAverage <- function(vals) {
  vals$avg_data <- data.frame(
    Words_in_Script=NA,
    Words_in_DB=NA,
    Avg_WCM_Score=NA,
    Avg_Zipf_WF_Score=NA
  )
  total_words_in_tscript <- vals$wbw_english[! vals$wbw_english %in% c("s", "d", "t", "ve", "ll")]  # remove contracted bits
  vals$avg_data[1,1] = toString(length(total_words_in_tscript))  # Total number of words in the input
  vals$avg_data[1,2] = toString(vals$words_in_db)  # Total number of words found in the database
  if(vals$words_in_db == 0) {  # if no data just display NA
    vals$avg_data[1,3] = "NA"
    vals$avg_data[1,4] = "NA"
  } else {
    phon_denom <- calculateDenominator(vals, vals$word_by_word$WCM_Score)
    wf_denom <- calculateDenominator(vals, vals$word_by_word$Zipf_Word_Frequency)
    vals$avg_data[1,3] = toString(round(vals$phon_total/phon_denom, 3))  # Average WCM score 
    vals$avg_data[1,4] = toString(round(vals$wf_total/wf_denom, 3))  # Average word frequency 
  }
  return(vals$avg_data)
}

