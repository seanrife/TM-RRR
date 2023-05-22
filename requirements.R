# Function to read in and clean datafiles from each lab
readInMainFile <- function(filename) {
  varNamesToRetain <- c("id", "startlanguage", "essayGroup", "delayGroup", "dvGroup", "labID", "WTMS1", "WTMS2", "WTDP1", "WTDP2", "ARTICLEEVAL[enjoy]", "ARTICLEEVAL[interesting]", "ARTICLEEVAL[recommend]", "ARTICLEEVAL[stay]", "WGTASK[word1_response]", "WGTASK[word2_response]", "WGTASK[word3_response]", "WGTASK[word4_response]", "WGTASK[word5_response]", "WSCTASK[S1_response]", "WSCTASK[S2_response]", "WSCTASK[S3_response]", "WSCTASK[S4_response]", "WSCTASK[S5_response]", "WSCTASK[S6_response]", "WSCTASK[S7_response]", "WSCTASK[S8_response]", "WSCTASK[S9_response]", "WSCTASK[S10_response]", "WSCTASK[S11_response]", "WSCTASK[S12_response]", "WSCTASK[S13_response]", "WSCTASK[S14_response]", "WSCTASK[S15_response]", "WSCTASK[S16_response]", "WSCTASK[S17_response]", "WSCTASK[S18_response]", "WSCTASK[S19_response]", "WSCTASK[S20_response]", "WSCTASK[S21_response]", "WSCTASK[S22_response]", "WSCTASK[S23_response]", "WSCTASK[S24_response]", "WSCTASK[S25_response]", "Gender", "Age", "Purpose", "Understand", "Familiar", "interviewtime", "DelayTime")
  df <- read.csv(filename, header = T, stringsAsFactors = F, check.names=F)
  df$DelayTime <- df[, which(colnames(df)=="ARTICLETime")-1]
  colnames(df)[1] <- "id"
  df <- df[varNamesToRetain]
  # LS places problematic characters in variable names. Remove them.
  names(df) <- gsub(x = names(df),
                    pattern = "\\]",
                    replacement = "")
  names(df) <- gsub(x = names(df),
                    pattern = "\\[",
                    replacement = "_")
  names(df) <- gsub(x = names(df),
                    pattern = "\\_SQ001",
                    replacement = "")
  return(df)
}

# Function to read in exclusions file and return dataframe with exclusions data
readInExclusionsFile <- function(filename) {
  df <- read.csv(filename, header = T, stringsAsFactors = F, check.names=F)
  colnames(df)[1] <- "id"
  return(df)
}


# Function to read in list of death-related words and check for consistency with letters
letter_search <- function(word, language) {
  if (language=="English"){
    letters <- c("c", "o", "b", "u", "r", "e", "s", "a", "t", "k", "i", "l", "d", "h", "p", "l", "m", "g", "v")
  }
  else if (language=="Dutch"){
    letters <- c("m", "r", "o", "d", "l", "j", "i", "k", "s", "t", "e", "g", "r", "f", "a", "v", "o", "d")
  }
  else if (language=="German"){
    letters <- c("c", "t", "r", "f", "l", "i", "d", "e", "g", "h", "t", "b", "o", "l", "e", "s", "a", "f", "n")
  }
  else if (language=="Turkish"){
    letters <- c("m", "e", "g", "ö", "a", "z", "m", "c", "e", "s", "ü", "t", "r", "k", "l", "i̇", "d", "h", "ş")
  }
  else if (language=="Spanish"){
    letters <- c("i", "a", "c", "u", "o", "e", "r", "n", "m", "c", "r", "a", "e", "v", "b", "l", "a", "t", "d")
  }
  else if (language=="Slovak"){
    letters <- c("v", "o", "ť", "s", "p", "k", "b", "d", "z", "i", "o", "l", "ch", "a", "m", "v", "r", "h", "e")
  }
  word_split <- strsplit(word, "")
  match <- TRUE
  for (letter in word_split) {
    for (l in letter) {
      if (l %in% letters) {
        letters <- letters[-match(l, letters)]
      } else {
        match <- FALSE
      }
    }
  }
  return(match)
}

# Function to find death-related words (DV1) and return 1 if found
is_deathword_DV1 <- function(x, language){
  word <- tolower(x)
  word <- gsub(" ", "", word)
  
  if (language=="en") {
    deathwords <- deathwords_English
  }
  if (language=="nl") {
    deathwords <- deathwords_Dutch
  }
  if (language=="de") {
    deathwords <- deathwords_German
  }
  if (language=="tr") {
    deathwords <- deathwords_Turkish
  }
  if (language=="es") {
    deathwords <- deathwords_Spanish
  }
  if (language=="sk") {
    deathwords <- deathwords_Slovak
  }
  
  if (word %in% deathwords){
    return(1)
  }
  else{
    return(0)
  }
}

# Function to find death-related word (DV2) and return 1 if found
is_deathword_DV2 <- function(x, language, index) {
  if (is.na(x)){
    return(0)
  }
  if (language=='en') {
    words <- c("buried", "dead", "grave", "killed", "skull", "coffin")
    word <- tolower(x)
    word <- gsub(" ", "", word)
    if (nchar(word) == 2) {
      if (index == 1 & word == "ie") return(1)
      else if (index == 2 & word == "ad") return(1)
      else if (index == 3 & word == "ve") return(1)
      else if (index == 4 & word == "ll") return(1)
      else if (index == 5 & word == "ul") return(1)
      else if (index == 6 & word == "in") return(1)
      else return(0)
    }
    else if (word == words[index]) {
      return(1)
    }
    else {
      return(0)
    }
  }
  if (language=='nl'){
    words <- c("graf", "lijk", "kist", "doden", "dood", "moord")
    word <- tolower(x)
    word <- gsub(" ", "", word)
    # TODO: fill in two-character responses for this language
    if (nchar(word) == 2) {
      if (index == 1 & word == "af") return(1)
      else if (index == 2 & word == "ij") return(1)
      else if (index == 3 & word == "is") return(1)
      else if (index == 4 & word == "od") return(1)
      else if (index == 5 & word == "od") return(1)
      else if (index == 6 & word == "mo") return(1)
      else return(0)
    }
    if (word == words[index]){
      return(1)
    }
    else {
      return(0)
    }
  }
  if (language=='de'){
    words <- c("sterben", "tot", "tod", "grab", "mord", "leiche", "sarg")
    word <- tolower(x)
    word <- gsub(" ", "", word)
    # TODO: fill in two-character responses for this language
    if (nchar(word) <= 2) {
      if (index == 1 & word == "rb") return(1)
      else if (index == 2 & (word == "t" | word == "d")) return(1)
      else if (index == 3 & word == "b") return(1)
      else if (index == 4 & word == "rd") return(1)
      else if (index == 5 & word == "ch") return(1)
      else if (index == 6 & word == "rg") return(1)
      else return(0)
    }
    if (word == words[index]){
      return(1)
    }
    else {
      return(0)
    }
  }
  if (language=='tr'){
    words <- c("gömmek", "ceset", "mezar", "ölüm", "İskelet", "tabut")
    word <- tolower(x)
    word <- gsub(" ", "", word)
    if (nchar(word) <= 3) {
      if (index == 1 & word == "öm") return(1)
      else if (index == 2 & word == "se") return(1)
      else if (index == 3 & word == "ma") return(1)
      else if (index == 4 & word == "lm") return(1)
      else if (index == 5 & word == "klt") return(1)
      else if (index == 6 & word == "at") return(1)
      else return(0)
    }
    if (word == words[index]){
      return(1)
    }
    else {
      return(0)
    }
  }
  if (language=='es'){
    words <- c("entierro", "muerte", "tumba", "matar", "calavera", "ataúd", "ataud")
    word <- tolower(x)
    word <- gsub(" ", "", word)
    if (nchar(word) == 2) {
      if (index == 1 & word == "rr") return(1)
      else if (index == 2 & word == "rt") return(1)
      else if (index == 3 & word == "mb") return(1)
      else if (index == 4 & word == "ma") return(1)
      else if (index == 5 & word == "ve") return(1)
      else if (index == 6 & (word == "úd" | word == "ud")) return(1)
      else return(0)
    }
    if (word == words[index]){
      return(1)
    }
    else {
      return(0)
    }
  }
  if (language=='sk'){
    words <- c("pochovať", "smrť", "hrob", "zabiť", "vdova", "lebka", "rakva")
    word <- tolower(x)
    word <- gsub(" ", "", word)
    if (nchar(word) == 2) {
      if (index == 1 & word == "ov") return(1)
      else if (index == 2 & word == "mr") return(1)
      else if (index == 3 & word == "ob") return(1)
      else if (index == 4 & word == "zi") return(1)
      else if (index == 5 & word == "vd") return(1)
      else if (index == 6 & word == "eb") return(1)
      # Note: Slovak labs had an extra death-related word
      else if (index == 7 & word == "rk") return(1)
      else return(0)
    }
    if (word == words[index]){
      return(1)
    }
    else {
      return(0)
    }
  }
}


# Function to remove zero variance SE instances from ES/SE vectors
clean_zero_variance <- function(esVec, seVec) {
  out_es <- vector()
  out_se <- vector()
  for (i in 1:length(esVec)) {
    if (seVec[i] > 0) {
      out_es <- c(out_es, esVec[i])
      out_se <- c(out_se, seVec[i])
      }
  }
  return(data.frame(es=out_es, se=out_se))
}