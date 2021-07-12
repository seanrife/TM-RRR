# Analysis for RRR of Trafimow and Hughes (2012), Study 3
# Sean C. Rife / @seanrife / seanrife.com / srife1@murraystate.edu

# Clean house
rm(list = ls())

#### USER-DEFINED VARIABLES ####

# Set base directory
# Uses this to look for main datasets, ratings & exclusions
baseDir <- "F:\\Dropbox\\Research\\TM RRR" # DESKTOP
#baseDir <- "C:\\Users\\srife1\\Dropbox\\Research\\TM RRR" # LAPTOP

setwd(baseDir)

# Set input directory
# Should contain a summary file for all labs (LabInfo.csv)
# Should also contain 3 files for each lab (with lab name appended to the beginning):
#   1. _main.csv - main dataset
#   2. _rating.csv - ratings of the words from each lab
#   3. _exclude - manually-identified exclusions
dataDir <- paste0(baseDir, "\\data")

# Location for output (text and graphs)
outDir <- paste0(baseDir, "\\output")


# CODE BELOW NEED NOT BE MODIFIED

################################
#### READ IN AND CLEAN DATA ####
################################

# Load required libraries
if(!require(metafor)){install.packages('metafor')}
library(metafor)
if(!require(yarrr)){install.packages('yarrr')}
library(yarrr)
if(!require(effsize)){install.packages('effsize')}
library(effsize)
if(!require(plotrix)){install.packages('plotrix')}
library(plotrix)
if(!require(ggplot2)){install.packages('ggplot2')}
library(ggplot2)
if(!require(Cairo)){install.packages('Cairo')}
library(Cairo)
if(!require(tidyverse)){install.packages('tidyverse')}
library(tidyverse)

mergedDF <- data.frame()

# Read in lab info
labInfo <- read.csv(paste0(dataDir,"\\LabInfo.csv"), stringsAsFactors=FALSE)

# Create dataframes for descriptive tables
ORIGINAL_DV1_descriptives <- labInfo
PRIMARY_DV1_descriptives <- labInfo
PRIMARY_DV2_descriptives <- labInfo
SECONDARY_DV1_descriptives <- labInfo
SECONDARY_DV2_descriptives <- labInfo

# Lab names; used for reading in data and labeling
labNames <- as.vector(labInfo$labID)


# ANALYSES INCLUDES THREE APPROACHES:

# ORIGINAL = T&H study exact replication
# original only uses DV1 for consistency w/T&H

# Additional analyses to interrogate (1) basic TM theory and (2) further investigate delay and DTA
# Both DVs are examined in primary and secondary analyses
# PRIMARY = treatment vs. control across delay conditions (collapsing delay condition)
# SECONDARY = treatment delayed vs. treatment no-delay

# delayGroup == 1: delay present
# essayGroup == 1: death group

# Initialize containers for output data
ORIGINAL_DV1_metaVecES <- vector()
ORIGINAL_DV1_metaVecSE <- vector()
ORIGINAL_DV1_metaVecMeanExp <- vector()
ORIGINAL_DV1_metaVecMeanCtrl <- vector()
ORIGINAL_DV1_metaVecR <- vector()
ORIGINAL_DV1_metaVecN <- vector()

PRIMARY_DV1_metaVecES <- vector()
PRIMARY_DV1_metaVecSE <- vector()
PRIMARY_DV1_metaVecMeanExp <- vector()
PRIMARY_DV1_metaVecMeanCtrl <- vector()

PRIMARY_DV2_metaVecES <- vector()
PRIMARY_DV2_metaVecSE <- vector()
PRIMARY_DV2_metaVecMeanExp <- vector()
PRIMARY_DV2_metaVecMeanCtrl <- vector()

SECONDARY_DV1_metaVecES <- vector()
SECONDARY_DV1_metaVecSE <- vector()
SECONDARY_DV1_metaVecMeanExp <- vector()
SECONDARY_DV1_metaVecMeanCtrl <- vector()

SECONDARY_DV2_metaVecES <- vector()
SECONDARY_DV2_metaVecSE <- vector()
SECONDARY_DV2_metaVecMeanExp <- vector()
SECONDARY_DV2_metaVecMeanCtrl <- vector()

# Function to read in and clean datafiles from each lab
readInFile <- function(filename) {
  varNamesToRetain <- c("startlanguage", "essayGroup", "delayGroup", "dvGroup", "labID", "WTMS1", "WTMS2", "WTDP1", "WTDP2", "ARTICLEEVAL[enjoy]", "ARTICLEEVAL[interesting]", "ARTICLEEVAL[recommend]", "ARTICLEEVAL[stay]", "WGTASK[word1_response]", "WGTASK[word2_response]", "WGTASK[word3_response]", "WGTASK[word4_response]", "WGTASK[word5_response]", "WSCTASK[S1_response]", "WSCTASK[S2_response]", "WSCTASK[S3_response]", "WSCTASK[S4_response]", "WSCTASK[S5_response]", "WSCTASK[S6_response]", "WSCTASK[S7_response]", "WSCTASK[S8_response]", "WSCTASK[S9_response]", "WSCTASK[S10_response]", "WSCTASK[S11_response]", "WSCTASK[S12_response]", "WSCTASK[S13_response]", "WSCTASK[S14_response]", "WSCTASK[S15_response]", "WSCTASK[S16_response]", "WSCTASK[S17_response]", "WSCTASK[S18_response]", "WSCTASK[S19_response]", "WSCTASK[S20_response]", "WSCTASK[S21_response]", "WSCTASK[S22_response]", "WSCTASK[S23_response]", "WSCTASK[S24_response]", "WSCTASK[S25_response]", "Gender", "Age", "Purpose", "Understand", "Familiar", "interviewtime", "DelayTime")
  df <- read.csv(filename, header = T, stringsAsFactors = F, check.names=F)
  df$DelayTime <- df[, which(colnames(df)=="ARTICLETime")-1]
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


# Function to read in list of death-related words and check for consistency with letters
letter_search <- function(word, language) {
  if (language=="English"){
    letters <- c("c", "o", "b", "u", "r", "e", "s", "a", "t", "k", "i", "l", "d", "h", "p", "l", "m", "g", "v")
  }
  else if (language=="Dutch"){
    letters <- c("c", "o", "b", "u", "r", "e", "s", "a", "t", "k", "i", "l", "d", "h", "p", "l", "m", "g", "v")
  }
  else if (language=="German"){
    letters <- c("c", "o", "b", "u", "r", "e", "s", "a", "t", "k", "i", "l", "d", "h", "p", "l", "m", "g", "v")
  }
  else if (language=="Turkish"){
    letters <- c("c", "o", "b", "u", "r", "e", "s", "a", "t", "k", "i", "l", "d", "h", "p", "l", "m", "g", "v")
  }
  else if (language=="Spanish"){
    letters <- c("c", "o", "b", "u", "r", "e", "s", "a", "t", "k", "i", "l", "d", "h", "p", "l", "m", "g", "v")
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
  
  if (word %in% deathwords){
    return(1)
  }
  else{
    return(0)
  }
}

is_deathword_DV2 <- function(x, language, index){
  if (is.na(x)){
    return(0)
  }
  if (language=='en') {
    words <- c("buried", "dead", "grave", "killed", "skull", "coffin")
    word <- tolower(x)
    word <- gsub(" ", "", word)
    if (word == words[index]){
      return(1)
    }
    else {
      return(0)
    }
  }
  if (language=='nl'){
    words <- c("begraven", "dood", "graf", "gedood", "schedel", "kist")
    word <- tolower(x)
    word <- gsub(" ", "", word)
    if (word == words[index]){
      return(1)
    }
    else {
      return(0)
    }
  }
  if (language=='de'){
    words <- c("begraben", "tot", "Grab", "getötet", "Schädel", "Sarg")
    word <- tolower(x)
    word <- gsub(" ", "", word)
    if (word == words[index]){
      return(1)
    }
    else {
      return(0)
    }
  }
  if (language=='tr'){
    words <- c("gömülü", "ölü", "mezar", "öldürüldü", "kafatasi", "tabut")
    word <- tolower(x)
    word <- gsub(" ", "", word)
    if (word == words[index]){
      return(1)
    }
    else {
      return(0)
    }
  }
  if (language=='es'){
    words <- c("enterrado", "muerto", "tumba", "asesinado", "cráneo", "ataúd")
    word <- tolower(x)
    word <- gsub(" ", "", word)
    if (word == words[index]){
      return(1)
    }
    else {
      return(0)
    }
  }
}


# Produce lists (by language) of death-related words for DV2

raw_deathwords_English <- scan("DeathWordList_English.txt", what="", sep="\n")
deathwords_English = c()

for (word in raw_deathwords_English) {
  if (letter_search(word, "English")) {
    deathwords_English <- c(deathwords_English, word)
  }
}

raw_deathwords_Dutch <- scan("DeathWordList_Dutch.txt", what="", sep="\n")
deathwords_Dutch = c()

for (word in raw_deathwords_Dutch) {
  if (letter_search(word, "Dutch")) {
    deathwords_Dutch <- c(deathwords_Dutch, word)
  }
}

raw_deathwords_German <- scan("DeathWordList_German.txt", what="", sep="\n")
deathwords_German = c()

for (word in raw_deathwords_German) {
  if (letter_search(word, "German")) {
    deathwords_German <- c(deathwords_German, word)
  }
}

raw_deathwords_Turkish <- scan("DeathWordList_Turkish.txt", what="", sep="\n")
deathwords_Turkish = c()

for (word in raw_deathwords_Turkish) {
  if (letter_search(word, "Turkish")) {
    deathwords_Turkish <- c(deathwords_Turkish, word)
  }
}

raw_deathwords_Spanish <- scan("DeathWordList_Spanish.txt", what="", sep="\n")
deathwords_Spanish = c()

for (word in raw_deathwords_Spanish) {
  if (letter_search(word, "Spanish")) {
    deathwords_Spanish <- c(deathwords_Spanish, word)
  }
}



for (lab in labNames) {

  workingLabPathMain <- paste0(dataDir,"\\",lab,"_main.csv")
  df <- readInFile(workingLabPathMain)


  # Put N info into labInfo DF
  labInfo$N[labInfo$labID == as.factor(lab)] <- nrow(df)
  
  # Get rid of rows with critical missing data
  df <- df[(!is.na(df$Gender) & !is.na(df$Age)),]

  # Exclude flagged cases or those who failed exit interview
  # df <- df[df$Purpose=="",]
  df <- df[!is.na(df$Understand) & df$Understand==0,]
  # df <- df[df$Familiar==0,]
  # Exclude if participant took less than 5 minutes to complete the survey
  df <- df[!is.na(df$interviewtime) & df$interviewtime > 300,]
  
  # Put N info into labInfo DF (with exclusions)
  labInfo$Nused[labInfo$labID == as.factor(lab)] <- nrow(df)

  df$COUNT_DV1_Q1 <- mapply(is_deathword_DV1, df$WGTASK_word1_response, language=df$startlanguage, USE.NAMES=F)
  df$COUNT_DV1_Q2 <- mapply(is_deathword_DV1, df$WGTASK_word2_response, language=df$startlanguage, USE.NAMES=F)
  df$COUNT_DV1_Q3 <- mapply(is_deathword_DV1, df$WGTASK_word3_response, language=df$startlanguage, USE.NAMES=F)
  df$COUNT_DV1_Q4 <- mapply(is_deathword_DV1, df$WGTASK_word4_response, language=df$startlanguage, USE.NAMES=F)
  df$COUNT_DV1_Q5 <- mapply(is_deathword_DV1, df$WGTASK_word5_response, language=df$startlanguage, USE.NAMES=F)
  
  df$COUNT_DV1 <- rowSums(df[, c(which(colnames(df)=="COUNT_DV1_Q1"):which(colnames(df)=="COUNT_DV1_Q5"))], na.rm = FALSE)

  df$COUNT_DV2_Q1 <- mapply(is_deathword_DV2, df$WSCTASK_S1_response, index=1, language=df$startlanguage, USE.NAMES=F)
  df$COUNT_DV2_Q2 <- mapply(is_deathword_DV2, df$WSCTASK_S5_response, index=2, language=df$startlanguage, USE.NAMES=F)
  df$COUNT_DV2_Q3 <- mapply(is_deathword_DV2, df$WSCTASK_S12_response, index=3, language=df$startlanguage, USE.NAMES=F)
  df$COUNT_DV2_Q4 <- mapply(is_deathword_DV2, df$WSCTASK_S15_response, index=4, language=df$startlanguage, USE.NAMES=F)
  df$COUNT_DV2_Q5 <- mapply(is_deathword_DV2, df$WSCTASK_S19_response, index=5, language=df$startlanguage, USE.NAMES=F)
  df$COUNT_DV2_Q6 <- mapply(is_deathword_DV2, df$WSCTASK_S22_response, index=6, language=df$startlanguage, USE.NAMES=F)

  df$COUNT_DV2 <- rowSums(df[, c(which(colnames(df)=="COUNT_DV2_Q1"):which(colnames(df)=="COUNT_DV2_Q6"))], na.rm = FALSE)
  
  
  # Put % female into LabInfo DF
  TotalWithGender <- table(df$Gender)[names(table(df$Gender))==2] + table(df$Gender)[names(table(df$Gender))==1]
  labInfo$percFemale[labInfo$labID == as.factor(lab)] <- format(round(100*(table(df$Gender)[names(table(df$Gender))==2]/TotalWithGender), digits=2))

  # Get mean and SD of age
  labInfo$ageMean[labInfo$labID == as.factor(lab)] <- format(mean(df$Age, na.rm=T))
  labInfo$ageSD[labInfo$labID == as.factor(lab)] <- format(sd(df$Age, na.rm=T))
  
  # Create group identifiers for original experiment
  df$originalExperiment <- 0
  df$originalExperiment[df$essayGroup==1 & df$delayGroup==1] <- 1
  
  df$primaryAnalysis[df$delayGroup==1 & df$essayGroup==1] <- 0
  df$primaryAnalysis[df$delayGroup==2 & df$essayGroup==1] <- 1
  
  # Create group identifiers for secondary analysis
  df$secondaryAnalysis <- 0
  df$secondaryAnalysis[df$essayGroup==1] <- 1
  
  
  # Add to merged dataframe
  mergedDF <- rbind(mergedDF, df)
  
  ## ANALYSES ##
  
  # Using the following variable naming scheme:
  # ORIGINAL_DV1 - primary analysis (exact replication of T&H)
  # PRIMARY_ - primary TM analysis
  # SECONDARY_ - additional analysis of delay effect
  
  # Calculate tests/stats for primary analysis
  ORIGINAL_DV1_m_exp <- mean(df$COUNT_DV1[df$originalExperiment==1])
  ORIGINAL_DV1_sd_exp <- sd(df$COUNT_DV1[df$originalExperiment==1])
  ORIGINAL_DV1_m_ctrl <- mean(df$COUNT_DV1[df$originalExperiment==0])
  ORIGINAL_DV1_sd_ctrl <- sd(df$COUNT_DV1[df$originalExperiment==0])
  ORIGINAL_DV1_r <- cor.test(df$COUNT_DV1, df$DelayTime, na.rm=T)
  ORIGINAL_DV1_se <- std.error(df$COUNT_DV1)
  
  # For descriptive stats tables
  ORIGINAL_DV1_descriptives$mean_exp[ORIGINAL_DV1_descriptives$labID == as.factor(lab)] <- format(ORIGINAL_DV1_m_exp)
  ORIGINAL_DV1_descriptives$sd_exp[ORIGINAL_DV1_descriptives$labID == as.factor(lab)] <- format(ORIGINAL_DV1_sd_exp)
  ORIGINAL_DV1_descriptives$mean_ctrl[ORIGINAL_DV1_descriptives$labID == as.factor(lab)] <- format(ORIGINAL_DV1_m_ctrl)
  ORIGINAL_DV1_descriptives$sd_ctrl[ORIGINAL_DV1_descriptives$labID == as.factor(lab)] <- format(ORIGINAL_DV1_sd_ctrl)
  
  ORIGINAL_DV1_metaVecR <- c(ORIGINAL_DV1_metaVecR, ORIGINAL_DV1_r$estimate)
  ORIGINAL_DV1_metaVecN <- c(ORIGINAL_DV1_metaVecN, (ORIGINAL_DV1_r$parameter + 2))
  
  ORIGINAL_DV1_metaVecES <- c(ORIGINAL_DV1_metaVecES, (ORIGINAL_DV1_m_exp-ORIGINAL_DV1_m_ctrl))
  ORIGINAL_DV1_metaVecSE <- c(ORIGINAL_DV1_metaVecSE, ORIGINAL_DV1_se)
  
  ORIGINAL_DV1_metaVecMeanExp <- c(ORIGINAL_DV1_metaVecMeanExp, ORIGINAL_DV1_m_exp)
  ORIGINAL_DV1_metaVecMeanCtrl <- c(ORIGINAL_DV1_metaVecMeanCtrl, ORIGINAL_DV1_m_ctrl)
  

  # Calculate tests/stats for primary analysis

  PRIMARY_DV1_m_exp <- mean(df$COUNT_DV1[df$primaryAnalysis==1], na.rm=T)
  PRIMARY_DV1_sd_exp <- sd(df$COUNT_DV1[df$primaryAnalysis==1], na.rm=T)
  PRIMARY_DV1_m_ctrl <- mean(df$COUNT_DV1[df$primaryAnalysis==0], na.rm=T)
  PRIMARY_DV1_sd_ctrl <- sd(df$COUNT_DV1[df$primaryAnalysis==0], na.rm=T)
  PRIMARY_DV1_n_exp <- length(df$COUNT_DV1[df$primaryAnalysis==1])
  PRIMARY_DV1_n_ctrl <- length(df$COUNT_DV1[df$primaryAnalysis==0])
  PRIMARY_DV1_d <- cohen.d(df$COUNT_DV1, as.factor(df$primaryAnalysis), na.rm=T)$estimate
  PRIMARY_DV1_d_CI_UPPER <- cohen.d(df$COUNT_DV1, as.factor(df$primaryAnalysis), na.rm=T)$conf.int[2]
  PRIMARY_DV1_d_CI_LOWER <- cohen.d(df$COUNT_DV1, as.factor(df$primaryAnalysis), na.rm=T)$conf.int[1]
  PRIMARY_DV1_se <- std.error(df$COUNT_DV1)
  
  
  # For descriptive stats table
  PRIMARY_DV1_descriptives$mean_exp[PRIMARY_DV1_descriptives$labID == as.factor(lab)] <- format(PRIMARY_DV1_m_exp)
  PRIMARY_DV1_descriptives$sd_exp[PRIMARY_DV1_descriptives$labID == as.factor(lab)] <- format(PRIMARY_DV1_sd_exp)
  PRIMARY_DV1_descriptives$mean_ctrl[PRIMARY_DV1_descriptives$labID == as.factor(lab)] <- format(PRIMARY_DV1_m_ctrl)
  PRIMARY_DV1_descriptives$sd_ctrl[PRIMARY_DV1_descriptives$labID == as.factor(lab)] <- format(PRIMARY_DV1_sd_ctrl)
  
  PRIMARY_DV1_metaVecES <- c(PRIMARY_DV1_metaVecES, (PRIMARY_DV1_d))
  PRIMARY_DV1_metaVecSE <- c(PRIMARY_DV1_metaVecSE, PRIMARY_DV1_se)
  
  PRIMARY_DV1_metaVecMeanExp <- c(PRIMARY_DV1_metaVecMeanExp, PRIMARY_DV1_m_exp)
  PRIMARY_DV1_metaVecMeanCtrl <- c(PRIMARY_DV1_metaVecMeanCtrl, PRIMARY_DV1_m_ctrl)
  
  
  PRIMARY_DV2_m_exp <- mean(df$COUNT_DV2[df$primaryAnalysis==1], na.rm=T)
  PRIMARY_DV2_sd_exp <- sd(df$COUNT_DV2[df$primaryAnalysis==1], na.rm=T)
  PRIMARY_DV2_m_ctrl <- mean(df$COUNT_DV2[df$primaryAnalysis==0], na.rm=T)
  PRIMARY_DV2_sd_ctrl <- sd(df$COUNT_DV2[df$primaryAnalysis==0], na.rm=T)
  PRIMARY_DV2_n_exp <- length(df$COUNT_DV2[df$primaryAnalysis==1])
  PRIMARY_DV2_n_ctrl <- length(df$COUNT_DV2[df$primaryAnalysis==0])
  PRIMARY_DV2_d <- cohen.d(df$COUNT_DV2, as.factor(df$primaryAnalysis), na.rm=T)$estimate
  PRIMARY_DV2_d_CI_UPPER <- cohen.d(df$COUNT_DV2, as.factor(df$primaryAnalysis), na.rm=T)$conf.int[2]
  PRIMARY_DV2_d_CI_LOWER <- cohen.d(df$COUNT_DV2, as.factor(df$primaryAnalysis), na.rm=T)$conf.int[1]
  PRIMARY_DV2_se <- std.error(df$COUNT_DV2)
  
  
  # For descriptive stats table
  PRIMARY_DV2_descriptives$mean_exp[PRIMARY_DV2_descriptives$labID == as.factor(lab)] <- format(PRIMARY_DV2_m_exp)
  PRIMARY_DV2_descriptives$sd_exp[PRIMARY_DV2_descriptives$labID == as.factor(lab)] <- format(PRIMARY_DV2_sd_exp)
  PRIMARY_DV2_descriptives$mean_ctrl[PRIMARY_DV2_descriptives$labID == as.factor(lab)] <- format(PRIMARY_DV2_m_ctrl)
  PRIMARY_DV2_descriptives$sd_ctrl[PRIMARY_DV2_descriptives$labID == as.factor(lab)] <- format(PRIMARY_DV2_sd_ctrl)
  
  PRIMARY_DV2_metaVecES <- c(PRIMARY_DV2_metaVecES, (PRIMARY_DV2_d))
  PRIMARY_DV2_metaVecSE <- c(PRIMARY_DV2_metaVecSE, PRIMARY_DV2_se)
  
  PRIMARY_DV2_metaVecMeanExp <- c(PRIMARY_DV2_metaVecMeanExp, PRIMARY_DV2_m_exp)
  PRIMARY_DV2_metaVecMeanCtrl <- c(PRIMARY_DV2_metaVecMeanCtrl, PRIMARY_DV2_m_ctrl)
  
  
  # Calculate tests/stats for secondary analysis
  # Many of these will be identical to primary analyses,
  # but calculating for the sake of clarity
  SECONDARY_DV1_m_exp <- mean(df$COUNT_DV1[df$secondaryAnalysis==1], na.rm=T)
  SECONDARY_DV1_sd_exp <- sd(df$COUNT_DV1[df$secondaryAnalysis==1], na.rm=T)
  SECONDARY_DV1_m_ctrl <- mean(df$COUNT_DV1[df$secondaryAnalysis==0], na.rm=T)
  SECONDARY_DV1_sd_ctrl <- sd(df$COUNT_DV1[df$secondaryAnalysis==0], na.rm=T)
  SECONDARY_DV1_n_exp <- length(df$COUNT_DV1[df$secondaryAnalysis==1])
  SECONDARY_DV1_n_ctrl <- length(df$COUNT_DV1[df$secondaryAnalysis==0])
  SECONDARY_DV1_d <- cohen.d(df$COUNT_DV1, as.factor(df$secondaryAnalysis), na.rm=T)$estimate
  SECONDARY_DV1_d_CI_UPPER <- cohen.d(df$COUNT_DV1[df$secondaryAnalysis==1 | df$secondaryAnalysis==0], as.factor(df$secondaryAnalysis), na.rm=T)$conf.int[2]
  SECONDARY_DV1_d_CI_LOWER <- cohen.d(df$COUNT_DV1[df$secondaryAnalysis==1 | df$secondaryAnalysis==0], as.factor(df$secondaryAnalysis), na.rm=T)$conf.int[1]
  SECONDARY_DV1_se <- std.error(df$COUNT_DV1[df$secondaryAnalysis==1 | df$secondaryAnalysis==0])
  
  
  # For descriptive stats table
  SECONDARY_DV1_descriptives$mean_exp[SECONDARY_DV1_descriptives$labID == as.factor(lab)] <- format(SECONDARY_DV1_m_exp)
  SECONDARY_DV1_descriptives$sd_exp[SECONDARY_DV1_descriptives$labID == as.factor(lab)] <- format(SECONDARY_DV1_sd_exp)
  SECONDARY_DV1_descriptives$mean_ctrl[SECONDARY_DV1_descriptives$labID == as.factor(lab)] <- format(SECONDARY_DV1_m_ctrl)
  SECONDARY_DV1_descriptives$sd_ctrl[SECONDARY_DV1_descriptives$labID == as.factor(lab)] <- format(SECONDARY_DV1_sd_ctrl)
  
  SECONDARY_DV1_metaVecES <- c(SECONDARY_DV1_metaVecES, (SECONDARY_DV1_d))
  SECONDARY_DV1_metaVecSE <- c(SECONDARY_DV1_metaVecSE, SECONDARY_DV1_se)
  
  SECONDARY_DV1_metaVecMeanExp <- c(SECONDARY_DV1_metaVecMeanExp, SECONDARY_DV1_m_exp)
  SECONDARY_DV1_metaVecMeanCtrl <- c(SECONDARY_DV1_metaVecMeanCtrl, SECONDARY_DV1_m_ctrl)
  
  
  SECONDARY_DV2_m_exp <- mean(df$COUNT_DV2[df$secondaryAnalysis==1], na.rm=T)
  SECONDARY_DV2_sd_exp <- sd(df$COUNT_DV2[df$secondaryAnalysis==1], na.rm=T)
  SECONDARY_DV2_m_ctrl <- mean(df$COUNT_DV2[df$secondaryAnalysis==0], na.rm=T)
  SECONDARY_DV2_sd_ctrl <- sd(df$COUNT_DV2[df$secondaryAnalysis==0], na.rm=T)
  SECONDARY_DV2_n_exp <- length(df$COUNT_DV2[df$secondaryAnalysis==1])
  SECONDARY_DV2_n_ctrl <- length(df$COUNT_DV2[df$secondaryAnalysis==0])
  SECONDARY_DV2_d <- cohen.d(df$COUNT_DV2[df$secondaryAnalysis==1 | df$secondaryAnalysis==0], as.factor(df$secondaryAnalysis), na.rm=T)$estimate
  SECONDARY_DV2_d_CI_UPPER <- cohen.d(df$COUNT_DV2[df$secondaryAnalysis==1 | df$secondaryAnalysis==0], as.factor(df$secondaryAnalysis), na.rm=T)$conf.int[2]
  SECONDARY_DV2_d_CI_LOWER <- cohen.d(df$COUNT_DV2[df$secondaryAnalysis==1 | df$secondaryAnalysis==0], as.factor(df$secondaryAnalysis), na.rm=T)$conf.int[1]
  SECONDARY_DV2_se <- std.error(df$COUNT_DV2[df$secondaryAnalysis==1 | df$secondaryAnalysis==0])
  
  
  # For descriptive stats table
  SECONDARY_DV2_descriptives$mean_exp[SECONDARY_DV2_descriptives$labID == as.factor(lab)] <- format(SECONDARY_DV2_m_exp)
  SECONDARY_DV2_descriptives$sd_exp[SECONDARY_DV2_descriptives$labID == as.factor(lab)] <- format(SECONDARY_DV2_sd_exp)
  SECONDARY_DV2_descriptives$mean_ctrl[SECONDARY_DV2_descriptives$labID == as.factor(lab)] <- format(SECONDARY_DV2_m_ctrl)
  SECONDARY_DV2_descriptives$sd_ctrl[SECONDARY_DV2_descriptives$labID == as.factor(lab)] <- format(SECONDARY_DV2_sd_ctrl)
  
  SECONDARY_DV2_metaVecES <- c(SECONDARY_DV2_metaVecES, (SECONDARY_DV2_d))
  SECONDARY_DV2_metaVecSE <- c(SECONDARY_DV2_metaVecSE, SECONDARY_DV2_se)
  
  SECONDARY_DV2_metaVecMeanExp <- c(SECONDARY_DV2_metaVecMeanExp, SECONDARY_DV2_m_exp)
  SECONDARY_DV2_metaVecMeanCtrl <- c(SECONDARY_DV2_metaVecMeanCtrl, SECONDARY_DV2_m_ctrl)

}


######################################
#### RUN ANALYSES & CREATE GRAPHS ####
######################################

#### REPLICATION ANALYSES ####

# Create descriptive statistics tables
write.csv(ORIGINAL_DV1_descriptives, paste0(outDir, "\\ORIGINAL_WG_descriptives.csv"), row.names = F)
write.csv(PRIMARY_DV1_descriptives, paste0(outDir, "\\PRIMARY_WG_descriptives.csv"), row.names = F)
write.csv(PRIMARY_DV2_descriptives, paste0(outDir, "\\PRIMARY_WC_descriptives.csv"), row.names = F)
write.csv(SECONDARY_DV1_descriptives, paste0(outDir, "\\SECONDARY_WG_descriptives.csv"), row.names = F)
write.csv(SECONDARY_DV2_descriptives, paste0(outDir, "\\SECONDARY_WC_descriptives.csv"), row.names = F)
write.csv(labInfo, paste0(outDir, "\\labDescriptives.csv"), row.names = F)

# Meta analysis
sink(paste0(outDir, "\\ma-original-WG-bg.txt"))
metaRAW_DV1 <- rma.uni(yi = ORIGINAL_DV1_metaVecES, sei = ORIGINAL_DV1_metaVecSE)
summary(metaRAW_DV1)
sink()

# Meta analysis
es <- escalc(measure="COR", ri=ORIGINAL_DV1_metaVecR, ni=ORIGINAL_DV1_metaVecN)
sink(paste0(outDir, "\\ma-original-WG-cont.txt"))
metaR_DV1 <- rma.uni(es)
summary(metaR_DV1)
sink()


# Forest plot

# Raw ES of original study
THes <- .94-.58
# SE estimate using pooled SD from original study
THse <- sqrt(((1.21^2)+(.67^2)+(.73^2)+(.67^2))/4)/sqrt(120)

# Forest plot code snipped from Wagenmakers et al. (2016)

# Forest plot with word generation dv

Cairo(file=paste0(outDir, "\\forest_original_WG.png"), 
      bg="white",
      type="png",
      units="in", 
      width=11, height=7, 
      #pointsize=12, 
      dpi=600)


forest(x = c(THes, ORIGINAL_DV1_metaVecES), sei = c(THse, ORIGINAL_DV1_metaVecSE), xlab="Mean difference", cex.lab=1.4,
       ilab=cbind(c(".94", format(round(ORIGINAL_DV1_metaVecMeanExp, digits=2))), c(".58", format(round(ORIGINAL_DV1_metaVecMeanCtrl, digits=2)))),
       ilab.xpos=c(grconvertX(.18, from = "ndc", "user"),
                   grconvertX(.28, from = "ndc", "user")), cex.axis=1.1, lwd=1.4,
       rows=c(length(labNames)+7, (length(labNames)+2):3), ylim=c(-2, length(labNames)+11),
       slab = c("Original Study", labNames))

abline(h=length(labNames)+5, lwd=1.4)
text(grconvertX(.019, from = "ndc", "user"), length(labNames)+3.75, "RRR Studies", cex=1.2, pos = 4)
text(grconvertX(.053, from = "ndc", "user"), length(labNames)+10, "Study", cex=1.2)
text(grconvertX(.18, from = "ndc", "user"), length(labNames)+10, "Delay", cex=1.2)
text(grconvertX(.28, from = "ndc", "user"), length(labNames)+10, "Other Cond.", cex=1.2)
text(grconvertX(.875, from = "ndc", "user"), length(labNames)+10, paste0("Mean difference", " [95% CI]"), cex=1.2)

abline(h=1, lwd=1.4)
addpoly(metaRAW_DV1, atransf=FALSE, row=-1, cex=1.3, mlab="Meta-Analytic Effect Size:")

dev.off()



# Line graphs of each lab's findings

all_linear <- ggplot(mergedDF, aes(x = DelayTime, y = COUNT_DV1, group = labID)) +
  labs(x = "Delay Time", y = "Word Count") +
  geom_smooth(method = "loess", se = T, color = "darkgrey") +
  geom_point(alpha = 0.3, size = 0) +
  facet_wrap(~labID, scales = "free")

ggsave(paste0(outDir, "\\ORIGINAL_WG_line-graphs.png"))

all_linear <- ggplot(mergedDF, aes(x = DelayTime, y = COUNT_DV2, group = labID)) +
  labs(x = "Delay Time", y = "Word Count") +
  geom_smooth(method = "loess", se = T, color = "darkgrey") +
  geom_point(alpha = 0.3, size = 0) +
  facet_wrap(~labID, scales = "free")

ggsave(paste0(outDir, "\\ORIGINAL_WC_line-graphs.png"))



#### PRIMARY ANALYSES ####

# Meta analysis
sink(paste0(outDir, "\\ma-primary-WG.txt"))
primary_DV1_meta <- rma.uni(yi = PRIMARY_DV1_metaVecES, sei = PRIMARY_DV1_metaVecSE)
summary(primary_DV1_meta)
sink()

# Meta analysis
sink(paste0(outDir, "\\ma-primary-WC.txt"))
primary_DV2_meta <- rma.uni(yi = PRIMARY_DV2_metaVecES, sei = PRIMARY_DV2_metaVecSE)
summary(primary_DV2_meta)
sink()


# Forest plot code snipped from Wagenmakers et al. (2016)

# Word generation dv

Cairo(file=paste0(outDir, "\\forest_primary_WG.png"), 
      bg="white",
      type="png",
      units="in", 
      width=11, height=7, 
      #pointsize=12, 
      dpi=600)


forest(x = PRIMARY_DV1_metaVecES, sei = PRIMARY_DV1_metaVecSE, xlab="Mean difference", cex.lab=1.4,
       ilab=cbind(format(round(PRIMARY_DV1_metaVecMeanExp, digits=2)), format(round(PRIMARY_DV1_metaVecMeanCtrl, digits=2))),
       ilab.xpos=c(grconvertX(.18, from = "ndc", "user"),
                   grconvertX(.28, from = "ndc", "user")), cex.axis=1.1, lwd=1.4,
       ylim=c(-2, length(labNames)+3),
       slab = labNames)

text(grconvertX(.053, from = "ndc", "user"), length(labNames)+2, "Study", cex=1.2)
text(grconvertX(.18, from = "ndc", "user"), length(labNames)+2, "Death", cex=1.2)
text(grconvertX(.28, from = "ndc", "user"), length(labNames)+2, "Dental Pain", cex=1.2)
text(grconvertX(.875, from = "ndc", "user"), length(labNames)+2, paste0("Mean difference", " [95% CI]"), cex=1.2)

abline(h=0, lwd=1.4)
addpoly(primary_DV1_meta, atransf=FALSE, row=-1, cex=1.3, mlab="Meta-Analytic Effect Size:")

dev.off()

# Word completion dv

Cairo(file=paste0(outDir, "\\forest_primary_WC.png"), 
      bg="white",
      type="png",
      units="in", 
      width=11, height=7, 
      #pointsize=12, 
      dpi=600)


forest(x = PRIMARY_DV2_metaVecES, sei = PRIMARY_DV2_metaVecSE, xlab="Mean difference", cex.lab=1.4,
       ilab=cbind(format(round(PRIMARY_DV2_metaVecMeanExp, digits=2)), format(round(PRIMARY_DV2_metaVecMeanCtrl, digits=2))),
       ilab.xpos=c(grconvertX(.18, from = "ndc", "user"),
                   grconvertX(.28, from = "ndc", "user")), cex.axis=1.1, lwd=1.4,
       ylim=c(-2, length(labNames)+3),
       slab = labNames)

text(grconvertX(.053, from = "ndc", "user"), length(labNames)+2, "Study", cex=1.2)
text(grconvertX(.18, from = "ndc", "user"), length(labNames)+2, "Death", cex=1.2)
text(grconvertX(.28, from = "ndc", "user"), length(labNames)+2, "Dental Pain", cex=1.2)
text(grconvertX(.875, from = "ndc", "user"), length(labNames)+2, paste0("Mean difference", " [95% CI]"), cex=1.2)

abline(h=0, lwd=1.4)
addpoly(primary_DV2_meta, atransf=FALSE, row=-1, cex=1.3, mlab="Meta-Analytic Effect Size:")

dev.off()


#### SECONDARY ANALYSES ####

# Meta analysis
sink(paste0(outDir, "\\ma-secondary-WG.txt"))
secondary_DV1_meta <- rma.uni(yi = SECONDARY_DV1_metaVecES, sei = SECONDARY_DV1_metaVecSE)
summary(secondary_DV1_meta)
sink()

# Meta analysis
sink(paste0(outDir, "\\ma-secondary-WC.txt"))
secondary_DV2_meta <- rma.uni(yi = SECONDARY_DV2_metaVecES, sei = SECONDARY_DV2_metaVecSE)
summary(secondary_DV2_meta)
sink()


# Forest plot code snipped from Wagenmakers et al. (2016)

# Word generation dv

Cairo(file=paste0(outDir, "\\forest_secondary_WG.png"), 
      bg="white",
      type="png",
      units="in", 
      width=11, height=7, 
      #pointsize=12, 
      dpi=600)


forest(x = SECONDARY_DV1_metaVecES, sei = SECONDARY_DV1_metaVecSE, xlab="Mean difference", cex.lab=1.4,
       ilab=cbind(format(round(SECONDARY_DV1_metaVecMeanExp, digits=2)), format(round(SECONDARY_DV1_metaVecMeanCtrl, digits=2))),
       ilab.xpos=c(grconvertX(.18, from = "ndc", "user"),
                   grconvertX(.28, from = "ndc", "user")), cex.axis=1.1, lwd=1.4,
       ylim=c(-2, length(labNames)+3),
       slab = labNames)

text(grconvertX(.053, from = "ndc", "user"), length(labNames)+2, "Study", cex=1.2)
text(grconvertX(.18, from = "ndc", "user"), length(labNames)+2, "Delay", cex=1.2)
text(grconvertX(.28, from = "ndc", "user"), length(labNames)+2, "No Delay", cex=1.2)
text(grconvertX(.875, from = "ndc", "user"), length(labNames)+2, paste0("Mean difference", " [95% CI]"), cex=1.2)

abline(h=0, lwd=1.4)
addpoly(secondary_DV1_meta, atransf=FALSE, row=-1, cex=1.3, mlab="Meta-Analytic Effect Size:")

dev.off()

# Word completion dv

Cairo(file=paste0(outDir, "\\forest_secondary_WC.png"), 
      bg="white",
      type="png",
      units="in", 
      width=11, height=7, 
      #pointsize=12, 
      dpi=600)


forest(x = SECONDARY_DV2_metaVecES, sei = SECONDARY_DV2_metaVecSE, xlab="Mean difference", cex.lab=1.4,
       ilab=cbind(format(round(SECONDARY_DV2_metaVecMeanExp, digits=2)), format(round(SECONDARY_DV2_metaVecMeanCtrl, digits=2))),
       ilab.xpos=c(grconvertX(.18, from = "ndc", "user"),
                   grconvertX(.28, from = "ndc", "user")), cex.axis=1.1, lwd=1.4,
       ylim=c(-2, length(labNames)+3),
       slab = labNames)

text(grconvertX(.053, from = "ndc", "user"), length(labNames)+2, "Study", cex=1.2)
text(grconvertX(.18, from = "ndc", "user"), length(labNames)+2, "Delay", cex=1.2)
text(grconvertX(.28, from = "ndc", "user"), length(labNames)+2, "No Delay", cex=1.2)
text(grconvertX(.875, from = "ndc", "user"), length(labNames)+2, paste0("Mean difference", " [95% CI]"), cex=1.2)

abline(h=0, lwd=1.4)
addpoly(secondary_DV2_meta, atransf=FALSE, row=-1, cex=1.3, mlab="Meta-Analytic Effect Size:")

dev.off()

