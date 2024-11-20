# Analysis for RRR of Trafimow and Hughes (2012), Study 3
# Sean C. Rife / @seanrife / seanrife.com / srife1@murraystate.edu


# Clean house
#switch(menu(c("Yes", "No"), title="This script creates a bunch of new objects. To make things simpler, we clear the current environment of any existing objects. Is that cool or nah?"),
#rm(list = ls()), cat("Okey-dokey\n"))



#### USER-DEFINED VARIABLES ####

# Set base directory
# Uses this to look for main datasets, ratings & exclusions
baseDir <- "H:/Dropbox/Research/TM RRR" # DESKTOP
#baseDir <- "C:/Users/srife1/Dropbox/Research/TM RRR" # OFFICE
#baseDir <- "/home/sean/Dropbox/Research/TM RRR" # XPS13
#baseDir <- "C:/Users/Sean/Dropbox/Research/TM RRR"

setwd(baseDir)

# Set input directory
# Should contain 2 files for each lab (with lab name appended to the beginning):
#   1. _main.csv - main dataset
#   2. _coding_completed_normalized.csv - manually-identified exclusions
dataDir <- paste0(baseDir, "/data")

# Location for output (text and graphs)
outDir <- paste0(baseDir, "/output")

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
if(!require(statpsych)){install.packages('statpsych')}
library(statpsych)
#if(!require(devtools)){install.packages('devtools')}
#library(devtools)
#devtools::install_github('rcalinjageman/esci')
#library(esci)

mergedDF <- data.frame()

# Read in lab info
labInfo <- read.csv(paste0(baseDir,"/LabInfo.csv"), stringsAsFactors=FALSE)

# Create dataframes for descriptive tables
ORIGINAL_DV1_descriptives <- labInfo
PRIMARY_DV1_descriptives <- labInfo
PRIMARY_DV2_descriptives <- labInfo
SECONDARY_DV1_descriptives <- labInfo
SECONDARY_DV2_descriptives <- labInfo

# Lab names; used for reading in data
labIDs <- as.vector(labInfo$labID)

labID_name_mappings <- read.csv(paste0(baseDir,"/labID_name_mappings.csv"), stringsAsFactors=FALSE)
# TEMPORARY: REMOVE PROBLEMATIC LAB WITH LOW N
#labID_name_mappings <- labID_name_mappings[-c(13,13),]

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
ORIGINAL_DV1_metaVecSDExp <- vector()
ORIGINAL_DV1_metaVecSDCtrl <- vector()
ORIGINAL_DV1_metaVecNExp <- vector()
ORIGINAL_DV1_metaVecNCtrl <- vector()
ORIGINAL_DV1_metaVecR <- vector()
ORIGINAL_DV1_metaVecN <- vector()

ORIGINAL_DV1_metaVecD <- vector()
ORIGINAL_DV1_metaVecDSE <- vector()

PRIMARY_DV1_metaVecES <- vector()
PRIMARY_DV1_metaVecSE <- vector()
PRIMARY_DV1_metaVecMeanExp <- vector()
PRIMARY_DV1_metaVecSDExp <- vector()
PRIMARY_DV1_metaVecMeanCtrl <- vector()
PRIMARY_DV1_metaVecSDCtrl <- vector()
PRIMARY_DV1_metaVecNExp <- vector()
PRIMARY_DV1_metaVecNCtrl <- vector()
PRIMARY_DV1_metavecD <- vector()
PRIMARY_DV1_metavecD_SE <- vector()

PRIMARY_DV2_metaVecES <- vector()
PRIMARY_DV2_metaVecSE <- vector()
PRIMARY_DV2_metaVecMeanExp <- vector()
PRIMARY_DV2_metaVecSDExp <- vector()
PRIMARY_DV2_metaVecMeanCtrl <- vector()
PRIMARY_DV2_metaVecSDCtrl <- vector()
PRIMARY_DV2_metaVecNExp <- vector()
PRIMARY_DV2_metaVecNCtrl <- vector()

SECONDARY_DV1_metaVecES <- vector()
SECONDARY_DV1_metaVecSE <- vector()
SECONDARY_DV1_metaVecMeanExp <- vector()
SECONDARY_DV1_metaVecSDExp <- vector()
SECONDARY_DV1_metaVecMeanCtrl <- vector()
SECONDARY_DV1_metaVecSDCtrl <- vector()
SECONDARY_DV1_metaVecNExp <- vector()
SECONDARY_DV1_metaVecNCtrl <- vector()

SECONDARY_DV2_metaVecES <- vector()
SECONDARY_DV2_metaVecSE <- vector()
SECONDARY_DV2_metaVecMeanExp <- vector()
SECONDARY_DV2_metaVecSDExp <- vector()
SECONDARY_DV2_metaVecMeanCtrl <- vector()
SECONDARY_DV2_metaVecSDCtrl <- vector()
SECONDARY_DV2_metaVecNExp <- vector()
SECONDARY_DV2_metaVecNCtrl <- vector()


#setwd(getSrcDirectory(function(){})[1])
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("requirements.R")

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

raw_deathwords_Slovak <- scan("DeathWordList_Slovak.txt", what="", sep="\n")
deathwords_Slovak = c()

for (word in raw_deathwords_Slovak) {
  if (letter_search(word, "Slovak")) {
    deathwords_Slovak <- c(deathwords_Slovak, word)
  }
}


# This is The Big Loop: for each lab in our list of lab IDs, read in the
# exclusions file and main dataset, then munge and put needed results into
# vectors for use in analyses / image generation
for (lab in labIDs) {
  workingLabPathExclusions <- paste0(dataDir,"/",lab,"_coding_completed_normalized.csv")
  df_exclusions <- readInExclusionsFile(workingLabPathExclusions)
  
  workingLabPathMain <- paste0(dataDir,"/",lab,"_main.csv")
  df_main <- readInMainFile(workingLabPathMain)
  
  merged_df_exclusions <- merge(df_main, df_exclusions, by = c("id"), all=T)
  merged_df_exclusions$Exclude[is.na(merged_df_exclusions$Exclude)] <- 0
  
  df <- merged_df_exclusions[merged_df_exclusions$Exclude==0,]
  
  rm(merged_df_exclusions)
  
  # Make labID the actual lab ID (not part of raw data files as this is not always used)
  df$labID <- lab
  
  # Put N info into labInfo DF
  labInfo$N[labInfo$labID == as.factor(lab)] <- nrow(df)
  
  # Get rid of rows with critical missing data
  df <- df[(!is.na(df$Gender) & !is.na(df$Age)),]
  
  # Exclude flagged cases or those who failed exit interview
  df <- df[!is.na(df$Understand) & df$Understand==0,]
  # Exclude if participant took less than 5 minutes to complete the survey
  df <- df[!is.na(df$interviewtime) & df$interviewtime > 300,]

  # Put N info into labInfo DF (with exclusions)
  labInfo$Nused[labInfo$labID == as.factor(lab)] <- nrow(df)
  
  # Generate word counts for DV1 questions
  df$COUNT_DV1_Q1 <- mapply(is_deathword_DV1, df$WGTASK_word1_response, language=df$startlanguage, USE.NAMES=F)
  df$COUNT_DV1_Q2 <- mapply(is_deathword_DV1, df$WGTASK_word2_response, language=df$startlanguage, USE.NAMES=F)
  df$COUNT_DV1_Q3 <- mapply(is_deathword_DV1, df$WGTASK_word3_response, language=df$startlanguage, USE.NAMES=F)
  df$COUNT_DV1_Q4 <- mapply(is_deathword_DV1, df$WGTASK_word4_response, language=df$startlanguage, USE.NAMES=F)
  df$COUNT_DV1_Q5 <- mapply(is_deathword_DV1, df$WGTASK_word5_response, language=df$startlanguage, USE.NAMES=F)
  
  # Add up count variables for DV1 (Q1-Q5)
  df$COUNT_DV1 <- rowSums(df[, c(which(colnames(df)=="COUNT_DV1_Q1"):which(colnames(df)=="COUNT_DV1_Q5"))], na.rm = TRUE)
  
  # Generate word counts for DV2 questions
  df$COUNT_DV2_Q1 <- mapply(is_deathword_DV2, df$WSCTASK_S1_response, index=1, language=df$startlanguage, USE.NAMES=F)
  df$COUNT_DV2_Q2 <- mapply(is_deathword_DV2, df$WSCTASK_S5_response, index=2, language=df$startlanguage, USE.NAMES=F)
  df$COUNT_DV2_Q3 <- mapply(is_deathword_DV2, df$WSCTASK_S12_response, index=3, language=df$startlanguage, USE.NAMES=F)
  df$COUNT_DV2_Q4 <- mapply(is_deathword_DV2, df$WSCTASK_S15_response, index=4, language=df$startlanguage, USE.NAMES=F)
  df$COUNT_DV2_Q5 <- mapply(is_deathword_DV2, df$WSCTASK_S19_response, index=5, language=df$startlanguage, USE.NAMES=F)
  df$COUNT_DV2_Q6 <- mapply(is_deathword_DV2, df$WSCTASK_S22_response, index=6, language=df$startlanguage, USE.NAMES=F)
  
  # Deal with extra death-related word in Slovak-language labs
  if (df$startlanguage[1] == 'sk') {
    df$COUNT_DV2_Q7 <- mapply(is_deathword_DV2, df$WSCTASK_S18_response, index=7, language=df$startlanguage, USE.NAMES=F)
    # Add up count variables for DV2 (Q1-Q7, since Slovak had an extra word)
    df$COUNT_DV2 <- rowSums(df[, c(which(colnames(df)=="COUNT_DV2_Q1"):which(colnames(df)=="COUNT_DV2_Q7"))], na.rm = TRUE)
  }  else {
    # Add up count variables for DV2 (Q1-Q6)
    df$COUNT_DV2_Q7 <- 0
    df$COUNT_DV2 <- rowSums(df[, c(which(colnames(df)=="COUNT_DV2_Q1"):which(colnames(df)=="COUNT_DV2_Q6"))], na.rm = TRUE)
  }

  # Null values for wrong dvGroups
  # dvGroup = 1 is WGTASK (DV1), dvGroup = 2 is WSCTASK (DV2)
  df$COUNT_DV1[df$dvGroup == 2] <- NA
  df$COUNT_DV2[df$dvGroup == 1] <- NA
  
  # Put % female into LabInfo DF
  TotalWithGender <- table(df$Gender)[names(table(df$Gender))==2] + table(df$Gender)[names(table(df$Gender))==1]
  labInfo$percFemale[labInfo$labID == as.factor(lab)] <- format(round(100*(table(df$Gender)[names(table(df$Gender))==2]/TotalWithGender), digits=2))
  
  # Get mean and SD of age
  labInfo$ageMean[labInfo$labID == as.factor(lab)] <- format(mean(df$Age, na.rm=T))
  labInfo$ageSD[labInfo$labID == as.factor(lab)] <- format(sd(df$Age, na.rm=T))
  
  
  # For the following: essayGroup=1 is death cond., essayGroup=2 is control (dental pain)
  # delayGroup=1 received the delay, delayGroup=2 did not.
  # The original experiment compared the death/delay group with the other three
  # groups (death/no delay, control/no delay, death/no delay)
  
  # For the three blocks below, we create new variables that indicate groups
  # (e.g., experimental/control) as 1 or 0, leaving any other rows empty
  
  # Create group identifiers for original experiment
  df$originalExperiment <- 0 # all cases
  df$originalExperiment[df$essayGroup==1 & df$delayGroup==2] <- 1 # only death/no delay
  
  # Create group identifiers for primary analysis (death/dental pain)
  df$primaryAnalysis[df$essayGroup==2] <- 0 # dental pain
  df$primaryAnalysis[df$essayGroup==1] <- 1 # death
  
  # Create group identifiers for secondary analysis (delay/no delay)
  df$secondaryAnalysis[df$essayGroup==1 & df$delayGroup==2] <- 0 # death/no delay
  df$secondaryAnalysis[df$essayGroup==1 & df$delayGroup==1] <- 1 # death/delay
  
  df$labname <- labID_name_mappings$labname[labID_name_mappings$labID == lab]
  df$labname_short <- labID_name_mappings$labname_short[labID_name_mappings$labID == lab]
  
  # Add to merged dataframe
  mergedDF <- rbind(mergedDF, df)
  
  ## ANALYSES ##
  
  # Using the following variable naming scheme:
  # ORIGINAL_DV1 - primary analysis (exact replication of T&H)
  # PRIMARY_ - primary TM analysis
  # SECONDARY_ - additional analysis of delay effect
  
  if (lab != "METAlab") { # Skip lab without enough participants in each category
    # Calculate tests/stats for primary analysis
    ORIGINAL_DV1_m_exp <- mean(df$COUNT_DV1[df$originalExperiment==1], na.rm=T)
    ORIGINAL_DV1_sd_exp <- sd(df$COUNT_DV1[df$originalExperiment==1], na.rm=T)
    ORIGINAL_DV1_m_ctrl <- mean(df$COUNT_DV1[df$originalExperiment==0], na.rm=T)
    ORIGINAL_DV1_sd_ctrl <- sd(df$COUNT_DV1[df$originalExperiment==0], na.rm=T)
    ORIGINAL_DV1_n_exp <- length(df$COUNT_DV1[df$originalExperiment==1])
    ORIGINAL_DV1_n_ctrl <- length(df$COUNT_DV1[df$originalExperiment==0])
    ORIGINAL_DV1_r <- cor.test(df$COUNT_DV1, df$DelayTime, na.rm=T)
    
    ORIGINAL_DV1_se <- sqrt((ORIGINAL_DV1_sd_exp^2 / ORIGINAL_DV1_n_exp) + (ORIGINAL_DV1_sd_ctrl^2 / ORIGINAL_DV1_n_ctrl))
    
    ORIGINAL_DV1_metaVecES <- c(ORIGINAL_DV1_metaVecES, ORIGINAL_DV1_m_exp-ORIGINAL_DV1_m_ctrl)
    ORIGINAL_DV1_metaVecSE <- c(ORIGINAL_DV1_metaVecSE, ORIGINAL_DV1_se)
  
    # For descriptive stats tables
    ORIGINAL_DV1_descriptives$mean_exp[ORIGINAL_DV1_descriptives$labID == as.factor(lab)] <- format(ORIGINAL_DV1_m_exp)
    ORIGINAL_DV1_descriptives$sd_exp[ORIGINAL_DV1_descriptives$labID == as.factor(lab)] <- format(ORIGINAL_DV1_sd_exp)
    ORIGINAL_DV1_descriptives$mean_ctrl[ORIGINAL_DV1_descriptives$labID == as.factor(lab)] <- format(ORIGINAL_DV1_m_ctrl)
    ORIGINAL_DV1_descriptives$sd_ctrl[ORIGINAL_DV1_descriptives$labID == as.factor(lab)] <- format(ORIGINAL_DV1_sd_ctrl)
    
    ORIGINAL_DV1_metaVecR <- c(ORIGINAL_DV1_metaVecR, ORIGINAL_DV1_r$estimate)
    ORIGINAL_DV1_metaVecN <- c(ORIGINAL_DV1_metaVecN, (ORIGINAL_DV1_r$parameter + 2))
    
    ORIGINAL_DV1_metaVecMeanExp <- c(ORIGINAL_DV1_metaVecMeanExp, ORIGINAL_DV1_m_exp)
    ORIGINAL_DV1_metaVecSDExp <- c(ORIGINAL_DV1_metaVecSDExp, ORIGINAL_DV1_sd_exp)
    ORIGINAL_DV1_metaVecNExp <- c(ORIGINAL_DV1_metaVecNExp, ORIGINAL_DV1_n_exp)
    ORIGINAL_DV1_metaVecMeanCtrl <- c(ORIGINAL_DV1_metaVecMeanCtrl, ORIGINAL_DV1_m_ctrl)
    ORIGINAL_DV1_metaVecSDCtrl <- c(ORIGINAL_DV1_metaVecSDCtrl, ORIGINAL_DV1_sd_ctrl)
    ORIGINAL_DV1_metaVecNCtrl <- c(ORIGINAL_DV1_metaVecNCtrl, ORIGINAL_DV1_n_ctrl)
  }
  
  
  # Calculate tests/stats for primary analysis
  PRIMARY_DV1_m_exp <- mean(df$COUNT_DV1[df$primaryAnalysis==1], na.rm=T)
  PRIMARY_DV1_sd_exp <- sd(df$COUNT_DV1[df$primaryAnalysis==1], na.rm=T)
  PRIMARY_DV1_m_ctrl <- mean(df$COUNT_DV1[df$primaryAnalysis==0], na.rm=T)
  PRIMARY_DV1_sd_ctrl <- sd(df$COUNT_DV1[df$primaryAnalysis==0], na.rm=T)
  PRIMARY_DV1_n_exp <- length(df$COUNT_DV1[df$primaryAnalysis==1])
  PRIMARY_DV1_n_ctrl <- length(df$COUNT_DV1[df$primaryAnalysis==0])
  
  PRIMARY_DV1_se <- sqrt((PRIMARY_DV1_sd_exp^2 / PRIMARY_DV1_n_exp) + (PRIMARY_DV1_sd_ctrl^2 / PRIMARY_DV1_n_ctrl))
  
  PRIMARY_DV1_metaVecES <- c(PRIMARY_DV1_metaVecES, PRIMARY_DV1_m_exp-PRIMARY_DV1_m_ctrl)
  PRIMARY_DV1_metaVecSE <- c(PRIMARY_DV1_metaVecSE, PRIMARY_DV1_se)
  
  # For descriptive stats table
  PRIMARY_DV1_descriptives$mean_exp[PRIMARY_DV1_descriptives$labID == as.factor(lab)] <- format(PRIMARY_DV1_m_exp)
  PRIMARY_DV1_descriptives$sd_exp[PRIMARY_DV1_descriptives$labID == as.factor(lab)] <- format(PRIMARY_DV1_sd_exp)
  PRIMARY_DV1_descriptives$mean_ctrl[PRIMARY_DV1_descriptives$labID == as.factor(lab)] <- format(PRIMARY_DV1_m_ctrl)
  PRIMARY_DV1_descriptives$sd_ctrl[PRIMARY_DV1_descriptives$labID == as.factor(lab)] <- format(PRIMARY_DV1_sd_ctrl)
  
  PRIMARY_DV1_metaVecMeanExp <- c(PRIMARY_DV1_metaVecMeanExp, PRIMARY_DV1_m_exp)
  PRIMARY_DV1_metaVecSDExp <- c(PRIMARY_DV1_metaVecSDExp, PRIMARY_DV1_sd_exp)
  PRIMARY_DV1_metaVecNExp <- c(PRIMARY_DV1_metaVecNExp, PRIMARY_DV1_n_exp)
  PRIMARY_DV1_metaVecMeanCtrl <- c(PRIMARY_DV1_metaVecMeanCtrl, PRIMARY_DV1_m_ctrl)
  PRIMARY_DV1_metaVecSDCtrl <- c(PRIMARY_DV1_metaVecSDCtrl, PRIMARY_DV1_sd_ctrl)
  PRIMARY_DV1_metaVecNCtrl <- c(PRIMARY_DV1_metaVecNCtrl, PRIMARY_DV1_n_ctrl)
  
  PRIMARY_DV2_m_exp <- mean(df$COUNT_DV2[df$primaryAnalysis==1], na.rm=T)
  PRIMARY_DV2_sd_exp <- sd(df$COUNT_DV2[df$primaryAnalysis==1], na.rm=T)
  PRIMARY_DV2_m_ctrl <- mean(df$COUNT_DV2[df$primaryAnalysis==0], na.rm=T)
  PRIMARY_DV2_sd_ctrl <- sd(df$COUNT_DV2[df$primaryAnalysis==0], na.rm=T)
  PRIMARY_DV2_n_exp <- length(df$COUNT_DV2[df$primaryAnalysis==1])
  PRIMARY_DV2_n_ctrl <- length(df$COUNT_DV2[df$primaryAnalysis==0])
  PRIMARY_DV2_se <- sqrt((PRIMARY_DV2_sd_exp^2 / PRIMARY_DV2_n_exp) + (PRIMARY_DV2_sd_ctrl^2 / PRIMARY_DV2_n_ctrl))
  
  PRIMARY_DV2_metaVecES <- c(PRIMARY_DV2_metaVecES, PRIMARY_DV2_m_exp-PRIMARY_DV2_m_ctrl)
  PRIMARY_DV2_metaVecSE <- c(PRIMARY_DV2_metaVecSE, PRIMARY_DV2_se)
  
  
  # For descriptive stats table
  PRIMARY_DV2_descriptives$mean_exp[PRIMARY_DV2_descriptives$labID == as.factor(lab)] <- format(PRIMARY_DV2_m_exp)
  PRIMARY_DV2_descriptives$sd_exp[PRIMARY_DV2_descriptives$labID == as.factor(lab)] <- format(PRIMARY_DV2_sd_exp)
  PRIMARY_DV2_descriptives$mean_ctrl[PRIMARY_DV2_descriptives$labID == as.factor(lab)] <- format(PRIMARY_DV2_m_ctrl)
  PRIMARY_DV2_descriptives$sd_ctrl[PRIMARY_DV2_descriptives$labID == as.factor(lab)] <- format(PRIMARY_DV2_sd_ctrl)
  
  PRIMARY_DV2_metaVecMeanExp <- c(PRIMARY_DV2_metaVecMeanExp, PRIMARY_DV2_m_exp)
  PRIMARY_DV2_metaVecSDExp <- c(PRIMARY_DV2_metaVecSDExp, PRIMARY_DV2_sd_exp)
  PRIMARY_DV2_metaVecNExp <- c(PRIMARY_DV2_metaVecNExp, PRIMARY_DV2_n_exp)
  PRIMARY_DV2_metaVecMeanCtrl <- c(PRIMARY_DV2_metaVecMeanCtrl, PRIMARY_DV2_m_ctrl)
  PRIMARY_DV2_metaVecSDCtrl <- c(PRIMARY_DV2_metaVecSDCtrl, PRIMARY_DV2_sd_ctrl)
  PRIMARY_DV2_metaVecNCtrl <- c(PRIMARY_DV2_metaVecNCtrl, PRIMARY_DV2_n_ctrl)
  
  
  # Calculate tests/stats for secondary analysis
  # Many of these will be identical to primary analyses,
  # but calculating for the sake of clarity
  
  # essayGroup == 1: death; essayGroup == 2: dental pain
  # delayGroup == 1: delay cond; delayGroup == 2: no delay cond
  
  if (lab != "METAlab") { # Skip lab without enough participants in each category
    SECONDARY_DV1_m_exp <- mean(df$COUNT_DV1[df$secondaryAnalysis==1], na.rm=T)
    SECONDARY_DV1_sd_exp <- sd(df$COUNT_DV1[df$secondaryAnalysis==1], na.rm=T)
    SECONDARY_DV1_m_ctrl <- mean(df$COUNT_DV1[df$secondaryAnalysis==0], na.rm=T)
    SECONDARY_DV1_sd_ctrl <- sd(df$COUNT_DV1[df$secondaryAnalysis==0], na.rm=T)
    SECONDARY_DV1_n_exp <- length(df$COUNT_DV1[df$secondaryAnalysis==1])
    SECONDARY_DV1_n_ctrl <- length(df$COUNT_DV1[df$secondaryAnalysis==0])
    SECONDARY_DV1_se <- sqrt((SECONDARY_DV1_sd_exp^2 / SECONDARY_DV1_n_exp) + (SECONDARY_DV1_sd_ctrl^2 / SECONDARY_DV1_n_ctrl))
    
    SECONDARY_DV1_metaVecES <- c(SECONDARY_DV1_metaVecES, SECONDARY_DV1_m_exp-SECONDARY_DV1_m_ctrl)
    SECONDARY_DV1_metaVecSE <- c(SECONDARY_DV1_metaVecSE, SECONDARY_DV1_se)
    
    # For descriptive stats table
    SECONDARY_DV1_descriptives$mean_exp[SECONDARY_DV1_descriptives$labID == as.factor(lab)] <- format(SECONDARY_DV1_m_exp)
    SECONDARY_DV1_descriptives$sd_exp[SECONDARY_DV1_descriptives$labID == as.factor(lab)] <- format(SECONDARY_DV1_sd_exp)
    SECONDARY_DV1_descriptives$mean_ctrl[SECONDARY_DV1_descriptives$labID == as.factor(lab)] <- format(SECONDARY_DV1_m_ctrl)
    SECONDARY_DV1_descriptives$sd_ctrl[SECONDARY_DV1_descriptives$labID == as.factor(lab)] <- format(SECONDARY_DV1_sd_ctrl)
  
    SECONDARY_DV1_metaVecMeanExp <- c(SECONDARY_DV1_metaVecMeanExp, SECONDARY_DV1_m_exp)
    SECONDARY_DV1_metaVecSDExp <- c(SECONDARY_DV1_metaVecSDExp, SECONDARY_DV1_sd_exp)
    SECONDARY_DV1_metaVecNExp <- c(SECONDARY_DV1_metaVecNExp, SECONDARY_DV1_n_exp)
    SECONDARY_DV1_metaVecMeanCtrl <- c(SECONDARY_DV1_metaVecMeanCtrl, SECONDARY_DV1_m_ctrl)
    SECONDARY_DV1_metaVecSDCtrl <- c(SECONDARY_DV1_metaVecSDCtrl, SECONDARY_DV1_sd_ctrl)
    SECONDARY_DV1_metaVecNCtrl <- c(SECONDARY_DV1_metaVecNCtrl, SECONDARY_DV1_n_ctrl)
  }
  
  SECONDARY_DV2_m_exp <- mean(df$COUNT_DV2[df$secondaryAnalysis==1], na.rm=T)
  SECONDARY_DV2_sd_exp <- sd(df$COUNT_DV2[df$secondaryAnalysis==1], na.rm=T)
  SECONDARY_DV2_m_ctrl <- mean(df$COUNT_DV2[df$secondaryAnalysis==0], na.rm=T)
  SECONDARY_DV2_sd_ctrl <- sd(df$COUNT_DV2[df$secondaryAnalysis==0], na.rm=T)
  SECONDARY_DV2_n_exp <- length(df$COUNT_DV2[df$secondaryAnalysis==1])
  SECONDARY_DV2_n_ctrl <- length(df$COUNT_DV2[df$secondaryAnalysis==0])
  SECONDARY_DV2_se <- sqrt((SECONDARY_DV2_sd_exp^2 / SECONDARY_DV2_n_exp) + (SECONDARY_DV2_sd_ctrl^2 / SECONDARY_DV2_n_ctrl))
  
  SECONDARY_DV2_metaVecES <- c(SECONDARY_DV2_metaVecES, SECONDARY_DV2_m_exp-SECONDARY_DV2_m_ctrl)
  SECONDARY_DV2_metaVecSE <- c(SECONDARY_DV2_metaVecSE, SECONDARY_DV2_se)
  
  # For descriptive stats table
  SECONDARY_DV2_descriptives$mean_exp[SECONDARY_DV2_descriptives$labID == as.factor(lab)] <- format(SECONDARY_DV2_m_exp)
  SECONDARY_DV2_descriptives$sd_exp[SECONDARY_DV2_descriptives$labID == as.factor(lab)] <- format(SECONDARY_DV2_sd_exp)
  SECONDARY_DV2_descriptives$mean_ctrl[SECONDARY_DV2_descriptives$labID == as.factor(lab)] <- format(SECONDARY_DV2_m_ctrl)
  SECONDARY_DV2_descriptives$sd_ctrl[SECONDARY_DV2_descriptives$labID == as.factor(lab)] <- format(SECONDARY_DV2_sd_ctrl)
  
  SECONDARY_DV2_metaVecMeanExp <- c(SECONDARY_DV2_metaVecMeanExp, SECONDARY_DV2_m_exp)
  SECONDARY_DV2_metaVecSDExp <- c(SECONDARY_DV2_metaVecSDExp, SECONDARY_DV2_sd_exp)
  SECONDARY_DV2_metaVecNExp <- c(SECONDARY_DV2_metaVecNExp, SECONDARY_DV2_n_exp)
  SECONDARY_DV2_metaVecMeanCtrl <- c(SECONDARY_DV2_metaVecMeanCtrl, SECONDARY_DV2_m_ctrl)
  SECONDARY_DV2_metaVecSDCtrl <- c(SECONDARY_DV2_metaVecSDCtrl, SECONDARY_DV2_sd_ctrl)
  SECONDARY_DV2_metaVecNCtrl <- c(SECONDARY_DV2_metaVecNCtrl, SECONDARY_DV2_n_ctrl)
  
}


######################################
#### RUN ANALYSES & CREATE GRAPHS ####
######################################

#### REPLICATION ANALYSES ####

# Create descriptive statistics tables
write.csv(ORIGINAL_DV1_descriptives, paste0(outDir, "/ORIGINAL_WG_descriptives.csv"), row.names = F)
write.csv(PRIMARY_DV1_descriptives, paste0(outDir, "/PRIMARY_WG_descriptives.csv"), row.names = F)
write.csv(PRIMARY_DV2_descriptives, paste0(outDir, "/PRIMARY_WC_descriptives.csv"), row.names = F)
write.csv(SECONDARY_DV1_descriptives, paste0(outDir, "/SECONDARY_WG_descriptives.csv"), row.names = F)
write.csv(SECONDARY_DV2_descriptives, paste0(outDir, "/SECONDARY_WC_descriptives.csv"), row.names = F)
write.csv(labInfo, paste0(outDir, "/labDescriptives.csv"), row.names = F)

# Meta analysis (between groups)
sink(paste0(outDir, "/ma-original-WG-bg.txt"))
metaRAW_DV1 <- rma.uni(yi = ORIGINAL_DV1_metaVecES, sei = ORIGINAL_DV1_metaVecSE)
summary(metaRAW_DV1)
sink()

# Meta analysis (continuous time)
es <- escalc(measure="COR", ri=ORIGINAL_DV1_metaVecR, ni=ORIGINAL_DV1_metaVecN)
sink(paste0(outDir, "/ma-original-WG-cont.txt"))
metaR_DV1 <- rma.uni(es)
summary(metaR_DV1)
sink()


# Forest plot

# Raw ES of original study
THes <- .94-.58
# SE estimate using pooled SD from original study
THse <- sqrt(((1.21^2)+(.67^2)+(.73^2)+(.67^2))/4)/sqrt(120)

# Forest plots with help from Wagenmakers et al. (2016)

# Forest plot with word generation dv

ORIGINAL_DV1_metaVecSE[is.na(ORIGINAL_DV1_metaVecSE)] <- 0

Cairo(file=paste0(outDir, "/forest_original_WG.png"), 
      bg="white",
      type="png",
      units="in", 
      width=11, height=9, 
      #pointsize=12, 
      dpi=600)

forest(x = c(THes, ORIGINAL_DV1_metaVecES), sei = c(THse, ORIGINAL_DV1_metaVecSE), xlab="Mean difference", cex.lab=1.2,
       ilab=cbind(c(".58", format(round(ORIGINAL_DV1_metaVecMeanCtrl, digits=2))), c(".94", format(round(ORIGINAL_DV1_metaVecMeanExp, digits=2)))),
       ilab.xpos=c(grconvertX(.24, from = "ndc", "user"),
                   grconvertX(.32, from = "ndc", "user")), cex.axis=1.1, lwd=1.4,
       rows=c(length(labID_name_mappings$labname_short[-13])+7, (length(labID_name_mappings$labname_short[-13])+2):3),
       slab = c("Original Study", labID_name_mappings$labname_short[-13]),
       ylim=c(-2, length(labID_name_mappings$labname_short[-13])+11),
       xlim = c(-1.8, 1.8))

abline(h=length(labIDs[-13])+5, lwd=1.4)
text(grconvertX(.019, from = "ndc", "user"), length(labIDs[-13])+3.75, "RRR Studies", cex=1.2, pos = 4)
text(grconvertX(.053, from = "ndc", "user"), length(labIDs[-13])+10, "Study", cex=1.2)
text(grconvertX(.24, from = "ndc", "user"), length(labIDs[-13])+10, "Delay", cex=1.2)
text(grconvertX(.32, from = "ndc", "user"), length(labIDs[-13])+10, "Other", cex=1.2)
text(grconvertX(.875, from = "ndc", "user"), length(labIDs[-13])+10, paste0("Mean difference", " [95% CI]"), cex=1.2)

abline(h=1, lwd=1.4)
addpoly(metaRAW_DV1, atransf=FALSE, row=-1, cex=1.3, mlab="Meta-Analytic Effect Size:")

dev.off()



# Line graphs of each lab's findings

# Alphabetize by lab ID

mergedDF <- mergedDF[order(mergedDF$labID),]

all_linear <- ggplot(mergedDF, aes(x = DelayTime, y = COUNT_DV1, group = labID)) +
  labs(x = "Delay Time", y = "Word Count") +
  geom_smooth(method = "lm", se = T, color = "darkgrey") +
  geom_point(alpha = 0.3, size = 0) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  facet_wrap(~labname_short, scales = "free")

ggsave(paste0(outDir, "/ORIGINAL_WG_line-graphs.png"), dpi=600)

all_linear <- ggplot(mergedDF, aes(x = DelayTime, y = COUNT_DV2, group = labID)) +
  labs(x = "Delay Time", y = "Word Count") +
  geom_smooth(method = "lm", se = T, color = "darkgrey") +
  geom_point(alpha = 0.3, size = 0) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  facet_wrap(~labname_short, scales = "free")

ggsave(paste0(outDir, "/ORIGINAL_WC_line-graphs.png"), dpi=600)

all_loess <- ggplot(mergedDF, aes(x = DelayTime, y = COUNT_DV1, group = labID)) +
  labs(x = "Delay Time", y = "Word Count") +
  geom_smooth(method = "loess", se = T, color = "darkgrey") +
  geom_point(alpha = 0.3, size = 0) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  facet_wrap(~labname_short, scales = "free")

ggsave(paste0(outDir, "/ORIGINAL_WG_loess-graphs.png"), dpi=600)

all_loess <- ggplot(mergedDF, aes(x = DelayTime, y = COUNT_DV2, group = labID)) +
  labs(x = "Delay Time", y = "Word Count") +
  geom_smooth(method = "loess", se = T, color = "darkgrey") +
  geom_point(alpha = 0.3, size = 0) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  facet_wrap(~labname_short, scales = "free")

ggsave(paste0(outDir, "/ORIGINAL_WC_loess-graphs.png"), dpi=600)


#### PRIMARY ANALYSES ####

# Meta analysis (word generation)
sink(paste0(outDir, "/ma-primary-WG.txt"))
primary_DV1_meta <- rma.uni(yi = PRIMARY_DV1_metaVecES, sei = PRIMARY_DV1_metaVecSE)
summary(primary_DV1_meta)
sink()

# Meta analysis (word creation)
sink(paste0(outDir, "/ma-primary-WC.txt"))
primary_DV2_meta <- rma.uni(yi = PRIMARY_DV2_metaVecES, sei = PRIMARY_DV2_metaVecSE)
summary(primary_DV2_meta)
sink()


# Word generation dv

PRIMARY_DV1_metaVecSE[is.na(PRIMARY_DV1_metaVecSE)] <- 0

Cairo(file=paste0(outDir, "/forest_primary_WG.png"), 
      bg="white",
      type="png",
      units="in", 
      width=11, height=9, 
      #pointsize=12, 
      dpi=600)

forest(x = PRIMARY_DV1_metaVecES, sei = PRIMARY_DV1_metaVecSE, xlab="Mean difference", cex.lab=1.4,
       ilab=cbind(format(round(PRIMARY_DV1_metaVecMeanCtrl, digits=2)), format(round(PRIMARY_DV1_metaVecMeanExp, digits=2))),
       ilab.xpos=c(grconvertX(.22, from = "ndc", "user"),
                   grconvertX(.30, from = "ndc", "user")),
       cex.axis=1.1, lwd=1.4,
       ylim=c(-2, length(labID_name_mappings$labname_short)+3),
       xlim=c(-1.1, 1),
       slab = labID_name_mappings$labname_short)

text(grconvertX(.053, from = "ndc", "user"), length(labIDs)+2, "Study", cex=1.2)
text(grconvertX(.22, from = "ndc", "user"), length(labIDs)+2, "Pain", cex=1.2)
text(grconvertX(.30, from = "ndc", "user"), length(labIDs)+2, "Death", cex=1.2)
text(grconvertX(.87, from = "ndc", "user"), length(labIDs)+2, paste0("Mean difference", " [95% CI]"), cex=1.2)

abline(h=0, lwd=1.4)
addpoly(primary_DV1_meta, atransf=FALSE, row=-1, cex=1.3, mlab="Meta-Analytic Effect Size:")

dev.off()

# Word completion dv

Cairo(file=paste0(outDir, "/forest_primary_WC.png"), 
      bg="white",
      type="png",
      units="in", 
      width=11, height=9, 
      #pointsize=12, 
      dpi=600)

forest(x = PRIMARY_DV2_metaVecES, sei = PRIMARY_DV2_metaVecSE, xlab="Mean difference", cex.lab=1.4,
       ilab=cbind(format(round(PRIMARY_DV2_metaVecMeanCtrl, digits=2)), format(round(PRIMARY_DV2_metaVecMeanExp, digits=2))),
       ilab.xpos=c(grconvertX(.22, from = "ndc", "user"),
                   grconvertX(.3, from = "ndc", "user")), cex.axis=1.1, lwd=1.4,
       ylim=c(-2, length(labID_name_mappings$labname_short)+3),
       xlim=c(-4.4, 4.9),
       slab = labID_name_mappings$labname_short)

text(grconvertX(.053, from = "ndc", "user"), length(labIDs)+2, "Study", cex=1.2)
text(grconvertX(.22, from = "ndc", "user"), length(labIDs)+2, "Pain", cex=1.2)
text(grconvertX(.3, from = "ndc", "user"), length(labIDs)+2, "Death", cex=1.2)
text(grconvertX(.87, from = "ndc", "user"), length(labIDs)+2, paste0("Mean difference", " [95% CI]"), cex=1.2)

abline(h=0, lwd=1.4)
addpoly(primary_DV2_meta, atransf=FALSE, row=-1, cex=1.3, mlab="Meta-Analytic Effect Size:")

dev.off()


#### SECONDARY ANALYSES ####

# Meta analysis, word generation
sink(paste0(outDir, "/ma-secondary-WG.txt"))
secondary_DV1_meta <- rma.uni(yi = SECONDARY_DV1_metaVecES, sei = SECONDARY_DV1_metaVecSE)
summary(secondary_DV1_meta)
sink()

# Meta analysis, word creation
sink(paste0(outDir, "/ma-secondary-WC.txt"))
secondary_DV2_meta <- rma.uni(yi = SECONDARY_DV2_metaVecES, sei = SECONDARY_DV2_metaVecSE)
summary(secondary_DV2_meta)
sink()


# Word generation dv

SECONDARY_DV1_metaVecSE[is.na(SECONDARY_DV1_metaVecSE)] <- 0

Cairo(file=paste0(outDir, "/forest_secondary_WG.png"), 
      bg="white",
      type="png",
      units="in", 
      width=11, height=9, 
      #pointsize=12, 
      dpi=600)

forest(x = SECONDARY_DV1_metaVecES, sei = SECONDARY_DV1_metaVecSE, xlab="Mean difference", cex.lab=1.4,
       ilab=cbind(format(round(SECONDARY_DV1_metaVecMeanCtrl, digits=2)), format(round(SECONDARY_DV1_metaVecMeanExp, digits=2))),
       ilab.xpos=c(grconvertX(.22, from = "ndc", "user"),
                   grconvertX(.3, from = "ndc", "user")),
       cex.axis=1.1,
       lwd=1.4,
       ylim=c(-2, length(labID_name_mappings$labname_short[-13])+3),
       xlim=c(-2.3, 1.3),
       slab = labID_name_mappings$labname_short[-13])

text(grconvertX(.053, from = "ndc", "user"), length(labIDs[-13])+2, "Study", cex=1.2)
text(grconvertX(.22, from = "ndc", "user"), length(labIDs[-13])+2, "No Delay", cex=1.2)
text(grconvertX(.3, from = "ndc", "user"), length(labIDs[-13])+2, "Delay", cex=1.2)
text(grconvertX(.87, from = "ndc", "user"), length(labIDs[-13])+2, paste0("Mean difference", " [95% CI]"), cex=1.2)

abline(h=0, lwd=1.4)
addpoly(secondary_DV1_meta, atransf=FALSE, row=-1, cex=1.3, mlab="Meta-Analytic Effect Size:")

dev.off()

# Word completion dv

Cairo(file=paste0(outDir, "/forest_secondary_WC.png"), 
      bg="white",
      type="png",
      units="in", 
      width=11, height=9, 
      #pointsize=12, 
      dpi=600)

forest(x = SECONDARY_DV2_metaVecES, sei = SECONDARY_DV2_metaVecSE, xlab="Mean difference", cex.lab=1.4,
       ilab=cbind(format(round(SECONDARY_DV2_metaVecMeanCtrl, digits=2)), format(round(SECONDARY_DV2_metaVecMeanExp, digits=2))),
       ilab.xpos=c(grconvertX(.22, from = "ndc", "user"),
                   grconvertX(.3, from = "ndc", "user")), cex.axis=1.1, lwd=1.4,
       ylim=c(-2, length(labID_name_mappings$labname_short)+3),
       xlim=c(-9.4, 5.4),
       slab = labID_name_mappings$labname_short)

text(grconvertX(.053, from = "ndc", "user"), length(labIDs)+2, "Study", cex=1.2)
text(grconvertX(.22, from = "ndc", "user"), length(labIDs)+2, "No Delay", cex=1.2)
text(grconvertX(.3, from = "ndc", "user"), length(labIDs)+2, "Delay", cex=1.2)
text(grconvertX(.87, from = "ndc", "user"), length(labIDs)+2, paste0("Mean difference", " [95% CI]"), cex=1.2)

abline(h=0, lwd=1.4)
addpoly(secondary_DV2_meta, atransf=FALSE, row=-1, cex=1.3, mlab="Meta-Analytic Effect Size:")

dev.off()

