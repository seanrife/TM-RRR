# Analysis for RRR of Trafimow and Hughes (2012), Study 3
# Sean C. Rife / @seanrife / seanrife.com / srife1@murraystate.edu

# Clean house
rm(list = ls())

# Set base directory
# Looks here for main datasets, ratings & exclusions
baseDir <- "C:\\Users\\srife1\\Dropbox\\Research\\TM RRR\\data"

# Lab names; used for reading in data
labNames <- c("Lab1", "Lab2", "Lab3", "Lab4", "Lab5", "Lab6", "Lab7", "Lab8",
              "Lab9", "Lab10", "Lab11", "Lab12", "Lab13", "Lab14", "Lab15")

# Load required libraries
if(!require(metafor)){install.packages('metafor')}
library(metafor)
if(!require(tools)){install.packages('tools')}
library(tools)
if(!require(yarrr)){install.packages('yarrr')}
library(yarrr)

# Create dataframe for summary data
outFrame <- data.frame()
colnames(outFrame) <- c("labID", "avgAge", "nFemale", "MSn_delay", "DPn_delay",
                       "MSn_nodelay", "DPn_nodelay", "MSmean_delay", "DPmean_delay",
                       "MSmean_nodelay", "DPmean_nodelay", "MSsd_delay",
                       "DPsd_delay", "MSsd_nodelay", "DPsd_nodelay", "delayTime")

# Function to read in and clean datafiles from each lab
readInFile <- function(filename) {
  df <- read.csv(filename, header = T, stringsAsFactors = F, check.names=F)
  names(df) <- gsub(x = names(df),
                    pattern = "\\]",
                    replacement = "")
  names(df) <- gsub(x = names(df),
                    pattern = "\\[",
                    replacement = "_")
  names(df) <- gsub(x = names(df),
                    pattern = "\\_SQ001",
                    replacement = "")
  df <- df[, -c(which(colnames(df)=="DEBRIEFING"):which(colnames(df)=="WTDP2Time"))]
  df <- df[, -c(which(colnames(df)=="ARTICLETime"):which(colnames(df)=="DEBRIEFINGTime"))]
  colnames(df)[ncol(df)] <- "DelayTime"
  return(df)
}


for (lab in labNames) {

  workingLabPathMain <- paste0(baseDir,"\\",lab,"_main.csv")
  workingLabPathRating <- paste0(baseDir,"\\",lab,"_rating.csv")
  workingLabPathExclude <- paste0(baseDir,"\\",lab,"_exclude.csv")
  df_main <- readInFile(workingLabPathMain)
  df_rating <- read.csv(workingLabPathRating)
  df_exclude <- read.csv(workingLabPathExclude)
  df <- merge(df_main,df_rating,by=c("id","labID"))
  df <- merge(df,df_exclude,by=c("id","labID"))
  
  # Exclude flagged cases or those who failed exit interview
  df <- df[df$FLAG==0,]
  df <- df[df$Understand==1,]
  df <- df[df$Familiar==0,]
  
  assign(lab,df)
}

rm(df_main)
rm(df_rating)
rm(df_exclude)

