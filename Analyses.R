# Analysis for RRR of Trafimow and Hughes (2012), Study 3
# Sean C. Rife / @seanrife / seanrife.com / srife1@murraystate.edu

# Clean house
rm(list = ls())

# Set base directory
# Looks here for main datasets, and ratings & exclusions in
# two subdirs: "/ratings" and "/exclusions"
baseDir <- "C:\\Users\\srife1\\Dropbox\\Research\\TM RRR\\data"

# Load required libraries
if(!require(metafor)){install.packages('metafor')}
library(metafor)
if(!require(tools)){install.packages('tools')}
library(tools)


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

files <- list.files(path=baseDir, pattern="*.csv", full.names=T, recursive=F)

for (f in files) {
  df <- readInFile(f)
  assign(file_path_sans_ext(basename(f)),df)
}
