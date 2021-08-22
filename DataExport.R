# Data export for RRR of Trafimow and Hughes (2012), Study 3
# Sean C. Rife / @seanrife / seanrife.com / srife1@murraystate.edu

# Clean house
rm(list = ls())


##### USER DEFINED #####
baseDir <- "H:\\Dropbox\\Research\\TM RRR" # DESKTOP
#baseDir <- "C:\\Users\\srife1\\Dropbox\\Research\\TM RRR" # LAPTOP

dataDir <- paste0(baseDir, "\\data")


# Function to read in and clean datafiles from each lab
readInFile <- function(filename) {
  varNamesToRetain <- c("id", "Purpose")
  df <- read.csv(filename, header = T, stringsAsFactors = F, check.names=F)
  colnames(df)[1] <- "id"
  df <- df[varNamesToRetain]
  return(df)
}


labInfo <- read.csv(paste0(dataDir,"\\LabInfo.csv"), stringsAsFactors=FALSE)
labNames <- as.vector(labInfo$labID)

for (labName in labNames) {
  labData <- readInFile(paste0(dataDir, '\\', labName, '_main.csv'))
  labData <- labData[!apply(labData, 1, function(x) any(x=="")),]
  labData[,"Decision"] <- ""
  write.csv(labData, paste0(dataDir, '\\', labName, '_coding.csv'), row.names = F)
}


