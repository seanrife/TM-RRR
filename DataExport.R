# Data export for RRR of Trafimow and Hughes (2012), Study 3
# Sean C. Rife / @seanrife / seanrife.com / srife1@murraystate.edu

# Clean house
rm(list = ls())


##### USER DEFINED #####
dataDir <- ""


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
  df <- df[, c(which(colnames(df)=="id"), which(colnames(df)=="labID"), which(colnames(df)=="WGTASK_word1"):which(colnames(df)=="WGTASK_word5"))]
  return(df)
}

files <- list.files(path=dataDir, pattern="*.csv", full.names=T, recursive=F)

lapply(files, function(x) {
  df <- readInFile(x)
  write.table(df, paste0(dataDir,"/out.csv"), sep="\t", quote=F, row.names=F, col.names=T, append=T)
})

