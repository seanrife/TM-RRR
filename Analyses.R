# Analysis for RRR of Trafimow and Hughes (2012), Study 3
# Sean C. Rife / @seanrife / seanrife.com / srife1@murraystate.edu

# Clean house
rm(list = ls())

# Set base directory
# Looks here for main datasets, ratings & exclusions
baseDir <- "D:\\Dropbox\\Research\\TM RRR\\data"

# Lab names; used for reading in data
labNames <- c("Lab1", "Lab2", "Lab3", "Lab4", "Lab5", "Lab6", "Lab7", "Lab8",
              "Lab9", "Lab10", "Lab11", "Lab12", "Lab13", "Lab14", "Lab15")

# Load required libraries
if(!require(metafor)){install.packages('metafor')}
library(metafor)
if(!require(yarrr)){install.packages('yarrr')}
library(yarrr)
if(!require(effsize)){install.packages('effsize')}
library(effsize)
if(!require(plotrix)){install.packages('plotrix')}
library(plotrix)
if(!require(apaTables)){install.packages('apaTables')}
library(apaTables)

mergedDF <- data.frame()

# Create dataframe for summary data
outFrame <- data.frame()
#colnames(outFrame) <- c("labID", "avgAge", "nFemale", "MSn_delay", "DPn_delay",
#                       "MSn_nodelay", "DPn_nodelay", "MSmean_delay", "DPmean_delay",
#                       "MSmean_nodelay", "DPmean_nodelay", "MSsd_delay",
#                       "DPsd_delay", "MSsd_nodelay", "DPsd_nodelay", "delayTime")

metaVecES <- vector()
metaVecSE <- vector()
metaVecMeanExp <- vector()
metaVecMeanCtrl <- vector()

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
  
  df$originalExperiment <- 0
  df$originalExperiment[df$essayGroup==1 & df$delayGroup==1] <- 1
  
  # Calculate tests for primary analysis
  t <- t.test(df$COUNT ~ df$originalExperiment)
  m_exp <- mean(df$COUNT[df$originalExperiment==1])
  sd_exp <- sd(df$COUNT[df$originalExperiment==1])
  m_ctrl <- mean(df$COUNT[df$originalExperiment==0])
  sd_ctrl <- sd(df$COUNT[df$originalExperiment==0])
  n_exp <- length(df$COUNT[df$originalExperiment==1])
  n_ctrl <- length(df$COUNT[df$originalExperiment==0])
  r <- cor.test(df$COUNT,df$DelayTime)
  d <- cohen.d(df$COUNT, as.factor(df$originalExperiment))
  se <- std.error(df$COUNT)

  metaVecES <- c(metaVecES, (m_exp-m_ctrl))
  metaVecSE <- c(metaVecSE, se)
  
  metaVecMeanExp <- c(metaVecMeanExp, m_exp)
  metaVecMeanCtrl <- c(metaVecMeanCtrl, m_ctrl)
  
  mergedDF <- rbind(mergedDF, df)
  
  assign(lab,df)
  
}

rm(df_main)
rm(df_rating)
rm(df_exclude)

meta <- rma.uni(yi = metaVecES, sei = metaVecSE)



#### FOREST PLOT ####

forest(meta, xlab="Condition", ilab=cbind(c("5.14", metaVecMeanExp),
                                          c("4.32", metaVecMeanCtrl),
                                          ilab.xpos=c(grconvertX(.18, from = "ndc", "user")),
                                          grconvertX(.28, from = "ndc", "user")))
abline(h=length(labNames)+2, lwd=1.4)
text(grconvertX(.019, from = "ndc", "user"), length(labNames)+3.75, "RRR Studies", pos = 4)
text(grconvertX(.053, from = "ndc", "user"), length(labNames)+10, "Study")
text(grconvertX(.18, from = "ndc", "user"), length(labNames)+10, "Death")
text(grconvertX(.28, from = "ndc", "user"), length(labNames)+10, "Dental Pain")
text(grconvertX(.9, from = "ndc", "user"), length(labNames)+10, paste0("Condition", " [95% CI]"))
addpoly(meta, atransf=FALSE, row=-1, mlab="Meta-Analytic Effect Size:")


#### RUN BETWEEN-LABS ANOVA ####
betweenLabsAOV <- aov(COUNT~labID, data=mergedDF)
apa.1way.table(labID, COUNT, mergedDF,"MSD_tab.doc")
apa.aov.table(betweenLabsAOV,"ANOVA_tab.doc")


#### PIRATE PLOT OF MAIN EFFECT ####

mergedDF$originalExperimentCHR <- "Dental Pain"
mergedDF$originalExperimentCHR[mergedDF$originalExperiment==1] <- "Death"
pirateplot(formula = COUNT ~ as.factor(originalExperimentCHR),
           data = mergedDF,
           xlab = "Condition",
           ylab = "Number of death-related words",
           main = "Replication of T&H (2012)")
