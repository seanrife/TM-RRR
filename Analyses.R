# Analysis for RRR of Trafimow and Hughes (2012), Study 3
# Sean C. Rife / @seanrife / seanrife.com / srife1@murraystate.edu

# Clean house
rm(list = ls())


#### USER-DEFINED VARIABLES ####

# Set base directory
# Uses this to look for main datasets, ratings & exclusions
baseDir <- "D:\\Dropbox\\Research\\TM RRR" # DESKTOP
#baseDir <- "C:\\Users\\srife1\\Dropbox\\Research\\TM RRR" # LAPTOP

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

mergedDF <- data.frame()

# Read in lab info
labInfo <- read.csv(paste0(dataDir,"\\LabInfo.csv"), stringsAsFactors=FALSE)

# Create dataframes for descriptive tables
PRIMARY_descriptives <- labInfo
SECONDARY_descriptives <- labInfo

# Lab names; used for reading in data and labeling
labNames <- as.vector(labInfo$labID)

# Create containers for output data
PRIMARY_metaVecES <- vector()
PRIMARY_metaVecSE <- vector()
PRIMARY_metaVecMeanExp <- vector()
PRIMARY_metaVecMeanCtrl <- vector()
PRIMARY_metaVecR <- vector()
PRIMARY_metaVecN <- vector()
SECONDARY_metaVecES <- vector()
SECONDARY_metaVecSE <- vector()
SECONDARY_metaVecMeanExp <- vector()
SECONDARY_metaVecMeanCtrl <- vector()



# Function to read in and clean datafiles from each lab
readInFile <- function(filename) {
  df <- read.csv(filename, header = T, stringsAsFactors = F, check.names=F)
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
  # Delete variables we don't need
  df <- df[, -c(which(colnames(df)=="DEBRIEFING"):which(colnames(df)=="WTDP2Time"))]
  df <- df[, -c(which(colnames(df)=="ARTICLETime"):which(colnames(df)=="DEBRIEFINGTime"))]
  # Change the only time variable we care about to something meaningful
  colnames(df)[ncol(df)] <- "DelayTime"
  return(df)
}


for (lab in labNames) {

  workingLabPathMain <- paste0(dataDir,"\\",lab,"_main.csv")
  workingLabPathRating <- paste0(dataDir,"\\",lab,"_rating.csv")
  workingLabPathExclude <- paste0(dataDir,"\\",lab,"_exclude.csv")
  df_main <- readInFile(workingLabPathMain)
  df_rating <- read.csv(workingLabPathRating)
  df_exclude <- read.csv(workingLabPathExclude)
  df <- merge(df_main,df_rating,by=c("id","labID"))
  df <- merge(df,df_exclude,by=c("id","labID"))
  
  
  # Put N info into labInfo DF
  labInfo$N[labInfo$labID == as.factor(lab)] <- nrow(df)
  
  # Exclude flagged cases or those who failed exit interview
  df <- df[df$FLAG==0,]
  df <- df[df$Understand==1,]
  df <- df[df$Familiar==0,]
  ## ADD REMOVAL BASED ON TIME HERE ##
  # Exclude if participant took less than 3 minutes to complete the survey
  
  # Put N info into labInfo DF (without exclusions)
  labInfo$Nused[labInfo$labID == as.factor(lab)] <- nrow(df)
  
  
  # Create group identifiers for original experiment
  df$originalExperiment <- 0
  df$originalExperiment[df$essayGroup==1 & df$delayGroup==1] <- 1

  # Create group identifiers for secondary analysis
  df$secondaryAnalysis <- 0
  df$secondaryAnalysis[df$essayGroup==1] <- 1
  
  
  # Add to merged dataframe
  mergedDF <- rbind(mergedDF, df)
  
  ## ANALYSES ##
  
  # Using the following variable naming scheme:
    # PRIMARY_ - primary analysis (exact replication of T&H)
    # SECONDARY_ - additional analysis not related directly to T&H

  # Calculate tests/stats for primary analysis
  PRIMARY_m_exp <- mean(df$COUNT[df$originalExperiment==1])
  PRIMARY_sd_exp <- sd(df$COUNT[df$originalExperiment==1])
  PRIMARY_m_ctrl <- mean(df$COUNT[df$originalExperiment==0])
  PRIMARY_sd_ctrl <- sd(df$COUNT[df$originalExperiment==0])
  PRIMARY_r <- cor.test(df$COUNT, df$DelayTime)
  PRIMARY_d <- cohen.d(df$COUNT, as.factor(df$originalExperiment))$estimate
  PRIMARY_se <- std.error(df$COUNT)
  
  
  # For descriptive stats table
  PRIMARY_descriptives$mean_exp[PRIMARY_descriptives$labID == as.factor(lab)] <- format(PRIMARY_m_exp, digits=3)
  PRIMARY_descriptives$sd_exp[PRIMARY_descriptives$labID == as.factor(lab)] <- format(PRIMARY_sd_exp, digits=3)
  PRIMARY_descriptives$mean_ctrl[PRIMARY_descriptives$labID == as.factor(lab)] <- format(PRIMARY_m_ctrl, digits=3)
  PRIMARY_descriptives$sd_ctrl[PRIMARY_descriptives$labID == as.factor(lab)] <- format(PRIMARY_sd_ctrl, digits=3)

  PRIMARY_metaVecR <- c(PRIMARY_metaVecR, PRIMARY_r$estimate)
  PRIMARY_metaVecN <- c(PRIMARY_metaVecN, (PRIMARY_r$parameter + 2))
  
  PRIMARY_metaVecES <- c(PRIMARY_metaVecES, PRIMARY_d)
  PRIMARY_metaVecSE <- c(PRIMARY_metaVecSE, PRIMARY_se)
  
  PRIMARY_metaVecMeanExp <- c(PRIMARY_metaVecMeanExp, PRIMARY_m_exp)
  PRIMARY_metaVecMeanCtrl <- c(PRIMARY_metaVecMeanCtrl, PRIMARY_m_ctrl)
  


  
  
  # Calculate tests/stats for secondary analysis
  # Many of these will be identical to primary analyses,
  # but calculating for the sake of clarity
  SECONDARY_m_exp <- mean(df$COUNT[df$secondaryAnalysis==1])
  SECONDARY_sd_exp <- sd(df$COUNT[df$secondaryAnalysis==1])
  SECONDARY_m_ctrl <- mean(df$COUNT[df$secondaryAnalysis==0])
  SECONDARY_sd_ctrl <- sd(df$COUNT[df$secondaryAnalysis==0])
  SECONDARY_n_exp <- length(df$COUNT[df$secondaryAnalysis==1])
  SECONDARY_n_ctrl <- length(df$COUNT[df$secondaryAnalysis==0])
  SECONDARY_d <- cohen.d(df$COUNT, as.factor(df$secondaryAnalysis))$estimate
  SECONDARY_d_CI_UPPER <- cohen.d(df$COUNT, as.factor(df$secondaryAnalysis))$conf.int[2]
  SECONDARY_d_CI_LOWER <- cohen.d(df$COUNT, as.factor(df$secondaryAnalysis))$conf.int[1]
  SECONDARY_se <- std.error(df$COUNT)
  
  
  # For descriptive stats table
  SECONDARY_descriptives$mean_exp[SECONDARY_descriptives$labID == as.factor(lab)] <- format(SECONDARY_m_exp, digits=3)
  SECONDARY_descriptives$sd_exp[SECONDARY_descriptives$labID == as.factor(lab)] <- format(SECONDARY_sd_exp, digits=3)
  SECONDARY_descriptives$mean_ctrl[SECONDARY_descriptives$labID == as.factor(lab)] <- format(SECONDARY_m_ctrl, digits=3)
  SECONDARY_descriptives$sd_ctrl[SECONDARY_descriptives$labID == as.factor(lab)] <- format(SECONDARY_sd_ctrl, digits=3)

  SECONDARY_metaVecES <- c(SECONDARY_metaVecES, (SECONDARY_d))
  SECONDARY_metaVecSE <- c(SECONDARY_metaVecSE, SECONDARY_se)
  
  SECONDARY_metaVecMeanExp <- c(SECONDARY_metaVecMeanExp, SECONDARY_m_exp)
  SECONDARY_metaVecMeanCtrl <- c(SECONDARY_metaVecMeanCtrl, SECONDARY_m_ctrl)
  
}

# Clean things up by deleting leftover dataframes
rm(df_main)
rm(df_rating)
rm(df_exclude)


######################################
#### RUN ANALYSES & CREATE GRAPHS ####
######################################

#### PRIMARY ANALYSES ####

# Create descriptive statistics tables
write.csv(PRIMARY_descriptives, paste0(outDir, "\\PRIMARY_descriptives.csv"), row.names = F)
write.csv(SECONDARY_descriptives, paste0(outDir, "\\SECONDARY_descriptives.csv"), row.names = F)


# Meta analysis
sink(paste0(outDir, "\\ma-primary-bg.txt"))
meta <- rma.uni(yi = PRIMARY_metaVecES, sei = PRIMARY_metaVecSE)
summary(meta)
sink()


# Meta analysis
es <- escalc(measure="COR", ri=PRIMARY_metaVecR, ni=PRIMARY_metaVecN)
sink(paste0(outDir, "\\ma-primary-cont.txt"))
meta <- rma.uni(es)
summary(meta)
sink()



# Forest plot

# Raw ES of original study
THes <- .94-.58
# SE estimate using pooled SD from original study
THse <- sqrt(((1.21^2)+(.67^2)+(.73^2)+(.67^2))/4)/sqrt(120)

# Forest plot code snipped from Wagenmakers et al. (2016)

Cairo(file=paste0(outDir, "\\forest_main.png"), 
      bg="white",
      type="png",
      units="in", 
      width=11, height=7, 
      #pointsize=12, 
      dpi=600)


forest(x = c(THes, PRIMARY_metaVecES), sei = c(THse, PRIMARY_metaVecSE), xlab="Mean difference", cex.lab=1.4,
       ilab=cbind(c("5.14", format(PRIMARY_metaVecMeanExp, digits=3)), c("4.32", format(PRIMARY_metaVecMeanCtrl, digits=3))),
       ilab.xpos=c(grconvertX(.18, from = "ndc", "user"),
                   grconvertX(.28, from = "ndc", "user")), cex.axis=1.1, lwd=1.4,
       rows=c(length(labNames)+7, (length(labNames)+2):3), ylim=c(-2, length(labNames)+11),
       slab = c("Original Study", paste0("Study ", seq_len(length(labNames)))))

abline(h=length(labNames)+5, lwd=1.4)
text(grconvertX(.019, from = "ndc", "user"), length(labNames)+3.75, "RRR Studies", cex=1.2, pos = 4)
text(grconvertX(.053, from = "ndc", "user"), length(labNames)+10, "Study", cex=1.2)
text(grconvertX(.18, from = "ndc", "user"), length(labNames)+10, "Delay", cex=1.2)
text(grconvertX(.28, from = "ndc", "user"), length(labNames)+10, "Other Cond.", cex=1.2)
text(grconvertX(.875, from = "ndc", "user"), length(labNames)+10, paste0("Mean difference", " [95% CI]"), cex=1.2)

abline(h=1, lwd=1.4)
addpoly(meta, atransf=FALSE, row=-1, cex=1.3, mlab="Meta-Analytic Effect Size:")

dev.off()


# Between-labs ANOVA
sink(paste0(outDir, "\\ANOVA.txt"))
betweenLabsAOV <- aov(COUNT~labID, data=mergedDF)
betweenLabsAOV
summary(betweenLabsAOV)
sink()


# Line graphs of each lab's findings

all_linear <- ggplot(mergedDF, aes(x = DelayTime, y = COUNT, group = labID)) +
  labs(x = "Delay Time", y = "Word Count") +
  geom_smooth(method = "loess", se = T, color = "darkgrey") +
  geom_point(alpha = 0.3, size = 0) +
  facet_wrap(~labID, scales = "free_x")

ggsave(paste0(outDir, "\\PRIMARY_line-graphs.png"))


#### SECONDARY ANALYSES ####

# Meta analysis
sink(paste0(outDir, "\\ma-secondary.txt"))
meta <- rma.uni(yi = SECONDARY_metaVecES, sei = SECONDARY_metaVecSE)
summary(meta)
sink()



# Forest plot code snipped from Wagenmakers et al. (2016)

Cairo(file=paste0(outDir, "\\forest_secondary.png"), 
      bg="white",
      type="png",
      units="in", 
      width=11, height=7, 
      #pointsize=12, 
      dpi=600)


forest(x = SECONDARY_metaVecES, sei = SECONDARY_metaVecSE, xlab="Mean difference", cex.lab=1.4,
       ilab=cbind(format(SECONDARY_metaVecMeanExp, digits=3), format(SECONDARY_metaVecMeanCtrl, digits=3)),
       ilab.xpos=c(grconvertX(.18, from = "ndc", "user"),
                   grconvertX(.28, from = "ndc", "user")), cex.axis=1.1, lwd=1.4,
       ylim=c(-2, length(labNames)+3),
       slab = paste0("Study ", seq_len(length(labNames))))

text(grconvertX(.053, from = "ndc", "user"), length(labNames)+2, "Study", cex=1.2)
text(grconvertX(.18, from = "ndc", "user"), length(labNames)+2, "Death", cex=1.2)
text(grconvertX(.28, from = "ndc", "user"), length(labNames)+2, "Dental Pain", cex=1.2)
text(grconvertX(.875, from = "ndc", "user"), length(labNames)+2, paste0("Mean difference", " [95% CI]"), cex=1.2)

abline(h=0, lwd=1.4)
addpoly(meta, atransf=FALSE, row=-1, cex=1.3, mlab="Meta-Analytic Effect Size:")

dev.off()