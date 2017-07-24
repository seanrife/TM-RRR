# Analysis for RRR of Trafimow and Hughes (2012), Study 3
# Sean C. Rife / @seanrife / seanrife.com / srife1@murraystate.edu

# Clean house
rm(list = ls())

# Set base directory
# Looks here for main datasets, ratings & exclusions
#baseDir <- "D:\\Dropbox\\Research\\TM RRR\\data" # DESKTOP
baseDir <- "C:\\Users\\srife1\\Dropbox\\Research\\TM RRR\\data" # LAPTOP

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
dfList <- list()

labInfo <- read.csv(paste0(baseDir,"\\LabInfo.csv"), stringsAsFactors=FALSE)

# Lab names; used for reading in data and labeling
labNames <- as.vector(labInfo$labID)

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
  
  
  # Put N info into labInfo DF
  labInfo$N[labInfo$labID == as.factor(lab)] <- nrow(df)
  
  # Exclude flagged cases or those who failed exit interview
  df <- df[df$FLAG==0,]
  df <- df[df$Understand==1,]
  df <- df[df$Familiar==0,]
  
  # Put N info into labInfo DF (without exclusions)
  labInfo$Nused[labInfo$labID == as.factor(lab)] <- nrow(df)
  
  df$originalExperiment <- 0
  df$originalExperiment[df$essayGroup==1 & df$delayGroup==1] <- 1

    
  ## ANALYSES ##
  
  # Using the following variable naming scheme:
    # PRIMARY_ - primary analysis (exact replication of T&H)
    # LR_ - linear regression of word count on delay
    # DTA_ - death-thought accessibility test
  
  # Calculate tests for primary analysis
  PRIMARY_t <- t.test(df$COUNT ~ df$originalExperiment)$statistic
  PRIMARY_t_p <- t.test(df$COUNT ~ df$originalExperiment)$p.value
  PRIMARY_m_exp <- mean(df$COUNT[df$originalExperiment==1])
  PRIMARY_sd_exp <- sd(df$COUNT[df$originalExperiment==1])
  PRIMARY_m_ctrl <- mean(df$COUNT[df$originalExperiment==0])
  PRIMARY_sd_ctrl <- sd(df$COUNT[df$originalExperiment==0])
  PRIMARY_n_exp <- length(df$COUNT[df$originalExperiment==1])
  PRIMARY_n_ctrl <- length(df$COUNT[df$originalExperiment==0])
  PRIMARY_b <- lm(COUNT~DelayTime, df)
  PRIMARY_d <- cohen.d(df$COUNT, as.factor(df$originalExperiment))$estimate
  PRIMARY_d_CI_UPPER <- cohen.d(df$COUNT, as.factor(df$originalExperiment))$conf.int[2]
  PRIMARY_d_CI_LOWER <- cohen.d(df$COUNT, as.factor(df$originalExperiment))$conf.int[1]
  PRIMARY_se <- std.error(df$COUNT)
  
  
  # For descriptive stats table
  labInfo$mean_exp[labInfo$labID == as.factor(lab)] <- format(PRIMARY_m_exp, digits=3)
  labInfo$sd_exp[labInfo$labID == as.factor(lab)] <- format(PRIMARY_sd_exp, digits=3)
  labInfo$mean_ctrl[labInfo$labID == as.factor(lab)] <- format(PRIMARY_m_ctrl, digits=3)
  labInfo$sd_ctrl[labInfo$labID == as.factor(lab)] <- format(PRIMARY_sd_ctrl, digits=3)

  metaVecES <- c(metaVecES, (PRIMARY_d))
  metaVecSE <- c(metaVecSE, PRIMARY_se)
  
  metaVecMeanExp <- c(metaVecMeanExp, PRIMARY_m_exp)
  metaVecMeanCtrl <- c(metaVecMeanCtrl, PRIMARY_m_ctrl)
  
  mergedDF <- rbind(mergedDF, df)
  
  dfList[[lab]] <- df

  

}

rm(df_main)
rm(df_rating)
rm(df_exclude)


#### DESCRIPTIVE STATISTICS TABLE ####

write.csv(labInfo, "descriptives.csv", row.names = F)


meta <- rma.uni(yi = metaVecES, sei = metaVecSE)



#### FOREST PLOT ####

# Raw ES of original study
THes <- .94-.58
# SE estimate using pooled SD from original study
THse <- sqrt(((1.21^2)+(.67^2)+(.73^2)+(.67^2))/4)/sqrt(120)

# Forest plot code snipped from Wagenmakers et al. (2016)

Cairo(file="forest_main.png", 
      bg="white",
      type="png",
      units="in", 
      width=11, height=7, 
      #pointsize=12, 
      dpi=600)


forest(x = c(THes, metaVecES), sei = c(THse, metaVecSE), xlab="Mean difference", cex.lab=1.4,
       ilab=cbind(c("5.14", format(metaVecMeanExp, digits=3)), c("4.32", format(metaVecMeanCtrl, digits=3))),
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





#### RUN BETWEEN-LABS ANOVA ####
betweenLabsAOV <- aov(COUNT~labID, data=mergedDF)
apa.1way.table(labID, COUNT, mergedDF,"MSD_tab.doc")
apa.aov.table(betweenLabsAOV,"ANOVA_tab.doc")


#### PIRATE PLOTS OF MAIN EFFECT FOR EACH LAB ####
# Snips borrowed from John Sakaluk
# https://sakaluk.wordpress.com/2017/02/03/15-make-it-pretty-diy-pirate-plots-in-ggplot2/

mergedDF$originalExperimentCHR <- "Dental Pain"
mergedDF$originalExperimentCHR[mergedDF$originalExperiment==1] <- "Death"

apatheme=
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text=element_text(family='Arial'),
        legend.title=element_blank(),
        legend.position=c(.7,.8),
        axis.line.x = element_line(color='black'),
        axis.line.y = element_line(color='black'))

pplots <- ggplot(data = mergedDF, aes(x = originalExperimentCHR, y = COUNT))+
  geom_violin(data= mergedDF, aes(x = originalExperimentCHR, y = COUNT))+
  geom_jitter(data= mergedDF, aes(x = originalExperimentCHR, y = COUNT), 
              shape = 1, width = .1)+
  geom_point(size = 3)+
  geom_errorbar(ymax= mean(mergedDF$COUNT)+(1.96*std.error(mergedDF$COUNT)), 
                ymin=mean(mergedDF$COUNT)+(-1.96*std.error(mergedDF$COUNT)), 
                width = 0.25)+
  facet_wrap(~labID)+
  apatheme




#### LINE GRAPHS OF EACH LAB'S FINDINGS ####

all_linear <- ggplot(mergedDF, aes(x = DelayTime, y = COUNT, group = labID)) +
  geom_smooth(method = "loess", se = T, color = "darkgrey") +
  geom_point(alpha = 0.3, size = 0) +
  facet_wrap(~labID)

print(all_linear)

