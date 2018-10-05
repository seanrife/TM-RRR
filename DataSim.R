# Simulated data for RRR of Trafimow and Hughes (2012), Study 3
# Sean C. Rife / @seanrife / seanrife.com / srife1@murraystate.edu

# Clean house
rm(list = ls())

# Set base directory
# Looks here for main datasets, ratings & exclusions
baseDir <- "D:\\Dropbox\\Research\\TM RRR\\data"
#baseDir <- "C:\\Users\\srife1\\Dropbox\\Research\\TM RRR\\data"

# Set random number generator
set.seed(271228065) # from random.org

# Create container to hold dataset
df <- data.frame()

# Names of labs
labNames <- c("Lab1", "Lab2", "Lab3", "Lab4", "Lab5", "Lab6", "Lab7", "Lab8",
              "Lab9", "Lab10", "Lab11", "Lab12", "Lab13", "Lab14", "Lab15")

# Create simulated data for primary datasets
for (name in labNames){
  nCases <- sample(100:200, 1)
  id <- 1:nCases
  submitdate <- rep_len("1970-01-01 00:00:00", nCases)
  lastpage <- rep_len("8", nCases)
  startlanguage <- rep_len("en", nCases)
  startdate <- rep_len("1970-01-01 00:00:00", nCases)
  datestamp <- rep_len("1970-01-01 00:00:00", nCases)
  CONSENT  <- vector(mode="character", length=nCases)
  CONSENTCHECK_SQ001 <- rep_len("1", nCases)
  essayGroup <- sample(1:2, nCases, replace=T) # 1=MS; 2=DP
  delayGroup <- sample(1:2, nCases, replace=T) # 1=delay; 2=no delay
  labID <- rep_len(name, nCases)
  WTINTRO <- vector(mode="character", length=nCases)
  WTMS1 <- vector(mode="character", length=nCases)
  WTMS2 <- vector(mode="character", length=nCases)
  WTDP1 <- vector(mode="character", length=nCases)
  WTDP2 <- vector(mode="character", length=nCases)
  ARTICLE <- vector(mode="character", length=nCases)
  ARTICLEEVAL_enjoy <- sample(0:1, nCases, replace=T)
  ARTICLEEVAL_interesting <- sample(0:1, nCases, replace=T)
  ARTICLEEVAL_recommend <- sample(0:1, nCases, replace=T)
  ARTICLEEVAL_stay <- sample(0:1, nCases, replace=T)
  WGTASK_word1_response <- vector(mode="character", length=nCases)
  WGTASK_word2_response <- vector(mode="character", length=nCases)
  WGTASK_word3_response <- vector(mode="character", length=nCases)
  WGTASK_word4_response <- vector(mode="character", length=nCases)
  WGTASK_word5_response <- vector(mode="character", length=nCases)
  WSCTASK_S1_response <- vector(mode="character", length=nCases)
  WSCTASK_S2_response <- vector(mode="character", length=nCases)
  WSCTASK_S3_response <- vector(mode="character", length=nCases)
  WSCTASK_S4_response <- vector(mode="character", length=nCases)
  WSCTASK_S5_response <- vector(mode="character", length=nCases)
  WSCTASK_S6_response <- vector(mode="character", length=nCases)
  WSCTASK_S7_response <- vector(mode="character", length=nCases)
  WSCTASK_S8_response <- vector(mode="character", length=nCases)
  WSCTASK_S9_response <- vector(mode="character", length=nCases)
  WSCTASK_S10_response <- vector(mode="character", length=nCases)
  WSCTASK_S11_response <- vector(mode="character", length=nCases)
  WSCTASK_S12_response <- vector(mode="character", length=nCases)
  WSCTASK_S13_response <- vector(mode="character", length=nCases)
  WSCTASK_S14_response <- vector(mode="character", length=nCases)
  WSCTASK_S15_response <- vector(mode="character", length=nCases)
  WSCTASK_S16_response <- vector(mode="character", length=nCases)
  WSCTASK_S17_response <- vector(mode="character", length=nCases)
  WSCTASK_S18_response <- vector(mode="character", length=nCases)
  WSCTASK_S19_response <- vector(mode="character", length=nCases)
  WSCTASK_S20_response <- vector(mode="character", length=nCases)
  WSCTASK_S21_response <- vector(mode="character", length=nCases)
  WSCTASK_S22_response <- vector(mode="character", length=nCases)
  WSCTASK_S23_response <- vector(mode="character", length=nCases)
  WSCTASK_S24_response <- vector(mode="character", length=nCases)
  WSCTASK_S25_response <- vector(mode="character", length=nCases)
  Gender <- sample(1:2, nCases, replace=T)
  Age <- sample(18:36, nCases, replace=T)
  Purpose <- vector(mode="character", length=nCases)
  Understand <- sample(c(1,0), size=nCases, replace=TRUE, prob=c(0.9,0.1))
  Familiar <- sample(c(1,0), size=nCases, replace=TRUE, prob=c(0.1,0.9))
  DEBRIEFING <- vector(mode="character", length=nCases)
  interviewtime <- vector(mode="character", length=nCases) 
  groupTime715 <- vector(mode="character", length=nCases)
  CONSENTTime <- vector(mode="character", length=nCases)
  CONSENTCHECKTime <- vector(mode="character", length=nCases)
  essayGroupTime <- vector(mode="character", length=nCases)
  delayGroupTime <- vector(mode="character", length=nCases)
  labNumTime <- vector(mode="character", length=nCases)
  groupTime716 <- vector(mode="character", length=nCases)
  WTINTROTime <- vector(mode="character", length=nCases)
  groupTime717 <- vector(mode="character", length=nCases)
  WTMS1Time <- vector(mode="character", length=nCases)
  WTMS2Time <- vector(mode="character", length=nCases)
  groupTime718 <- vector(mode="character", length=nCases)
  WTDP1Time <- vector(mode="character", length=nCases)
  WTDP2Time <- vector(mode="character", length=nCases)
  groupTime719 <- as.numeric(sample(60:360, nCases, replace=T))
  ARTICLETime <- vector(mode="character", length=nCases)
  ARTICLEEVALTime <- vector(mode="character", length=nCases)
  groupTime720 <- vector(mode="character", length=nCases)
  WGTASKTime <- vector(mode="character", length=nCases)
  groupTime1019 <- as.numeric(sample(60:360, nCases, replace=T))
  WSCTASKTime <- vector(mode="character", length=nCases)
  groupTime721 <- vector(mode="character", length=nCases)
  GenderTime <- vector(mode="character", length=nCases)
  AgeTime <- vector(mode="character", length=nCases)
  groupTime722 <- vector(mode="character", length=nCases)
  PurposeTime <- vector(mode="character", length=nCases)
  UnderstandTime <- vector(mode="character", length=nCases)
  FamiliarTime <- vector(mode="character", length=nCases)
  groupTime723 <- vector(mode="character", length=nCases)
  DEBRIEFINGTime <- vector(mode="character", length=nCases)
  COUNT_DV1 <- as.numeric(sample(c(0,5), size=nCases, replace=TRUE, prob=c(0.3,0.7)))
  COUNT_DV2 <- as.numeric(sample(c(0,5), size=nCases, replace=TRUE, prob=c(0.3,0.7)))
  FLAG <- sample(c(1,0), size=nCases, replace=TRUE, prob=c(0.1,0.9))

  # Assign to a dataframe
  simDFmain <- data.frame(id,submitdate,lastpage,startlanguage,startdate,
                      datestamp,CONSENT,CONSENTCHECK_SQ001,essayGroup,
                      delayGroup,labID,WTINTRO,WTMS1,WTMS2,WTDP1,WTDP2,
                      ARTICLE,ARTICLEEVAL_enjoy,ARTICLEEVAL_interesting,
                      ARTICLEEVAL_recommend,ARTICLEEVAL_stay,
                      WGTASK_word1_response,WGTASK_word2_response,
                      WGTASK_word3_response,WGTASK_word4_response,
                      WGTASK_word5_response,WSCTASK_S1_response,
                      WSCTASK_S2_response,WSCTASK_S3_response,
                      WSCTASK_S4_response,WSCTASK_S5_response,
                      WSCTASK_S6_response,WSCTASK_S7_response,
                      WSCTASK_S8_response,WSCTASK_S9_response,
                      WSCTASK_S10_response,WSCTASK_S11_response,
                      WSCTASK_S12_response,WSCTASK_S13_response,
                      WSCTASK_S14_response,WSCTASK_S15_response,
                      WSCTASK_S16_response,WSCTASK_S17_response,
                      WSCTASK_S18_response,WSCTASK_S19_response,
                      WSCTASK_S20_response,WSCTASK_S21_response,
                      WSCTASK_S22_response,WSCTASK_S23_response,
                      WSCTASK_S24_response,WSCTASK_S25_response,
                      Gender,Age,Purpose,Understand,Familiar,
                      DEBRIEFING,interviewtime,groupTime715,CONSENTTime,
                      CONSENTCHECKTime,essayGroupTime,delayGroupTime,
                      labNumTime,groupTime716,WTINTROTime,groupTime717,
                      WTMS1Time,WTMS2Time,groupTime718,WTDP1Time,
                      WTDP2Time,groupTime719,ARTICLETime,ARTICLEEVALTime,
                      groupTime720,WGTASKTime,groupTime1019,WGTASKTime,
                      groupTime721,GenderTime,AgeTime,groupTime722,
                      PurposeTime,UnderstandTime,FamiliarTime,
                      groupTime723,DEBRIEFINGTime)
  
  # Assign column names
  colnames(simDFmain) <- c("id","submitdate","lastpage","startlanguage","startdate",
                           "datestamp","CONSENT","CONSENTCHECK_SQ001","essayGroup",
                           "delayGroup","labID","WTINTRO","WTMS1","WTMS2","WTDP1","WTDP2",
                           "ARTICLE","ARTICLEEVAL_enjoy","ARTICLEEVAL_interesting",
                           "ARTICLEEVAL_recommend","ARTICLEEVAL_stay",
                           "WGTASK_word1_response","WGTASK_word2_response",
                           "WGTASK_word3_response","WGTASK_word4_response",
                           "WGTASK_word5_response","WSCTASK_S1_response",
                           "WSCTASK_S2_response","WSCTASK_S3_response",
                           "WSCTASK_S4_response","WSCTASK_S5_response",
                           "WSCTASK_S6_response","WSCTASK_S7_response",
                           "WSCTASK_S8_response","WSCTASK_S9_response",
                           "WSCTASK_S10_response","WSCTASK_S11_response",
                           "WSCTASK_S12_response","WSCTASK_S13_response",
                           "WSCTASK_S14_response","WSCTASK_S15_response",
                           "WSCTASK_S16_response","WSCTASK_S17_response",
                           "WSCTASK_S18_response","WSCTASK_S19_response",
                           "WSCTASK_S20_response","WSCTASK_S21_response",
                           "WSCTASK_S22_response","WSCTASK_S23_response",
                           "WSCTASK_S24_response","WSCTASK_S25_response",
                           "Gender","Age","Purpose","Understand","Familiar",
                           "DEBRIEFING","interviewtime","groupTime715","CONSENTTime",
                           "CONSENTCHECKTime","essayGroupTime","delayGroupTime",
                           "labNumTime","groupTime716","WTINTROTime","groupTime717",
                           "WTMS1Time","WTMS2Time","groupTime718","WTDP1Time",
                           "WTDP2Time","groupTime719","ARTICLETime","ARTICLEEVALTime",
                           "groupTime720","WGTASKTime","groupTime1019","WGTASKTime",
                           "groupTime721","GenderTime","AgeTime","groupTime722",
                           "PurposeTime","UnderstandTime","FamiliarTime",
                           "groupTime723","DEBRIEFINGTime")
  
  # Assign blank values for time variable for participants in the no delay condition
  simDFmain$groupTime719[simDFmain$delayGroup==2] <- 0
  
  # Change age values to int so they make sense
  simDFmain$Age <- round(simDFmain$Age)

  # Write to an output file
  write.csv(simDFmain, file = paste0(baseDir,"\\", name,"_main.csv"),row.names=FALSE, na="")
  
  # Moving on to ranking dataset
  
  simDF1rating <- data.frame(id,labID,WGTASK_word1_response,WGTASK_word2_response,
                            WGTASK_word3_response,WGTASK_word4_response,
                            WGTASK_word5_response,COUNT_DV1)
  
  colnames(simDF1rating) <- c("id","labID","WORD1","WORD2","WORD3","WORD4",
                             "WORD5","COUNT_DV1")
  
  # Write to an output file
  write.csv(simDF1rating, file = paste0(baseDir,"\\",name,"_DV1_rating.csv"),row.names=FALSE, na="")
  
  
  simDF2rating <- data.frame(id,labID,WSCTASK_S1_response,WSCTASK_S5_response,
                             WSCTASK_S12_response,WSCTASK_S15_response,
                             WSCTASK_S19_response,WSCTASK_S22_response,COUNT_DV2)
  
  colnames(simDF2rating) <- c("id","labID","WORD1","WORD2","WORD3","WORD4",
                              "WORD5","WORD6","COUNT_DV2")
  
  # Write to an output file
  write.csv(simDF2rating, file = paste0(baseDir,"\\",name,"_DV2_rating.csv"),row.names=FALSE, na="")
  
  # Move on to exclusions dataset
  simDFexclude <- data.frame(id,labID,Purpose,Understand,Familiar,FLAG)
  
  colnames(simDFexclude) <- c("id","labID","PURPOSE","RESPONSE1","RESPONSE2",
                             "FLAG")
  
  # Write to an output file
  write.csv(simDFexclude, file = paste0(baseDir,"\\", name,"_exclude.csv"),row.names=FALSE, na="")
  
  
  
}

