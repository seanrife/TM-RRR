# Power analysis for RRR of Trafimow and Hughes (2012), Study 3
# Sean C. Rife / @seanrife / seanrife.com / srife1@murraystate.edu
# Simulation based on code from Lakens (2015)
# http://daniellakens.blogspot.com/2015/10/practicing-meta-analytic-thinking.html

is.whole <- function(x) {is.numeric(x) && floor(x)==x }

if(!require(ggplot2)){install.packages('ggplot2')}
library(ggplot2)
if(!require(MBESS)){install.packages('MBESS')}
library(MBESS)
if(!require(pwr)){install.packages('pwr')}
library(pwr)
if(!require(meta)){install.packages('meta')}
library(meta)
if(!require(metafor)){install.packages('metafor')}
library(metafor)
if(!require(metap)){install.packages('metap')}
library(metap)
options(digits=10,scipen=999)

# Set up parameters for simulation
# Original values from Trafimow and Hughes:
#   MS/no delay: M=.94, SD=1.21
#   MS/delay: M=.56, SD=.67
#   DPS/no delay: M=.63, SD=.67
#   DPS/delay: M=.56, SD=.73

# Simulate using conservative parameters

set.seed(4263957) # from random.org
mx<-.767138 #Set mean in experimental group, assuming Cohen's d=.19 (half that of T&H)
sdx<-1.21 #Set standard deviation in sample 1 (based on MS/no delay condition from T&H)
my<-.58 #Set mean in control group (based on average across other three conditions from T&H)
sdy<-0.69 #Set standard deviation in sample 2 (based on average across other three conditions from T&H)
nSims <- 10 #number of participating labs
nLoops <- 5000 # Number of iterations used in simulation
sampleMin <- 100 # minimum of 100 participants per lab
sampleMax <- 200 # we assume no lab will recruit more than 200 participants
es.d <-numeric(nSims) #set up empty container for all simulated ES (cohen's d)
SSn1 <-numeric(nSims) #set up empty container for random sample sizes group 1
SSn2 <-numeric(nSims) #set up empty container for random sample sizes group 2
zOut <-numeric(nLoops) #set up empty container for z-values
pOut <-numeric(nLoops) #set up empty container for p-values
for (n in 1:nLoops){
  if (is.whole(n/1000)) print(paste("Iteration #", n, sep=""))
  # Yes, it's a nested loop. I'm a terrible person with a powerful computer.
  for(i in 1:nSims){ #for each simulated experiment
    SampleSize<-sample(sampleMin:sampleMax, 1) #randomly draw a sample
    x<-rnorm(n = round(SampleSize*.25), mean = mx, sd = sdx) #produce  simulated participants in exp. group (1/4 sample size)
    y<-rnorm(n = round(SampleSize*.75), mean = my, sd = sdy) #produce  simulated participants in ctrl. group (3/4 sample size)
    SSn1[i]<-SampleSize #save sample size group 1
    SSn2[i]<-SampleSize #save sample size group 2
    es.d[i]<-smd(Mean.1= mean(x), Mean.2=mean(y), s.1=sd(x), s.2=sd(y), n.1=SampleSize, n.2=SampleSize, Unbiased=FALSE) #Using Cohen's d due to large(ish) sample
  }
  #Insert effect sizes and sample sizes
  n1<-c(SSn1)
  n2<-c(SSn2)
  J<-1-3/(4*( SSn1+ SSn2-2)-1) #correction for bias
  es.d.v <-(((SSn1+SSn2)/(SSn1*SSn2))+(es.d^2/(2*(SSn1+SSn2))))*J^2  #Some formulas add *((n+n)/(n+n-2)) - we don't so that results are consistent with meta-analysis
  #Calculate Standard Errors ES
  d.se<-sqrt(es.d.v)
  #calculate meta-analysis
  meta <- metagen(es.d, d.se)
  zOut[n] <- meta$zval.fixed # not used
  pOut[n] <- meta$pval.fixed
}

print(paste("Raw number of type II errors: ", length(pOut[pOut>.05]), sep=""))
print(paste("Percentage correct @ alpha=.05: ", (length(pOut[pOut<.05])/length(pOut)*100), sep=""))
