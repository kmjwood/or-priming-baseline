## Title: or-priming-baseline analysis
## Author: Tina Seabrooke, Katie Wood

## Set up ----
rm(list=ls())

## Load libraries
library(plyr)    # provides 'ddply'
library(Rmisc)   # provides 'summarySE'
library(ez)      # provides 'ezANOVA'
library(ggplot2) # provides 'ggplot'
library(tidyverse) 
library(BayesFactor) # provides 'BayesFactor'
library(pwr)     # calculates power
library("ggpattern")  


## Read data file
data <- read.csv("or-priming-baseline-data.csv", stringsAsFactors = F)

## Custom settings
options(scipen = 999)

## Load custom functions
source('or-priming-baseline-functions.R')

## Training ----

## Subset discriminative training data
training <- subset(data, Running == 'Training')

## Mean accuracy / RT per participant and stimulus
training <- ddply(
  training,
  c('Subject', 'Stimulus'),
  summarise,
  trialnum = length(Running),
  countAcc = sum(Acc),
  percentAcc = 100*(countAcc/trialnum),
  meanRT = mean(RT)
)


## Test ----

## Subset test data in which participants performed an appropriate response
test <- subset(data, Running == 'Test' & Discard == 'False')



## Percent correct per participant across all conditions
testacc <- ddply(
  test,
  c('Subject', 'Age', 'Sex'),
  summarise,
  trialnum = length(Running),
  countAcc = sum(Acc),
  percentAcc = 100*(countAcc/trialnum)
)


## Exclude participants >=80% correct on test trials
testacc <- subset(testacc, percentAcc < 80)

## remove participants that are in the ‘testacc’ data.frame (i.e. those that 
## scored under 80%)
test <- test[!(test$Subject %in% testacc$Subject),]

## Percent correct per condition (excluding below 80% acc)
overallhighacc <- ddply(
  test,
  c('Congruency', 'Delay'),
  summarise,
  trialnum = length(Running),
  countAcc = sum(Acc),
  percentAcc = 100*(countAcc/trialnum)
)

## Percent correct per participant (excluding below 80% acc)
hightestacc <- ddply(
  test,
  c('Subject','Age', 'Sex'),
  summarise,
  trialnum = length(Running),
  countAcc = sum(Acc),
  percentAcc = 100*(countAcc/trialnum)
)


## Rerun descriptive stats for participant sample (excluding below 80% accuracy)

print('Mean, SEMs for age and gender for all participants')
summarySE(hightestacc, measurevar = 'Age')

print('Mean, SEMs for age and gender for all participants')
summarySE(hightestacc, measurevar = 'Age','Sex')


## Percent correct per participant and condition (excluding below 80% acc)
highacc <- ddply(
  test,
  c('Subject','Congruency', 'Delay'),
  summarise,
  trialnum = length(Running),
  countAcc = sum(Acc),
  percentAcc = 100*(countAcc/trialnum)
)

## Test accuracy ANOVA (participants who scored ##over 80% only)
highacc.ez <- ezANOVA(
  dat      = highacc,
  wid      = Subject,
  dv       = percentAcc,
  within   = .(Congruency, Delay),
  detailed = T,
  type     = 3)
print('Test accuracy ANOVA: Congruency x Delay repeated measures ANOVA')
print('DV = percent correct')
print(highacc.ez)

print('Means, SEMs for main effect of congruency')
summarySE(highacc, measurevar = 'percentAcc', groupvars = 'Congruency')

print('Means, SEMs for main effect of delay')
summarySE(highacc, measurevar = 'percentAcc', groupvars = 'Delay')


## Bayes Factor analysis ------------------------------------------------

## Factorise columns for anovaBF

highacc$Congruency <- factor(highacc$Congruency)
highacc$Delay <- factor(highacc$Delay)
highacc$Subject <- factor(highacc$Subject)

bf <- anovaBF(formula = percentAcc ~ Congruency*Delay + Subject,
              data = data.frame(highacc),
              whichRandom = "Subject")

highacc %>% group_by(Congruency) %>% summarise(mean(percentAcc))

bf[4] / bf[3]



## Mean accuracy per pp for each congruency condition (excluding accuracy <80%)
HighAccCong <- ddply(
  test,
  c('Subject','Congruency'),
  summarise,
  trialnum = length(Running),
  countAcc = sum(Acc),
  percentAcc = 100*(countAcc/trialnum)
)

## List of pairwise comparisons for t-tests
pairwise <- list(
  congBase   = subset(HighAccCong, Congruency != 'Incongruent'),
  incongBase = subset(HighAccCong, Congruency != 'Congruent'),
  congIncong = subset(HighAccCong, Congruency != 'Baseline')
)

## Function for pairwise comparisons
tests <- function (p){
  ## t-test
  tt <- t.test(percentAcc ~ Congruency, paired = T, data = p)
  ## Cohen's d_z
  dz <- p[,c("Subject", "Congruency", "percentAcc")]  
  colnames(dz) <- c('id', 'cond', 'dv')
  cd <- cohen.dz(dz)
  ret <- list(tt, cd)
}

## Apply function to each item in list
print("Pairwise comparisons")
lapply(pairwise, tests)



pwr.t.test(d=0.69,n=36,sig.level=0.05,type="paired",alternative="two.sided")



## Mean accuracy per pp for each delay condition (excluding accuracy <80%)
HighAccDelay <- ddply(
  test,
  c('Subject','Delay'),
  summarise,
  trialnum = length(Running),
  countAcc = sum(Acc),
  percentAcc = 100*(countAcc/trialnum)
)

## List of pairwise comparisons for t-tests
delaypairwise <- list(
  longShort   = subset(HighAccDelay)
)

## Function for delay pairwise comparisons
delaytest <- function (p){
  ## t-test
  tt <- t.test(percentAcc ~ Delay, paired = T, data = p)
  ## Cohen's d_z
  dz <- p[,c("Subject", "Delay", "percentAcc")]  
  colnames(dz) <- c('id', 'cond', 'dv')
  cd <- cohen.dz(dz)
  ret <- list(tt, cd)
}

## Apply function to each item in list
print("Pairwise comparisons")
lapply(delaypairwise, delaytest)



library(pwr)     # provides sample size needed for power

pwr.t.test(d=0.33,n=36,sig.level=0.05,type="paired",alternative="two.sided")





## Graphs ----

## Accuracy graph

## Delay conditions
## Convert long data to wide data

## Subset relevant rows and columns
short <- highacc[highacc$Delay == "Short", 
                 c('Subject', 'Congruency', 'percentAcc')]

## convert long data to wide data
short <- reshape(short, direction="wide", timevar="Congruency",idvar="Subject")

## get all columns except the "subject" column
short <- short[,2:4]

## Rename the columns so that they no longer include "percentAcc."
names(short) <- gsub(x = names(short), pattern = "percentAcc.", replacement="")  

## Calc CI
## Difference-adjusted 95% within-subject confidence intervals
short <- cm.ci(short)

## Delay conditions
## Convert long data to wide data
long <- highacc[highacc$Delay == "Long", 
                c('Subject', 'Congruency', 'percentAcc')]
long <- reshape(long, direction="wide", timevar = "Congruency",idvar="Subject")
long <- long[,2:4]
names(long) <- gsub(x = names(long), pattern = "percentAcc.", replacement="")  

## Calc CI
## Difference-adjusted 95% within-subject confidence intervals
long <- cm.ci(long)

## Combine
percentAcc <- cbind(rep(c("Short", "Long"), each = 3), rbind(short, long))
colnames(percentAcc) <- c("Delay", "Congruency", "lower", "av", "upper")

highaccplot <- ggplot(percentAcc, aes(fill=Congruency, y=av, x=Delay, pattern = Congruency, 
                                      pattern_type = Congruency)) +
  geom_bar_pattern(position="dodge", stat="identity", pattern_fill = "black",
                   colour = "black", pattern_spacing = 0.01,
                   pattern_frequency = 5, pattern_angle = 45) +
  ggpubr::theme_pubr() +
  theme(legend.position = "top") +
  theme( panel.background = element_rect(colour = "black", linewidth = 0.5)) +
  labs(x = "Delay", y = "Mean accuracy (%)")+
  scale_y_continuous(breaks=seq(0, 0.8, 0.1), limits = c(0, 0.8)) +
  scale_pattern_manual(values=c('none', 'stripe', 'none')) +
  scale_pattern_type_manual(values=c(NA, NA, NA)) +
  scale_fill_grey(start = .3, end = .7) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = .3,
    linewidth = 1.5,
    position = position_dodge(.9))+
  scale_x_discrete("Delay")+
  scale_y_continuous("Mean accuracy (%)", expand=c(0,0), 
                     breaks=seq(90, 100, by=2))+
  coord_cartesian(ylim=c(90,101))+
  theme_APA

## Grayscale colour scheme
highaccplot <- highaccplot + scale_fill_grey(start = .3, end = .7)
print(highaccplot)

ggsave(filename = "highaccplot.jpg", plot = highaccplot, width = 15, 
       height = 15, units = "cm")



## Reaction times ----

## Subset just correct test trials
hightestRT <- subset(test, Acc == 1)

## Mean RTs per condition 
## (already excluded accuarcy ## below 80% in acc analysis)
overallhightestRT <- ddply(
  hightestRT,
  c('Congruency', 'Delay'),
  summarise,
  meanRT = mean(RT)
)

## Mean reaction time per participant and condition 
## (already excluded accuarcy ## below 80% in acc analysis)
hightestRT <- ddply(
  hightestRT,
  c('Subject', 'Congruency', 'Delay'),
  summarise,
  meanRT = mean(RT)
)

print('Mean, SEM RT for each condition')
summarySE(hightestRT, measurevar = 'meanRT', groupvars = c('Delay', 
                                                           'Congruency'))

## Factorise columns for ezANOVA
cols <- c("Subject", "Congruency", "Delay")
hightestRT[cols] <- lapply(hightestRT[cols], factor)

## RT ANOVA (already excluded accuracy ## below 80% in acc analysis)
highrt.ez <- ezANOVA(
  data     = hightestRT,
  wid      = Subject,
  dv       = meanRT,
  within   = .(Congruency, Delay),
  detailed = T,
  type     = 3
)
print('RT ANOVA: Congruency x Delay')
print(highrt.ez)

print('Mean, SEM for main effect of congruency')
summarySE(hightestRT, measurevar = 'meanRT', groupvars = 'Congruency')

print('Mean, SEM for main effect of delay')
summarySE(hightestRT, measurevar = 'meanRT', groupvars = 'Delay')

## Mean RT per participant and Congruency condition
highRTCong <- ddply(
  test,
  c('Subject','Congruency'),
  summarise,
  meanRT = mean(RT)
)


## Pairwise comparisons for Congruency RT t-tests
pairwise <- list(
  congBase   = subset(highRTCong, Congruency != 'Incongruent'),
  incongBase = subset(highRTCong, Congruency != 'Congruent'),
  congIncong = subset(highRTCong, Congruency != 'Baseline')
)

## Function for pairwise comparisons
tests <- function (p){
  ## t-test
  tt <- t.test(meanRT ~ Congruency, paired = T, data = p)
  ## Cohen's d_z
  dz <- p[,c("Subject", "Congruency", "meanRT")]  
  colnames(dz) <- c('id', 'cond', 'dv')
  cd <- cohen.dz(dz)
  ret <- list(tt, cd)
}

## Apply function to each item in list
print("Pairwise comparisons")
lapply(pairwise, tests)


library(pwr)     # provides sample size needed for power

pwr.t.test(d=0.08,n=36,sig.level=0.05,type="paired",alternative="two.sided")



## Mean RT per participant for each delay condition
RTDelay <- ddply(
  test,
  c('Subject','Delay'),
  summarise,
  meanRT = mean(RT)
)

## List of pairwise comparisons for t-tests
delaypairwise <- list(
  longShort   = subset(RTDelay)
)

## Function for pairwise comparisons
delaytest <- function (p){
  ## t-test
  tt <- t.test(meanRT ~ Delay, paired = T, data = p)
  ## Cohen's d_z
  dz <- p[,c("Subject", "Delay", "meanRT")]  
  colnames(dz) <- c('id', 'cond', 'dv')
  cd <- cohen.dz(dz)
  ret <- list(tt, cd)
}

## Apply function to each item in list
print("Pairwise comparisons")
lapply(delaypairwise, delaytest)


library(pwr)     # provides sample size needed for power

pwr.t.test(d=0.99,n=36,sig.level=0.05,type="paired",alternative="two.sided")


## Graphs ----

## RT graph

## Delay conditions
## Convert long data to wide data

## Subset relevant rows and columns
short <- hightestRT[hightestRT$Delay == "Short", 
                 c('Subject', 'Congruency', 'meanRT')]

## convert long data to wide data
short <- reshape(short, direction="wide", timevar="Congruency",idvar="Subject")

## get all columns except the "subject" column
short <- short[,2:4]

## Rename the columns so that they no longer include "hightestRT"
names(short) <- gsub(x = names(short), pattern = "meanRT", replacement="")  

## Calc CI
## Difference-adjusted 95% within-subject confidence intervals
short <- cm.ci(short)

## Delay conditions
## Convert long data to wide data
long <- hightestRT[hightestRT$Delay == "Long", 
                c('Subject', 'Congruency', 'meanRT')]
long <- reshape(long, direction="wide", timevar = "Congruency",idvar="Subject")
long <- long[,2:4]
names(long) <- gsub(x = names(long), pattern = "meanRT", replacement="")  

## Calc CI
## Difference-adjusted 95% within-subject confidence intervals
long <- cm.ci(long)

## Combine
hightestRT <- cbind(rep(c("Short", "Long"), each = 3), rbind(short, long))
colnames(hightestRT) <- c("Delay", "Congruency", "lower", "av", "upper")

highRTplot <- ggplot(hightestRT, aes(fill=Congruency, y=av, x=Delay, pattern = Congruency, 
                                      pattern_type = Congruency)) +
  geom_bar_pattern(position="dodge", stat="identity", pattern_fill = "black",
                   colour = "black", pattern_spacing = 0.01,
                   pattern_frequency = 5, pattern_angle = 45) +
  ggpubr::theme_pubr() +
  theme(legend.position = "top") +
  theme( panel.background = element_rect(colour = "black", linewidth = 0.5)) +
  labs(x = "Delay Condition", y = "Mean Reaction Time (ms)")+
  scale_y_continuous(breaks=seq(0, 0.8, 0.1), limits = c(0, 0.8)) +
  scale_pattern_manual(values=c('none', 'stripe', 'none')) +
  scale_pattern_type_manual(values=c(NA, NA, NA)) +
  scale_fill_grey(start = .3, end = .7) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = .3,
    linewidth = 1.5,
    position = position_dodge(.9))+
  scale_x_discrete("Delay Condition")+
  scale_y_continuous("Mean Reaction Time (ms)", expand=c(0,0))+
  coord_cartesian(ylim=c(450,750))+
  theme_APA

## Grayscale colour scheme
highRTplot <- highRTplot + scale_fill_grey(start = .3, end = .7)
print(highRTplot)

ggsave(filename = "highRTplot.jpg", plot = highRTplot, width = 15, 
       height = 15, units = "cm")


