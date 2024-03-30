####################################################
##                                                ##
##    Psychometric Analysis of ToM Booklet 2      ##
##                                                ##
##                   MJ Heise                     ##
##                  March 2024                    ##
##                                                ##
####################################################

# CODE DESCRIPTION: This project is a psychometric analysis of the ToM Booklet 2,
# a multi-mental state measure of 3- to 12-year-old children's theory of mind.
# 1. DESCRIPTIVE STATISTICS:
# Summary of age, race and ethnicity, gender, and visit location.
#
# 2. MISSING DATA:
# Examine missingness across items, create composite of average score of 17 
# ToM prediction items, examine whether demographic or lab variables (age,
# gender, testing location, laboratory) predict missingness.
#
# 3. COMPARISONS ACROSS LABS & TESTING ENVIRONMENT:
# ToM composite is used to compared average ToM score across lab and testing 
# environment.


# Libraries
library(tidyverse) # v.2.0.0, data management
library(psych) # v.2.3.9, describe function
library(finalfit) # v.1.0.7, missing data analysis


#### 1. DESCRIPTIVE STATISTICS ####
# Read in data
dat <- read.csv('C:/Users/mheise/Box/Research/BowmanLab/IRT_PsychometricEvaluationofToMBooklet2/Data/Bowman&Richardson_ToMIRTData.csv')
# N = 224

# Table of race grouped by lab
dat %>% 
  group_by(lab) %>% 
  count(race)

# Age grouped by lab
dat %>%
  group_by(lab) %>%
  summarise(ageM = mean(ageYrs, na.rm = T),
         ageSD = sd(ageYrs, na.rm = T))

# Age (overall)
describe(dat$ageYrs)

# Table of gender grouped by lab
dat %>% 
  group_by(lab) %>% 
  count(gender)

# Table of visit location grouped by lab
dat %>% 
  group_by(lab) %>% 
  count(visitLocation)


#### 2. MISSING DATA ####
# Five variables had control questions, and if children earned a 0 on the 
# accompanying control question, they received a score of 0 for the prediction 
# question. These questions were:
# dat$q5.1_fb_pred 
# dat$q8.2_expect_pred 
# dat$q12.1_refHard_pred 
# dat$q17.1_moralFb_pred 
# dat$q18.1_SOFb_pred 

# Missing data cross items
psych::alpha(dat[7:29])
# q1.5.1_fb_pred had 44% missingness (was not administered to Saxe/Richardson)
# sample, so removed from analyses

# Create average score for ToM (correct items/items completed)
tomItems <- c('q1.1_comDes_pred', 'q2.1_divDes_pred', 'q3.1_divBel_pred', 'q4.1_refEasy_pred', 'q5.1_fb_pred', 'q7.1_moralFb_pred',  
              'q8.2_expect_pred', 'q10.1_refHard_pred', 'q11.2_sarCnt_pred', 'q12.1_refHard_pred', 'q13.1_interp_pred', 
              'q14.1_emoRem_pred', 'q15.2_trueBel_pred', 'q16.5_moralTb_pred', 'q17.1_moralFb_pred', 'q18.1_SOFb_pred', 
              'q19.2_sarc_pred')

n=length(tomItems)

dat$tomItemsComplete <- n-apply(X = is.na(dat[tomItems]), MARGIN = 1, FUN = sum) # Number of items each child completed
dat$tomCorrect <- apply(dat[tomItems],1,sum,na.rm=TRUE) # Sum of correct items
dat$tomCompositeAvg <- dat$tomCorrect/dat$tomItemsComplete # Proportion of correct items

table(dat$tomItemsComplete)
# Range of items completed = 10 to 17 (61% of children completed all items)

## Examine whether variables predict missingness
# Gender, visit location, and testing lab
# Saxe/Richardson studies had significantly more missing data but also 
# more preschoolers
explanatory = c('gender', 'visitLocation', 'lab')
dependent = 'tomCompositeAvg'
dat %>%
  summary_factorlist(dependent, explanatory,
                     p=TRUE, add_dependent_label=TRUE)

# Age
explanatory = c('ageYrs')
dependent = 'tomCompositeAvg'
dat %>%
  finalfit(dependent, explanatory)


#### 3. COMPARISONS ACROSS LABS & TESTING ENVIRONMENT ####
## Comparison of remote versus tested in-lab
# Subset Bowman lab data
dat %>%
  filter(lab == 'Bowman') %>%
  pivot_wider(id_cols = subNo,
              names_from = visitLocation, 
              values_from = c('ageYrs', 'gender', 'tomCompositeAvg')) -> datRemote

# T-tests of age and ToM average score by testing location
t.test(datRemote$ageYrs_Lab, datRemote$ageYrs_Zoom)
t.test(datRemote$tomCompositeAvg_Lab, datRemote$tomCompositeAvg_Zoom)

# Chi-square test for gender difference by testing location
genderRemote <- matrix(c(16, 17, 0, 28, 35, 1),nrow=3,ncol=2)
chisq.test(genderRemote)

## Comparison of lab: Sacramento/Bowman lab v. Boston/Saxe & Richardson lab
dat %>%
  pivot_wider(id_cols = subNo,
              names_from = lab, 
              values_from = c('ageYrs', 'gender', 'tomCompositeAvg')) -> datLab

# T-tests of age and ToM average score by lab
t.test(datLab$ageYrs_Richardson, datLab$ageYrs_Bowman)
t.test(datLab$tomCompositeAvg_Richardson, datLab$tomCompositeAvg_Bowman)

# Chi-square test for gender differences by lab
genderLab <- matrix(c(62, 65, 0, 52, 44, 1),nrow=3,ncol=2)
chisq.test(genderLab)

