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
#
# 1. EXPLORATORY FACTOR ANALYSIS:
# Scree plots and very simple structure (VSS) to examine potential factor
# composition of measure.
#
# 2. CONFIRMATORY FACTOR ANALYSIS:
# Fit a single-factor CFA, examine fit indices and fitted covariance matrix. 

# Libraries
library(lavaan) # v.0.6-16, confirmatory factor analysis
library(psych) # v.2.3.9, exploratory factor analysis
library(MASS) # v.7.3-60, exploratory factor analysis
library(semTools) # v.0.5-6, exploratory factor analysis


#### 1. EXPLORATORY FACTOR ANALYSIS ####
# Read in data
dat <- read.csv('C:/Users/mheise/Box/Research/BowmanLab/IRT_PsychometricEvaluationofToMBooklet2/Data/Bowman&Richardson_ToMIRTData.csv')

selectVars <- c('q1.1_comDes_pred',
                  'q2.1_divDes_pred',
                  'q3.1_divBel_pred',
                  'q4.1_refEasy_pred',
                  'q5.1_fb_pred',
                  'q7.1_moralFb_pred',
                  'q8.2_expect_pred',
                  'q10.1_refHard_pred',
                  'q11.2_sarCnt_pred',
                  'q12.1_refHard_pred',
                  'q13.1_interp_pred',
                  'q14.1_emoRem_pred',
                  'q15.2_trueBel_pred',
                  'q16.5_moralTb_pred',
                  'q17.1_moralFb_pred',
                  'q18.1_SOFb_pred',
                  'q19.2_sarc_pred')

datIRT <- dat[selectVars]

# Exploratory factor analysis
psych::scree(datIRT)
# Scree plots of principal components and factor analysis suggest a single factor
  
psych::fa.parallel(datIRT)

VSS(datIRT, n=3)

# Examine alpha for measure
psych::alpha(datIRT)
# Suggests that q15.2_trueBel_pred is negatively correlated with the scale


#### 2. CONFIRMATORY FACTOR ANALYSIS ####
# Define single-factor model
singleFactor <- 'TOM =~ q1.1_comDes_pred + 
  q2.1_divDes_pred +
  q3.1_divBel_pred +
  q4.1_refEasy_pred + 
  q5.1_fb_pred + 
  q7.1_moralFb_pred + 
  q8.2_expect_pred +
  q10.1_refHard_pred +
  q11.2_sarCnt_pred +
  q12.1_refHard_pred +
  q13.1_interp_pred +
  q14.1_emoRem_pred +
  q15.2_trueBel_pred +
  q16.5_moralTb_pred +
  q17.1_moralFb_pred +
  q18.1_SOFb_pred +
  q19.2_sarc_pred'

# Examine single-factor model
singleFactorFit <- cfa(model = singleFactor, data = dat, estimator = "WLSMV", missing = "pairwise", std.lv = TRUE, ordered = TRUE)

summary(singleFactorFit, std = TRUE, fit = TRUE)

# Fit measures
fitmeasures(singleFactorFit)

# Fitted covariance matrix
covMtxCFA <- fitted(singleFactorFit)$cov

