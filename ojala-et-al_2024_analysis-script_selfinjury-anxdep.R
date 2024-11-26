#### Analysis code for
#### Ojala et al. 2024
#### "The trajectory of anxiety and depressive symptoms and the impact of 
#### self-injury: a longitudinal 12-month cohort study of individuals with 
#### psychiatric symptoms"
#### published 2024-11-21 in PLOS ONE
# This is R code for the analyses in the manuscript
# Code developed by Maria Åbonde Garke
# email: maria.garke@ki.se

#### PACKAGES LOADED ####
library(plyr)
library(readxl)
library(nlme)
library(lmerTest)
library(lme4)
library(clubSandwich)
library(psych)
library(lubridate)
library(expss)
library(jtools)
library(sjPlot)
library(piecewiseSEM)
library(Rmisc)
library(VIM)
library(BSDA)
library(car)
library(ggpubr)
library(gghighlight)
library(viridis)
library(tidyverse)


#### DATA IMPORT ####


#### DATA CLEANING ####

## compute total scores on GAD-7 and PHQ-9
covid_all <- covid_all %>%
  mutate(gad7_tot_t1 = gad7_item1_t1 + gad7_item2_t1 + gad7_item3_t1 + 
           gad7_item4_t1 + gad7_item5_t1 + gad7_item6_t1 + gad7_item7_t1)

covid_all <- covid_all %>%
  mutate(gad7_tot_t2 = gad7_item1_t2 + gad7_item2_t2 + gad7_item3_t2 + 
           gad7_item4_t2 + gad7_item5_t2 + gad7_item6_t2 + gad7_item7_t2)

covid_all <- covid_all %>%
  mutate(gad7_tot_t3 = gad7_item1_t3 + gad7_item2_t3 + gad7_item3_t3 + 
           gad7_item4_t3 + gad7_item5_t3 + gad7_item6_t3 + gad7_item7_t3)

covid_all <- covid_all %>%
  mutate(gad7_tot_t4 = gad7_item1_t4 + gad7_item2_t4 + gad7_item3_t4 + 
           gad7_item4_t4 + gad7_item5_t4 + gad7_item6_t4 + gad7_item7_t4)

covid_all <- covid_all %>%
  mutate(gad7_tot_t5 = gad7_item1_t5 + gad7_item2_t5 + gad7_item3_t5 + 
           gad7_item4_t5 + gad7_item5_t5 + gad7_item6_t5 + gad7_item7_t5)

covid_all <- covid_all %>%
  mutate(gad7_tot_t6 = gad7_item1_t6 + gad7_item2_t6 + gad7_item3_t6 + 
           gad7_item4_t6 + gad7_item5_t6 + gad7_item6_t6 + gad7_item7_t6)

covid_all <- covid_all %>%
  mutate(gad7_tot_t7 = gad7_item1_t7 + gad7_item2_t7 + gad7_item3_t7 + 
           gad7_item4_t7 + gad7_item5_t7 + gad7_item6_t7 + gad7_item7_t7)

covid_all <- covid_all %>%
  mutate(gad7_tot_t8 = gad7_item1_t8 + gad7_item2_t8 + gad7_item3_t8 + 
           gad7_item4_t8 + gad7_item5_t8 + gad7_item6_t8 + gad7_item7_t8)

covid_all <- covid_all %>%
  mutate(gad7_tot_t9 = gad7_item1_t9 + gad7_item2_t9 + gad7_item3_t9 + 
           gad7_item4_t9 + gad7_item5_t9 + gad7_item6_t9 + gad7_item7_t9)

covid_all <- covid_all %>%
  mutate(gad7_tot_t10 = gad7_item1_t10 + gad7_item2_t10 + gad7_item3_t10 + 
           gad7_item4_t10 + gad7_item5_t10 + gad7_item6_t10 + gad7_item7_t10)

covid_all <- covid_all %>%
  mutate(gad7_tot_t11 = gad7_item1_t11 + gad7_item2_t11 + gad7_item3_t11 + 
           gad7_item4_t11 + gad7_item5_t11 + gad7_item6_t11 + gad7_item7_t11)

covid_all <- covid_all %>%
  mutate(gad7_tot_t12 = gad7_item1_t12 + gad7_item2_t12 + gad7_item3_t12 + 
           gad7_item4_t12 + gad7_item5_t12 + gad7_item6_t12 + gad7_item7_t12)

covid_all <- covid_all %>%
  mutate(gad7_tot_t13 = gad7_item1_t13 + gad7_item2_t13 + gad7_item3_t13 + 
           gad7_item4_t13 + gad7_item5_t13 + gad7_item6_t13 + gad7_item7_t13)

covid_all <- covid_all %>%
  mutate(gad7_tot_t14 = gad7_item1_t14 + gad7_item2_t14 + gad7_item3_t14 + 
           gad7_item4_t14 + gad7_item5_t14 + gad7_item6_t14 + gad7_item7_t14)



covid_all <- covid_all %>%
  mutate(phq9_tot_t1 = phq9_item1_t1 + phq9_item2_t1 + phq9_item3_t1 + 
           phq9_item4_t1 + phq9_item5_t1 + phq9_item6_t1 + phq9_item7_t1 +
           phq9_item8_t1 + phq9_item9_t1)

covid_all <- covid_all %>%
  mutate(phq9_tot_t2 = phq9_item1_t2 + phq9_item2_t2 + phq9_item3_t2 + 
           phq9_item4_t2 + phq9_item5_t2 + phq9_item6_t2 + phq9_item7_t2 +
           phq9_item8_t2 + phq9_item9_t2)

covid_all <- covid_all %>%
  mutate(phq9_tot_t3 = phq9_item1_t3 + phq9_item2_t3 + phq9_item3_t3 + 
           phq9_item4_t3 + phq9_item5_t3 + phq9_item6_t3 + phq9_item7_t3 +
           phq9_item8_t3 + phq9_item9_t3)

covid_all <- covid_all %>%
  mutate(phq9_tot_t4 = phq9_item1_t4 + phq9_item2_t4 + phq9_item3_t4 + 
           phq9_item4_t4 + phq9_item5_t4 + phq9_item6_t4 + phq9_item7_t4 +
           phq9_item8_t4 + phq9_item9_t4)

covid_all <- covid_all %>%
  mutate(phq9_tot_t5 = phq9_item1_t5 + phq9_item2_t5 + phq9_item3_t5 + 
           phq9_item4_t5 + phq9_item5_t5 + phq9_item6_t5 + phq9_item7_t5 +
           phq9_item8_t5 + phq9_item9_t5)

covid_all <- covid_all %>%
  mutate(phq9_tot_t6 = phq9_item1_t6 + phq9_item2_t6 + phq9_item3_t6 + 
           phq9_item4_t6 + phq9_item5_t6 + phq9_item6_t6 + phq9_item7_t6 +
           phq9_item8_t6 + phq9_item9_t6)

covid_all <- covid_all %>%
  mutate(phq9_tot_t7 = phq9_item1_t7 + phq9_item2_t7 + phq9_item3_t7 + 
           phq9_item4_t7 + phq9_item5_t7 + phq9_item6_t7 + phq9_item7_t7 +
           phq9_item8_t7 + phq9_item9_t7)

covid_all <- covid_all %>%
  mutate(phq9_tot_t8 = phq9_item1_t8 + phq9_item2_t8 + phq9_item3_t8 + 
           phq9_item4_t8 + phq9_item5_t8 + phq9_item6_t8 + phq9_item7_t8 +
           phq9_item8_t8 + phq9_item9_t8)

covid_all <- covid_all %>%
  mutate(phq9_tot_t9 = phq9_item1_t9 + phq9_item2_t9 + phq9_item3_t9 + 
           phq9_item4_t9 + phq9_item5_t9 + phq9_item6_t9 + phq9_item7_t9 +
           phq9_item8_t9 + phq9_item9_t9)

covid_all <- covid_all %>%
  mutate(phq9_tot_t10 = phq9_item1_t10 + phq9_item2_t10 + phq9_item3_t10 + 
           phq9_item4_t10 + phq9_item5_t10 + phq9_item6_t10 + phq9_item7_t10 +
           phq9_item8_t10 + phq9_item9_t10)

covid_all <- covid_all %>%
  mutate(phq9_tot_t11 = phq9_item1_t11 + phq9_item2_t11 + phq9_item3_t11 + 
           phq9_item4_t11 + phq9_item5_t11 + phq9_item6_t11 + phq9_item7_t11 +
           phq9_item8_t11 + phq9_item9_t11)

covid_all <- covid_all %>%
  mutate(phq9_tot_t12 = phq9_item1_t12 + phq9_item2_t12 + phq9_item3_t12 + 
           phq9_item4_t12 + phq9_item5_t12 + phq9_item6_t12 + phq9_item7_t12 +
           phq9_item8_t12 + phq9_item9_t12)

covid_all <- covid_all %>%
  mutate(phq9_tot_t13 = phq9_item1_t13 + phq9_item2_t13 + phq9_item3_t13 + 
           phq9_item4_t13 + phq9_item5_t13 + phq9_item6_t13 + phq9_item7_t13 +
           phq9_item8_t13 + phq9_item9_t13)

covid_all <- covid_all %>%
  mutate(phq9_tot_t14 = phq9_item1_t14 + phq9_item2_t14 + phq9_item3_t14 + 
           phq9_item4_t14 + phq9_item5_t14 + phq9_item6_t14 + phq9_item7_t14 +
           phq9_item8_t14 + phq9_item9_t14)


#make all columns numeric except timestamp variables
covid_all = covid_all %>% mutate(across(.cols=2:131, .fns=as.numeric))
covid_all = covid_all %>% mutate(across(.cols=146:306, .fns=as.numeric))
covid_all = covid_all %>% mutate(across(.cols=321:378, .fns=as.numeric))
covid_all = covid_all %>% mutate(across(.cols=380, .fns=as.numeric))
covid_all = covid_all %>% mutate(across(.cols=382, .fns=as.numeric))
covid_all = covid_all %>% mutate(across(.cols=384:410, .fns=as.numeric))

#make timestamp variables into date
covid_all = covid_all %>% mutate(across(.cols=132:145, .fns=as.Date))
covid_all = covid_all %>% mutate(across(.cols=307:320, .fns=as.Date))
covid_all = covid_all %>% mutate(across(.cols=379, .fns=as.Date))
covid_all = covid_all %>% mutate(across(.cols=381, .fns=as.Date))

covid_all = covid_all %>% mutate(across(.cols=132:145, .fns=ymd))
covid_all = covid_all %>% mutate(across(.cols=307:320, .fns=ymd))
covid_all = covid_all %>% mutate(across(.cols=379, .fns=ymd))
covid_all = covid_all %>% mutate(across(.cols=381, .fns=ymd))


#sort out participants who: have not consented, participants aged > 100 years,
#have not given their email at baseline

#in original data file n = 7605

covid_all <- covid_all %>%
  filter(samtycke_t1 == 1)
#results in n = 7451

covid_all <- covid_all %>%
  filter(cpfcconsent_complete_t1 == 2)
#results in n = 7258

#remove one instance of age=1000 and two instances of age < 18
covid_all <- covid_all %>%
  filter(age_t1 != 1000 | is.na(age_t1)) %>%
  filter(age_t1 >= 18 | is.na(age_t1))
#results in 7255

#insert 0s for taf11 and taf14 due to skip items based on taf10 and taf13
covid_all <- covid_all %>%
  mutate(across(taf_item11_t1, ~ case_when(taf_item10_t1 == 0 ~ 0,
                                                     TRUE ~ .)))

covid_all <- covid_all %>%
  mutate(across(taf_item14_t1, ~ case_when(taf_item13_t1 == 0 ~ 0,
                                           TRUE ~ .)))

#fix ID variable so that its numeric instead of character
covid_all$subjectID <- str_replace(covid_all$subjectID, "sub-", "")
covid_all$subjectID <- str_replace(covid_all$subjectID, "CPFCOV", "")
covid_all$subjectID <- substr(covid_all$subjectID, 1, 4)

covid_all$subjectID <- as.numeric(covid_all$subjectID)

### The sample we use for our analysis ###
#t1 & t2 | t3 | t4 | t5 & t6 | t7 | t8 | t9 & t10 | t11 | t12 | t13

covid_main <- covid_all %>%
  drop_na(gad7_tot_t1, phq9_tot_t1, taf_item10_t1, taf_item13_t1) %>%
  filter(!is.na(gad7_tot_t2) | !is.na(gad7_tot_t3) | !is.na(gad7_tot_t4) | !is.na(gad7_tot_t5)) %>%
  filter(!is.na(phq9_tot_t2) | !is.na(phq9_tot_t3) | !is.na(phq9_tot_t4) | !is.na(phq9_tot_t5)) %>%
  filter(!is.na(gad7_tot_t6) | !is.na(gad7_tot_t7) | !is.na(gad7_tot_t8) | !is.na(gad7_tot_t9) ) %>%
  filter(!is.na(phq9_tot_t6) | !is.na(phq9_tot_t7) | !is.na(phq9_tot_t8) | !is.na(phq9_tot_t9)) %>%
  filter(!is.na(gad7_tot_t10) | !is.na(gad7_tot_t11) | !is.na(gad7_tot_t12) | !is.na(gad7_tot_t13)) %>%
  filter(!is.na(phq9_tot_t10) | !is.na(phq9_tot_t11) | !is.na(phq9_tot_t12) | !is.na(phq9_tot_t13))

nrow(covid_main)
#results in our sample n=1839

#check for duplicates in email address
#covid_main <- covid_main %>%
#  distinct(email_t1, .keep_all = TRUE)
#results in 1810 cases

#filter away duplicate emails and keep row with earliest timestamp
covid_main <- covid_main %>%
  group_by(email_t1) %>%
  slice(which.min(cpfcconsent_timestamp_t1)) %>%
  ungroup()


#for estimation of full sample size (in comparison to our sample), make df without our sample
covid_our <- covid_main %>%
  mutate(our = 1)

covid_our <- covid_our %>%
  select(subjectID, our)

covid_all <- left_join(covid_all, covid_our, by = "subjectID")

covid_all$our[is.na(covid_all$our)] <- 0
  
covid_base <- covid_all %>%
  filter(our == 0)
#results in 5416 cases left when sorting out those fulfilling  criteria (+ duplicates in covid_main)

#need to sort out those first who have not provided email address
covid_noemail <- covid_base %>%
  filter(email_t1 == "")
#1051 persons who have not provided email address but have consented

#then actually filter them out from covid_base
covid_base <- covid_base %>% 
  filter(email_t1 != "")

#now in covid_base, sort out duplicates based on email and first timestamp
covid_base <- covid_base %>%
  group_by(email_t1) %>%
  slice(which.min(cpfcconsent_timestamp_t1)) %>%
  ungroup()
#results in n=4126 who have consented to longitudinal ie have email (without duplicates)

#merge covid_base and covid_noemail again to look at descriptives across these groups
covid_all_nd <- bind_rows(covid_base, covid_noemail)

#for covid_base, remove those who have not provided demographic info
covid_base_demo <- covid_base %>%
  drop_na(sex_t1, age_t1, education_t1, cpfcorigin_item1_t1,
          taf_item10_t1, taf_item13_t1, phq9_tot_t1, gad7_tot_t1)

#for covid_base, derive self-injury thoughts
covid_base_demo <- covid_base_demo %>%
  mutate(si_tot_t1 = ifelse(phq9_item9_t1 > 0, 1, 0))


#### DESCRIPTIVES AND DATA CHECKS ####

hist(covid_missing$gad7_tot_t1)

hist(covid_missing$gad7_tot_t2)

hist(covid_missing$gad7_tot_t3)

hist(covid_missing$gad7_tot_t4)

hist(covid_missing$gad7_tot_t5)

hist(covid_missing$gad7_tot_t6)

hist(covid_missing$gad7_tot_t7)

hist(covid_missing$phq9_tot_t1)

hist(covid_missing$phq9_tot_t2)

hist(covid_missing$phq9_tot_t3)

hist(covid_missing$phq9_tot_t4)

hist(covid_missing$phq9_tot_t5)

hist(covid_missing$phq9_tot_t6)

hist(covid_missing$phq9_tot_t7)


## transforming to long format to visualize mean group change over time
#dropping also those who have missing at baseline first
covid_missing_long <- covid_missing %>%
  drop_na(gad7_tot_t1:gad7_tot_t7, phq9_tot_t1:phq9_tot_t7, taf_item10_t1, taf_item13_t1) %>%
  select(subjectID, gad7_tot_t1:gad7_tot_t7, gad7_tot_t12:gad7_tot_t14, phq9_tot_t1:phq9_tot_t7,
         phq9_tot_t12:phq9_tot_t14, taf_item10_t1:taf_item14_t1) %>%
  filter(!is.na(gad7_tot_t12) | !is.na(gad7_tot_t13) | !is.na(gad7_tot_t14)) %>%
  filter(!is.na(phq9_tot_t12) | !is.na(phq9_tot_t13) | !is.na(phq9_tot_t14)) %>%
  pivot_longer(cols = 2:25, names_to = c("variable", "tot", "time"), 
               names_sep = "_", values_to = "value") %>%
  select(-tot)

covid_missing_long$time <- str_replace(covid_missing_long$time, "t", "")

covid_missing_long$time <- as.numeric(covid_missing_long$time)


### Demo/desc main sample ###

## table 1
table(covid_main$education)
prop.table(table(covid_main$education))
table(covid_main$sex)
prop.table(table(covid_main$sex))
describe(covid_main$age)

table(covid_main$cpfcorigin_item1_t1)

table(covid_main$rampemploy_item1c1_t1)
table(covid_main$rampemploy_item1c2_t1)
table(covid_main$rampemploy_item1c3_t1)
table(covid_main$rampemploy_item1c4_t1)
table(covid_main$rampemploy_item1c5_t1)
table(covid_main$rampemploy_item1c6_t1)
table(covid_main$rampemploy_item1c7_t1)
table(covid_main$rampemploy_item1c8_t1)
table(covid_main$rampemploy_item1c9_t1)
table(covid_main$rampemploy_item1c10_t1)
table(covid_main$rampemploy_item1c11_t1)
table(covid_main$rampemploy_item1c12_t1)


table(covid_main$rampdiagnos_item1c1_t1)
table(covid_main$rampdiagnos_item1c2_t1)
table(covid_main$rampdiagnos_item1c3_t1)
table(covid_main$rampdiagnos_item1c4_t1)
table(covid_main$rampdiagnos_item1c5_t1)
table(covid_main$rampdiagnos_item1c6_t1)
table(covid_main$rampdiagnos_item1c7_t1)
table(covid_main$rampdiagnos_item1c8_t1)
table(covid_main$rampdiagnos_item1c9_t1)
table(covid_main$rampdiagnos_item1c10_t1)
table(covid_main$rampdiagnos_item1c11_t1)
table(covid_main$rampdiagnos_item1c12_t1)
table(covid_main$rampdiagnos_item1c13_t1)
table(covid_main$rampdiagnos_item1c14_t1)
table(covid_main$rampdiagnos_item1c15_t1)
table(covid_main$rampdiagnos_item1c16_t1)
table(covid_main$rampdiagnos_item1c17_t1)
table(covid_main$rampdiagnos_item1c18_t1)
table(covid_main$rampdiagnos_item1c19_t1)
table(covid_main$rampdiagnos_item1c20_t1)
table(covid_main$rampdiagnos_item1c21_t1)

table(covid_main$tafitem10)
table(covid_main$tafitem11)

table(covid_main$tafitem13)
table(covid_main$tafitem14)

table(covid_main$with, covid_main$without)


## table 2

describe(covid_main$phq9_tot_t1)
describe(covid_main$phq9_tot_t2)
describe(covid_main$phq9_tot_t3)
describe(covid_main$phq9_tot_t4)
describe(covid_main$phq9_tot_t5)
describe(covid_main$phq9_tot_t6)
describe(covid_main$phq9_tot_t7)
describe(covid_main$phq9_tot_t8)
describe(covid_main$phq9_tot_t9)
describe(covid_main$phq9_tot_t10)
describe(covid_main$phq9_tot_t11)
describe(covid_main$phq9_tot_t12)
describe(covid_main$phq9_tot_t13)

describe(covid_main$gad7_tot_t1)
describe(covid_main$gad7_tot_t2)
describe(covid_main$gad7_tot_t3)
describe(covid_main$gad7_tot_t4)
describe(covid_main$gad7_tot_t5)
describe(covid_main$gad7_tot_t6)
describe(covid_main$gad7_tot_t7)
describe(covid_main$gad7_tot_t8)
describe(covid_main$gad7_tot_t9)
describe(covid_main$gad7_tot_t10)
describe(covid_main$gad7_tot_t11)
describe(covid_main$gad7_tot_t12)
describe(covid_main$gad7_tot_t13)

table(covid_main$si_tot_t1)
prop.table(table(covid_main$si_tot_t1))
table(covid_main$si_tot_t2)
prop.table(table(covid_main$si_tot_t2))
table(covid_main$si_tot_t3)
prop.table(table(covid_main$si_tot_t3))
table(covid_main$si_tot_t4)
prop.table(table(covid_main$si_tot_t4))
table(covid_main$si_tot_t5)
prop.table(table(covid_main$si_tot_t5))
table(covid_main$si_tot_t6)
prop.table(table(covid_main$si_tot_t6))
table(covid_main$si_tot_t7)
prop.table(table(covid_main$si_tot_t7))
table(covid_main$si_tot_t8)
prop.table(table(covid_main$si_tot_t8))
table(covid_main$si_tot_t9)
prop.table(table(covid_main$si_tot_t9))
table(covid_main$si_tot_t10)
prop.table(table(covid_main$si_tot_t10))
table(covid_main$si_tot_t11)
prop.table(table(covid_main$si_tot_t11))
table(covid_main$si_tot_t12)
prop.table(table(covid_main$si_tot_t12))
table(covid_main$si_tot_t13)
prop.table(table(covid_main$si_tot_t13))


### Demo/desc base sample ###

## table 1
table(covid_base_demo$education_t1)
prop.table(table(covid_base_demo$education_t1))
table(covid_base_demo$sex_t1)
prop.table(table(covid_base_demo$sex_t1))
describe(covid_base_demo$age_t1)

table(covid_base_demo$cpfcorigin_item1_t1)

#fulltime
table(covid_base_demo$rampemploy_item1c1_t1)
#parttime
table(covid_base_demo$rampemploy_item1c2_t1)
#unemployed
table(covid_base_demo$rampemploy_item1c3_t1)
#hourly
table(covid_base_demo$rampemploy_item1c4_t1)
table(covid_base_demo$rampemploy_item1c5_t1)
table(covid_base_demo$rampemploy_item1c6_t1)
table(covid_base_demo$rampemploy_item1c7_t1)
table(covid_base_demo$rampemploy_item1c8_t1)
table(covid_base_demo$rampemploy_item1c9_t1)
#retired
table(covid_base_demo$rampemploy_item1c10_t1)
#student
table(covid_base_demo$rampemploy_item1c11_t1)
table(covid_base_demo$rampemploy_item1c12_t1)

#gad
table(covid_base_demo$rampdiagnos_item1c1_t1)
#social anxiety
table(covid_base_demo$rampdiagnos_item1c2_t1)
table(covid_base_demo$rampdiagnos_item1c3_t1)
table(covid_base_demo$rampdiagnos_item1c4_t1)
table(covid_base_demo$rampdiagnos_item1c5_t1)
#panic disorder
table(covid_base_demo$rampdiagnos_item1c6_t1)
table(covid_base_demo$rampdiagnos_item1c7_t1)
#ptsd
table(covid_base_demo$rampdiagnos_item1c8_t1)
#depression
table(covid_base_demo$rampdiagnos_item1c9_t1)
#depression
table(covid_base_demo$rampdiagnos_item1c10_t1)
table(covid_base_demo$rampdiagnos_item1c11_t1)
#bipolar
table(covid_base_demo$rampdiagnos_item1c12_t1)
table(covid_base_demo$rampdiagnos_item1c13_t1)
#eating disorder
table(covid_base_demo$rampdiagnos_item1c14_t1)
#eating disorder
table(covid_base_demo$rampdiagnos_item1c15_t1)
table(covid_base_demo$rampdiagnos_item1c16_t1)
table(covid_base_demo$rampdiagnos_item1c17_t1)
#neuro
table(covid_base_demo$rampdiagnos_item1c18_t1)
table(covid_base_demo$rampdiagnos_item1c19_t1)
table(covid_base_demo$rampdiagnos_item1c20_t1)
#bipolar
table(covid_base_demo$rampdiagnos_item1c21_t1)

table(covid_base_demo$taf_item10_t1)
table(covid_base_demo$taf_item11_t1)

table(covid_base_demo$taf_item13_t1)
table(covid_base_demo$taf_item14_t1)


describe(covid_base_demo$phq9_tot_t1)
describe(covid_base_demo$gad7_tot_t1)
table(covid_base_demo$si_tot_t1)


### Demo/desc lifetime without sample ###

covid_lifetimewithout <- covid_main %>%
  filter(tafitem10 == 1)

## table 1
table(covid_lifetimewithout$education)
prop.table(table(covid_lifetimewithout$education))
table(covid_lifetimewithout$sex)
prop.table(table(covid_lifetimewithout$sex))
describe(covid_lifetimewithout$age)

table(covid_lifetimewithout$cpfcorigin_item1_t1)

#fulltime
table(covid_lifetimewithout$rampemploy_item1c1_t1)
#parttime
table(covid_lifetimewithout$rampemploy_item1c2_t1)
#unemployed
table(covid_lifetimewithout$rampemploy_item1c3_t1)
#hourly
table(covid_lifetimewithout$rampemploy_item1c4_t1)
table(covid_lifetimewithout$rampemploy_item1c5_t1)
table(covid_lifetimewithout$rampemploy_item1c6_t1)
table(covid_lifetimewithout$rampemploy_item1c7_t1)
table(covid_lifetimewithout$rampemploy_item1c8_t1)
table(covid_lifetimewithout$rampemploy_item1c9_t1)
#retired
table(covid_lifetimewithout$rampemploy_item1c10_t1)
#student
table(covid_lifetimewithout$rampemploy_item1c11_t1)
table(covid_lifetimewithout$rampemploy_item1c12_t1)

#gad
table(covid_lifetimewithout$rampdiagnos_item1c1_t1)
#social anxiety
table(covid_lifetimewithout$rampdiagnos_item1c2_t1)
table(covid_lifetimewithout$rampdiagnos_item1c3_t1)
table(covid_lifetimewithout$rampdiagnos_item1c4_t1)
table(covid_lifetimewithout$rampdiagnos_item1c5_t1)
#panic disorder
table(covid_lifetimewithout$rampdiagnos_item1c6_t1)
table(covid_lifetimewithout$rampdiagnos_item1c7_t1)
#ptsd
table(covid_lifetimewithout$rampdiagnos_item1c8_t1)
#depression
table(covid_lifetimewithout$rampdiagnos_item1c9_t1)
#depression
table(covid_lifetimewithout$rampdiagnos_item1c10_t1)
table(covid_lifetimewithout$rampdiagnos_item1c11_t1)
#bipolar
table(covid_lifetimewithout$rampdiagnos_item1c12_t1)
table(covid_lifetimewithout$rampdiagnos_item1c13_t1)
#eating disorder
table(covid_lifetimewithout$rampdiagnos_item1c14_t1)
#eating disorder
table(covid_lifetimewithout$rampdiagnos_item1c15_t1)
table(covid_lifetimewithout$rampdiagnos_item1c16_t1)
table(covid_lifetimewithout$rampdiagnos_item1c17_t1)
#neuro
table(covid_lifetimewithout$rampdiagnos_item1c18_t1)
table(covid_lifetimewithout$rampdiagnos_item1c19_t1)
table(covid_lifetimewithout$rampdiagnos_item1c20_t1)
#bipolar
table(covid_lifetimewithout$rampdiagnos_item1c21_t1)

table(covid_lifetimewithout$tafitem10)
table(covid_lifetimewithout$tafitem11)

table(covid_lifetimewithout$tafitem13)
table(covid_lifetimewithout$tafitem14)


### Demo/desc recent without sample ###

covid_recentwithout <- covid_main %>%
  filter(tafitem11 == 1)

## table 1
table(covid_recentwithout$education)
prop.table(table(covid_recentwithout$education))
table(covid_recentwithout$sex)
prop.table(table(covid_recentwithout$sex))
describe(covid_recentwithout$age)

table(covid_recentwithout$cpfcorigin_item1_t1)

#fulltime
table(covid_recentwithout$rampemploy_item1c1_t1)
#parttime
table(covid_recentwithout$rampemploy_item1c2_t1)
#unemployed
table(covid_recentwithout$rampemploy_item1c3_t1)
#hourly
table(covid_recentwithout$rampemploy_item1c4_t1)
table(covid_recentwithout$rampemploy_item1c5_t1)
table(covid_recentwithout$rampemploy_item1c6_t1)
table(covid_recentwithout$rampemploy_item1c7_t1)
table(covid_recentwithout$rampemploy_item1c8_t1)
table(covid_recentwithout$rampemploy_item1c9_t1)
#retired
table(covid_recentwithout$rampemploy_item1c10_t1)
#student
table(covid_recentwithout$rampemploy_item1c11_t1)
table(covid_recentwithout$rampemploy_item1c12_t1)

#gad
table(covid_recentwithout$rampdiagnos_item1c1_t1)
#social anxiety
table(covid_recentwithout$rampdiagnos_item1c2_t1)
table(covid_recentwithout$rampdiagnos_item1c3_t1)
table(covid_recentwithout$rampdiagnos_item1c4_t1)
table(covid_recentwithout$rampdiagnos_item1c5_t1)
#panic disorder
table(covid_recentwithout$rampdiagnos_item1c6_t1)
table(covid_recentwithout$rampdiagnos_item1c7_t1)
#ptsd
table(covid_recentwithout$rampdiagnos_item1c8_t1)
#depression
table(covid_recentwithout$rampdiagnos_item1c9_t1)
#depression
table(covid_recentwithout$rampdiagnos_item1c10_t1)
table(covid_recentwithout$rampdiagnos_item1c11_t1)
#bipolar
table(covid_recentwithout$rampdiagnos_item1c12_t1)
table(covid_recentwithout$rampdiagnos_item1c13_t1)
#eating disorder
table(covid_recentwithout$rampdiagnos_item1c14_t1)
#eating disorder
table(covid_recentwithout$rampdiagnos_item1c15_t1)
table(covid_recentwithout$rampdiagnos_item1c16_t1)
table(covid_recentwithout$rampdiagnos_item1c17_t1)
#neuro
table(covid_recentwithout$rampdiagnos_item1c18_t1)
table(covid_recentwithout$rampdiagnos_item1c19_t1)
table(covid_recentwithout$rampdiagnos_item1c20_t1)
#bipolar
table(covid_recentwithout$rampdiagnos_item1c21_t1)

table(covid_recentwithout$tafitem10)
table(covid_recentwithout$tafitem11)

table(covid_recentwithout$tafitem13)
table(covid_recentwithout$tafitem14)


### Demo/desc lifetime with sample ###

covid_lifetimewith <- covid_main %>%
  filter(tafitem13 == 1)

## table 1
table(covid_lifetimewith$education)
prop.table(table(covid_lifetimewith$education))
table(covid_lifetimewith$sex)
prop.table(table(covid_lifetimewith$sex))
describe(covid_lifetimewith$age)

table(covid_lifetimewith$cpfcorigin_item1_t1)

#fulltime
table(covid_lifetimewith$rampemploy_item1c1_t1)
#parttime
table(covid_lifetimewith$rampemploy_item1c2_t1)
#unemployed
table(covid_lifetimewith$rampemploy_item1c3_t1)
#hourly
table(covid_lifetimewith$rampemploy_item1c4_t1)
table(covid_lifetimewith$rampemploy_item1c5_t1)
table(covid_lifetimewith$rampemploy_item1c6_t1)
table(covid_lifetimewith$rampemploy_item1c7_t1)
table(covid_lifetimewith$rampemploy_item1c8_t1)
table(covid_lifetimewith$rampemploy_item1c9_t1)
#retired
table(covid_lifetimewith$rampemploy_item1c10_t1)
#student
table(covid_lifetimewith$rampemploy_item1c11_t1)
table(covid_lifetimewith$rampemploy_item1c12_t1)

#gad
table(covid_lifetimewith$rampdiagnos_item1c1_t1)
#social anxiety
table(covid_lifetimewith$rampdiagnos_item1c2_t1)
table(covid_lifetimewith$rampdiagnos_item1c3_t1)
table(covid_lifetimewith$rampdiagnos_item1c4_t1)
table(covid_lifetimewith$rampdiagnos_item1c5_t1)
#panic disorder
table(covid_lifetimewith$rampdiagnos_item1c6_t1)
table(covid_lifetimewith$rampdiagnos_item1c7_t1)
#ptsd
table(covid_lifetimewith$rampdiagnos_item1c8_t1)
#depression
table(covid_lifetimewith$rampdiagnos_item1c9_t1)
#depression
table(covid_lifetimewith$rampdiagnos_item1c10_t1)
table(covid_lifetimewith$rampdiagnos_item1c11_t1)
#bipolar
table(covid_lifetimewith$rampdiagnos_item1c12_t1)
table(covid_lifetimewith$rampdiagnos_item1c13_t1)
#eating disorder
table(covid_lifetimewith$rampdiagnos_item1c14_t1)
#eating disorder
table(covid_lifetimewith$rampdiagnos_item1c15_t1)
table(covid_lifetimewith$rampdiagnos_item1c16_t1)
table(covid_lifetimewith$rampdiagnos_item1c17_t1)
#neuro
table(covid_lifetimewith$rampdiagnos_item1c18_t1)
table(covid_lifetimewith$rampdiagnos_item1c19_t1)
table(covid_lifetimewith$rampdiagnos_item1c20_t1)
#bipolar
table(covid_lifetimewith$rampdiagnos_item1c21_t1)

table(covid_lifetimewith$tafitem10)
table(covid_lifetimewith$tafitem11)

table(covid_lifetimewith$tafitem13)
table(covid_lifetimewith$tafitem14)


### Demo/desc recent with sample ###

covid_recentwith <- covid_main %>%
  filter(tafitem14 == 1)

## table 1
table(covid_recentwith$education)
prop.table(table(covid_recentwith$education))
table(covid_recentwith$sex)
prop.table(table(covid_recentwith$sex))
describe(covid_recentwith$age)

table(covid_recentwith$cpfcorigin_item1_t1)

#fulltime
table(covid_recentwith$rampemploy_item1c1_t1)
#parttime
table(covid_recentwith$rampemploy_item1c2_t1)
#unemployed
table(covid_recentwith$rampemploy_item1c3_t1)
#hourly
table(covid_recentwith$rampemploy_item1c4_t1)
table(covid_recentwith$rampemploy_item1c5_t1)
table(covid_recentwith$rampemploy_item1c6_t1)
table(covid_recentwith$rampemploy_item1c7_t1)
table(covid_recentwith$rampemploy_item1c8_t1)
table(covid_recentwith$rampemploy_item1c9_t1)
#retired
table(covid_recentwith$rampemploy_item1c10_t1)
#student
table(covid_recentwith$rampemploy_item1c11_t1)
table(covid_recentwith$rampemploy_item1c12_t1)

#gad
table(covid_recentwith$rampdiagnos_item1c1_t1)
#social anxiety
table(covid_recentwith$rampdiagnos_item1c2_t1)
table(covid_recentwith$rampdiagnos_item1c3_t1)
table(covid_recentwith$rampdiagnos_item1c4_t1)
table(covid_recentwith$rampdiagnos_item1c5_t1)
#panic disorder
table(covid_recentwith$rampdiagnos_item1c6_t1)
table(covid_recentwith$rampdiagnos_item1c7_t1)
#ptsd
table(covid_recentwith$rampdiagnos_item1c8_t1)
#depression
table(covid_recentwith$rampdiagnos_item1c9_t1)
#depression
table(covid_recentwith$rampdiagnos_item1c10_t1)
table(covid_recentwith$rampdiagnos_item1c11_t1)
#bipolar
table(covid_recentwith$rampdiagnos_item1c12_t1)
table(covid_recentwith$rampdiagnos_item1c13_t1)
#eating disorder
table(covid_recentwith$rampdiagnos_item1c14_t1)
#eating disorder
table(covid_recentwith$rampdiagnos_item1c15_t1)
table(covid_recentwith$rampdiagnos_item1c16_t1)
table(covid_recentwith$rampdiagnos_item1c17_t1)
#neuro
table(covid_recentwith$rampdiagnos_item1c18_t1)
table(covid_recentwith$rampdiagnos_item1c19_t1)
table(covid_recentwith$rampdiagnos_item1c20_t1)
#bipolar
table(covid_recentwith$rampdiagnos_item1c21_t1)

table(covid_recentwith$tafitem10)
table(covid_recentwith$tafitem11)

table(covid_recentwith$tafitem13)
table(covid_recentwith$tafitem14)



### Demo/desc with suicidal intent sample ###

covid_with <- covid_main %>%
  filter(tafitem13 == 1)

## table S1
describe(covid_with$age)
describe(covid_with$phq9_tot_t1)
describe(covid_with$gad7_tot_t1)

table(covid_with$si_tot_t1)
prop.table(table(covid_with$si_tot_t1))
table(covid_with$education)
prop.table(table(covid_with$education))
table(covid_with$sex)
prop.table(table(covid_with$sex))

table(covid_with$cpfcorigin_item1_t1)
prop.table(table(covid_with$cpfcorigin_item1_t1))

#fulltime
table(covid_with$rampemploy_item1c1_t1)
#unemployed
table(covid_with$rampemploy_item1c3_t1)
#hourly
table(covid_with$rampemploy_item1c4_t1)
table(covid_with$rampemploy_item1c5_t1)
table(covid_with$rampemploy_item1c6_t1)
table(covid_with$rampemploy_item1c7_t1)
table(covid_with$rampemploy_item1c8_t1)
table(covid_with$rampemploy_item1c9_t1)
#parttime
table(covid_with$rampemploy_item1c2_t1)
#retired
table(covid_with$rampemploy_item1c10_t1)
#student
table(covid_with$rampemploy_item1c11_t1)
table(covid_with$rampemploy_item1c12_t1)

#gad
table(covid_with$rampdiagnos_item1c1_t1)
#social anxiety
table(covid_with$rampdiagnos_item1c2_t1)
table(covid_with$rampdiagnos_item1c3_t1)
table(covid_with$rampdiagnos_item1c4_t1)
table(covid_with$rampdiagnos_item1c5_t1)
#panic disorder
table(covid_with$rampdiagnos_item1c6_t1)
table(covid_with$rampdiagnos_item1c7_t1)
#ptsd
table(covid_with$rampdiagnos_item1c8_t1)
#depression
table(covid_with$rampdiagnos_item1c9_t1)
#depression
table(covid_with$rampdiagnos_item1c10_t1)
table(covid_with$rampdiagnos_item1c11_t1)
#bipolar
table(covid_with$rampdiagnos_item1c12_t1)
table(covid_with$rampdiagnos_item1c13_t1)
#bipolar
table(covid_with$rampdiagnos_item1c21_t1)
#eating disorder
table(covid_with$rampdiagnos_item1c14_t1)
#eating disorder
table(covid_with$rampdiagnos_item1c15_t1)
table(covid_with$rampdiagnos_item1c16_t1)
table(covid_with$rampdiagnos_item1c17_t1)
#neuro
table(covid_with$rampdiagnos_item1c18_t1)
table(covid_with$rampdiagnos_item1c19_t1)
table(covid_with$rampdiagnos_item1c20_t1)


table(covid_with$tafitem10)
prop.table(table(covid_with$tafitem10))
table(covid_with$tafitem11)
prop.table(table(covid_with$tafitem11))

table(covid_with$tafitem13)
prop.table(table(covid_with$tafitem13))
table(covid_with$tafitem14)
prop.table(table(covid_with$tafitem14))


table(covid_with$si_tot_t1)
prop.table(table(covid_with$si_tot_t1))
table(covid_with$si_tot_t2)
prop.table(table(covid_with$si_tot_t2))
table(covid_with$si_tot_t3)
prop.table(table(covid_with$si_tot_t3))
table(covid_with$si_tot_t4)
prop.table(table(covid_with$si_tot_t4))
table(covid_with$si_tot_t5)
prop.table(table(covid_with$si_tot_t5))
table(covid_with$si_tot_t6)
prop.table(table(covid_with$si_tot_t6))
table(covid_with$si_tot_t7)
prop.table(table(covid_with$si_tot_t7))
table(covid_with$si_tot_t8)
prop.table(table(covid_with$si_tot_t8))
table(covid_with$si_tot_t9)
prop.table(table(covid_with$si_tot_t9))
table(covid_with$si_tot_t10)
prop.table(table(covid_with$si_tot_t10))
table(covid_with$si_tot_t11)
prop.table(table(covid_with$si_tot_t11))
table(covid_with$si_tot_t12)
prop.table(table(covid_with$si_tot_t12))
table(covid_with$si_tot_t13)
prop.table(table(covid_with$si_tot_t13))


### Demo/desc without suicidal intent sample ###

covid_without <- covid_main %>%
  filter(tafitem10 == 1)

## table S1
describe(covid_without$age)
describe(covid_without$phq9_tot_t1)
describe(covid_without$gad7_tot_t1)

table(covid_without$si_tot_t1)
prop.table(table(covid_without$si_tot_t1))
table(covid_without$education)
prop.table(table(covid_without$education))
table(covid_without$sex)
prop.table(table(covid_without$sex))

table(covid_without$cpfcorigin_item1_t1)
prop.table(table(covid_without$cpfcorigin_item1_t1))

#fulltime
table(covid_without$rampemploy_item1c1_t1)
#unemployed
table(covid_without$rampemploy_item1c3_t1)
#hourly
table(covid_without$rampemploy_item1c4_t1)
table(covid_without$rampemploy_item1c5_t1)
table(covid_without$rampemploy_item1c6_t1)
table(covid_without$rampemploy_item1c7_t1)
table(covid_without$rampemploy_item1c8_t1)
table(covid_without$rampemploy_item1c9_t1)
#parttime
table(covid_without$rampemploy_item1c2_t1)
#retired
table(covid_without$rampemploy_item1c10_t1)
#student
table(covid_without$rampemploy_item1c11_t1)
table(covid_without$rampemploy_item1c12_t1)

#gad
table(covid_without$rampdiagnos_item1c1_t1)
#social anxiety
table(covid_without$rampdiagnos_item1c2_t1)
table(covid_without$rampdiagnos_item1c3_t1)
table(covid_without$rampdiagnos_item1c4_t1)
table(covid_without$rampdiagnos_item1c5_t1)
#panic disorder
table(covid_without$rampdiagnos_item1c6_t1)
table(covid_without$rampdiagnos_item1c7_t1)
#ptsd
table(covid_without$rampdiagnos_item1c8_t1)
#depression
table(covid_without$rampdiagnos_item1c9_t1)
#depression
table(covid_without$rampdiagnos_item1c10_t1)
table(covid_without$rampdiagnos_item1c11_t1)
#bipolar
table(covid_without$rampdiagnos_item1c12_t1)
table(covid_without$rampdiagnos_item1c13_t1)
#bipolar
table(covid_without$rampdiagnos_item1c21_t1)
#eating disorder
table(covid_without$rampdiagnos_item1c14_t1)
#eating disorder
table(covid_without$rampdiagnos_item1c15_t1)
table(covid_without$rampdiagnos_item1c16_t1)
table(covid_without$rampdiagnos_item1c17_t1)
#neuro
table(covid_without$rampdiagnos_item1c18_t1)
table(covid_without$rampdiagnos_item1c19_t1)
table(covid_without$rampdiagnos_item1c20_t1)


table(covid_without$tafitem10)
prop.table(table(covid_without$tafitem10))
table(covid_without$tafitem11)
prop.table(table(covid_without$tafitem11))

table(covid_without$tafitem13)
prop.table(table(covid_without$tafitem13))
table(covid_without$tafitem14)
prop.table(table(covid_without$tafitem14))


table(covid_without$si_tot_t1)
prop.table(table(covid_without$si_tot_t1))
table(covid_without$si_tot_t2)
prop.table(table(covid_without$si_tot_t2))
table(covid_without$si_tot_t3)
prop.table(table(covid_without$si_tot_t3))
table(covid_without$si_tot_t4)
prop.table(table(covid_without$si_tot_t4))
table(covid_without$si_tot_t5)
prop.table(table(covid_without$si_tot_t5))
table(covid_without$si_tot_t6)
prop.table(table(covid_without$si_tot_t6))
table(covid_without$si_tot_t7)
prop.table(table(covid_without$si_tot_t7))
table(covid_without$si_tot_t8)
prop.table(table(covid_without$si_tot_t8))
table(covid_without$si_tot_t9)
prop.table(table(covid_without$si_tot_t9))
table(covid_without$si_tot_t10)
prop.table(table(covid_without$si_tot_t10))
table(covid_without$si_tot_t11)
prop.table(table(covid_without$si_tot_t11))
table(covid_without$si_tot_t12)
prop.table(table(covid_without$si_tot_t12))
table(covid_without$si_tot_t13)
prop.table(table(covid_without$si_tot_t13))

### Demo/desc no SI sample ###

covid_nosi <- covid_main %>%
  filter(tafitem10 == 0 & tafitem11 == 0 & tafitem13 == 0 & tafitem14 == 0)

## table S1
describe(covid_nosi$age)
describe(covid_nosi$phq9_tot_t1)
describe(covid_nosi$gad7_tot_t1)

table(covid_nosi$si_tot_t1)
prop.table(table(covid_nosi$si_tot_t1))
table(covid_nosi$education)
prop.table(table(covid_nosi$education))
table(covid_nosi$sex)
prop.table(table(covid_nosi$sex))

table(covid_nosi$cpfcorigin_item1_t1)
prop.table(table(covid_nosi$cpfcorigin_item1_t1))

#fulltime
table(covid_nosi$rampemploy_item1c1_t1)
#unemployed
table(covid_nosi$rampemploy_item1c3_t1)
#hourly
table(covid_nosi$rampemploy_item1c4_t1)
table(covid_nosi$rampemploy_item1c5_t1)
table(covid_nosi$rampemploy_item1c6_t1)
table(covid_nosi$rampemploy_item1c7_t1)
table(covid_nosi$rampemploy_item1c8_t1)
table(covid_nosi$rampemploy_item1c9_t1)
#parttime
table(covid_nosi$rampemploy_item1c2_t1)
#retired
table(covid_nosi$rampemploy_item1c10_t1)
#student
table(covid_nosi$rampemploy_item1c11_t1)
table(covid_nosi$rampemploy_item1c12_t1)

#gad
table(covid_nosi$rampdiagnos_item1c1_t1)
#social anxiety
table(covid_nosi$rampdiagnos_item1c2_t1)
table(covid_nosi$rampdiagnos_item1c3_t1)
table(covid_nosi$rampdiagnos_item1c4_t1)
table(covid_nosi$rampdiagnos_item1c5_t1)
#panic disorder
table(covid_nosi$rampdiagnos_item1c6_t1)
table(covid_nosi$rampdiagnos_item1c7_t1)
#ptsd
table(covid_nosi$rampdiagnos_item1c8_t1)
#depression
table(covid_nosi$rampdiagnos_item1c9_t1)
#depression
table(covid_nosi$rampdiagnos_item1c10_t1)
table(covid_nosi$rampdiagnos_item1c11_t1)
#bipolar
table(covid_nosi$rampdiagnos_item1c12_t1)
table(covid_nosi$rampdiagnos_item1c13_t1)
#bipolar
table(covid_nosi$rampdiagnos_item1c21_t1)
#eating disorder
table(covid_nosi$rampdiagnos_item1c14_t1)
#eating disorder
table(covid_nosi$rampdiagnos_item1c15_t1)
table(covid_nosi$rampdiagnos_item1c16_t1)
table(covid_nosi$rampdiagnos_item1c17_t1)
#neuro
table(covid_nosi$rampdiagnos_item1c18_t1)
table(covid_nosi$rampdiagnos_item1c19_t1)
table(covid_nosi$rampdiagnos_item1c20_t1)


table(covid_nosi$si_tot_t1)
prop.table(table(covid_nosi$si_tot_t1))
table(covid_nosi$si_tot_t2)
prop.table(table(covid_nosi$si_tot_t2))
table(covid_nosi$si_tot_t3)
prop.table(table(covid_nosi$si_tot_t3))
table(covid_nosi$si_tot_t4)
prop.table(table(covid_nosi$si_tot_t4))
table(covid_nosi$si_tot_t5)
prop.table(table(covid_nosi$si_tot_t5))
table(covid_nosi$si_tot_t6)
prop.table(table(covid_nosi$si_tot_t6))
table(covid_nosi$si_tot_t7)
prop.table(table(covid_nosi$si_tot_t7))
table(covid_nosi$si_tot_t8)
prop.table(table(covid_nosi$si_tot_t8))
table(covid_nosi$si_tot_t9)
prop.table(table(covid_nosi$si_tot_t9))
table(covid_nosi$si_tot_t10)
prop.table(table(covid_nosi$si_tot_t10))
table(covid_nosi$si_tot_t11)
prop.table(table(covid_nosi$si_tot_t11))
table(covid_nosi$si_tot_t12)
prop.table(table(covid_nosi$si_tot_t12))
table(covid_nosi$si_tot_t13)
prop.table(table(covid_nosi$si_tot_t13))


#### INTERNAL CONSISTENCY ####

covid_main_cronbach <- covid_main %>%
  select(phq9_item1_t1, phq9_item2_t1, phq9_item3_t1, 
         phq9_item4_t1, phq9_item5_t1, phq9_item6_t1,
         phq9_item7_t1, phq9_item8_t1, phq9_item9_t1,
         gad7_item1_t1, gad7_item2_t1, gad7_item3_t1,
         gad7_item4_t1, gad7_item5_t1, gad7_item6_t1,
         gad7_item7_t1)

psych::alpha(covid_main_cronbach[,c(1:9)])$total$std.alpha
psych::alpha(covid_main_cronbach[,c(10:16)])$total$std.alpha



#### MAIN ANALYSIS ####

### Prepping for long format ###

#rename taf variable names to enable long format
covid_main <- covid_main %>%
  rename(tafitem10 = taf_item10_t1) %>%
  rename(tafitem11 = taf_item11_t1) %>%
  rename(tafitem13 = taf_item13_t1) %>%
  rename(tafitem14 = taf_item14_t1) %>%
  rename(cpfcconsenttimestamp_t1 = cpfcconsent_timestamp_t1) %>%
  rename(sex = sex_t1) %>%
  rename(age = age_t1) %>%
  rename(education = education_t1)

#make new variables in order and at end of df for suicidal ideation
covid_main <- covid_main %>%
  mutate(suic_tot_t1 = phq9_item9_t1) %>%
  mutate(suic_tot_t2 = phq9_item9_t2) %>%
  mutate(suic_tot_t3 = phq9_item9_t3) %>%
  mutate(suic_tot_t4 = phq9_item9_t4) %>%
  mutate(suic_tot_t5 = phq9_item9_t5) %>%
  mutate(suic_tot_t6 = phq9_item9_t6) %>%
  mutate(suic_tot_t7 = phq9_item9_t7) %>%
  mutate(suic_tot_t8 = phq9_item9_t8) %>%
  mutate(suic_tot_t9 = phq9_item9_t9) %>%
  mutate(suic_tot_t10 = phq9_item9_t10) %>%
  mutate(suic_tot_t11 = phq9_item9_t11) %>%
  mutate(suic_tot_t12 = phq9_item9_t12) %>%
  mutate(suic_tot_t13 = phq9_item9_t13)
  

#education and sex should be a factor
covid_main <- covid_main %>%
  mutate(across(sex, ~ case_when(. == 1 ~ 0,
                                 . == 2 ~ 1,
                                 . == 3 ~ 2,
                                 . == 4 ~ 2,
                                 . == 5 ~ 2)))

covid_main$sex <- factor(covid_main$sex, 
                             levels = c(0, 1, 2),
                             labels = c("Male", "Female", "Other"))

covid_main <- covid_main %>%
  mutate(across(education, ~ case_when(. == 1 ~ 0,
                                 . == 2 ~ 1,
                                 . == 3 ~ 2)))

covid_main$education <- factor(covid_main$education, 
                         levels = c(0, 1, 2),
                         labels = c("Compulsory", "Highschool", "University"))

#make new binary variables to denote clinical depression/anxiety at baseline
covid_main <- covid_main %>%
  mutate(severeanx = ifelse(gad7_tot_t1 >= 15, 1, 0))

covid_main <- covid_main %>%
  mutate(severedep = ifelse(phq9_tot_t1 >= 14, 1, 0))

#make a binary variable of phq-9 item so that 0 = 0 (no suicidal ideation) and >0 = 1 (suicidal ideation)
#to use for the suicidal ideation outcome in later analyses
covid_main <- covid_main %>%
  mutate(si_tot_t1 = ifelse(phq9_item9_t1 > 0, 1, 0)) %>%
  mutate(si_tot_t2 = ifelse(phq9_item9_t2 > 0, 1, 0)) %>%
  mutate(si_tot_t3 = ifelse(phq9_item9_t3 > 0, 1, 0)) %>%
  mutate(si_tot_t4 = ifelse(phq9_item9_t4 > 0, 1, 0)) %>%
  mutate(si_tot_t5 = ifelse(phq9_item9_t5 > 0, 1, 0)) %>%
  mutate(si_tot_t6 = ifelse(phq9_item9_t6 > 0, 1, 0)) %>%
  mutate(si_tot_t7 = ifelse(phq9_item9_t7 > 0, 1, 0)) %>%
  mutate(si_tot_t8 = ifelse(phq9_item9_t8 > 0, 1, 0)) %>%
  mutate(si_tot_t9 = ifelse(phq9_item9_t9 > 0, 1, 0)) %>%
  mutate(si_tot_t10 = ifelse(phq9_item9_t10 > 0, 1, 0)) %>%
  mutate(si_tot_t11 = ifelse(phq9_item9_t11 > 0, 1, 0)) %>%
  mutate(si_tot_t12 = ifelse(phq9_item9_t12 > 0, 1, 0)) %>%
  mutate(si_tot_t13 = ifelse(phq9_item9_t13 > 0, 1, 0))


#separate the self-harm variables so that there is no overlap between lifetime
#and recent, thus removing those who report recent from the lifetime group
#covid_main <- covid_main %>%
#  mutate(lifetimewithout = ifelse(tafitem10 == 1 & tafitem11 == 0, 1, 0))

#covid_main <- covid_main %>%
#  mutate(recentwithout = ifelse(tafitem11 == 1, 1, 0))

#covid_main <- covid_main %>%
#  mutate(lifetimewith = ifelse(tafitem13 == 1 & tafitem14 == 0, 1, 0))

#covid_main <- covid_main %>%
#  mutate(recentwith = ifelse(tafitem14 == 1, 1, 0))

#create two self-harm groups, lifetime+recent nonsuicidal (without) and 
#lifetime+recent suicidal (with)
#after reviewer comments and discussion 230602
covid_main <- covid_main %>%
  mutate(without = ifelse(tafitem10 == 1, 1, 0))

covid_main <- covid_main %>%
  mutate(with = ifelse(tafitem13 == 1, 1, 0))


#make a new variable that corresponds to "days since study started"
#study started at
#covid_main %>%
#  summarise(min = min(cpfcconsenttimestamp_t1))
#2020-07-02, this should be 0 and every date after that should be 1, 2, 3 etc.

covid_main <- covid_main %>%
  mutate(dayssincestart = cpfcconsenttimestamp_t1 - min(cpfcconsenttimestamp_t1))

covid_main$dayssincestart <- as.numeric(covid_main$dayssincestart)

#see mean of how many measurement points this sample participated in (with gad7 as example)
covid_main <- covid_main %>%
  group_by(subjectID) %>%
  mutate(participation = rowSums(is.na(across(gad7_tot_t1:gad7_tot_t13)))) %>%
  ungroup() %>%
  mutate(participation = 13 - participation)

describe(covid_main$participation)


#transform df to long format for growth curve analyses
covid_main_long <- covid_main %>%
  select(subjectID, age, education, sex, dayssincestart, severeanx,
         severedep, without, with,
         gad7_tot_t1:gad7_tot_t13, phq9_tot_t1:phq9_tot_t13, 
         suic_tot_t1:suic_tot_t13, si_tot_t1:si_tot_t13) %>%
  pivot_longer(cols = 10:61, names_to = c("variable", "tot", "time"), 
               names_sep = "_", values_to = "value") %>%
  select(-tot)

covid_main_long$time <- str_replace(covid_main_long$time, "t", "")

covid_main_long$time <- as.numeric(covid_main_long$time)

covid_main_long$time <- covid_main_long$time - 1


#### 1. Growth curve depression ####

## filter phq9 for all occasions into new df for depression growth curve
covid_main_longdep <- filter(covid_main_long, variable %in% c("phq9")) %>%
  spread(key = variable, value)

covid_main_longdep <- as.data.frame(covid_main_longdep)
write_rds(covid_main_longdep, "covid_main_longdep.rds")

# model 1 - unconditional model (only dep)
depmod1 <- lmerTest::lmer(phq9 ~ 1 + (1 | subjectID), data=covid_main_longdep, REML = TRUE, na.action = na.exclude, control=lmerControl(optimizer = "bobyqa"))
summary(depmod1)

# model 2 - unconditional growth model (dep and time)
depmod2 <- lmerTest::lmer(phq9 ~ time + (time | subjectID), data=covid_main_longdep, REML = TRUE, na.action = na.exclude, control=lmerControl(optimizer = "bobyqa"))
summary(depmod2)
stats::confint(depmod2)
conf_int(depmod2, vcov = "CR2")
#robust results look the same

# model 5 - same as above but added days since start + all other covariates
depmod5 <- lmerTest::lmer(phq9 ~ time + dayssincestart + age + sex + education + (time | subjectID), data=covid_main_longdep, REML = TRUE, na.action = na.exclude, control=lmerControl(optimizer = "bobyqa"))
summary(depmod5)
stats::confint(depmod5)
conf_int(depmod5, vcov = "CR2")
#robust results look the same

#a model with only covariates as a baseline for effect size calculation
depmod15 <- lmerTest::lmer(phq9 ~ dayssincestart + age + sex + education + ( 1 | subjectID), data=covid_main_longdep, REML = TRUE, na.action = na.exclude, control=lmerControl(optimizer = "bobyqa"))

## effect size time (pseudo r2)
#residual variance, without predictor
print(VarCorr(depmod15), comp = c("Variance"))
#residual variance, with predictor
print(VarCorr(depmod5), comp = c("Variance"))

(14.83 - 12.50)/14.83
#0.16, 16%


#### 2. Growth curve anxiety ####

## filter gad7 for all occasions into new df for anxiety growth curve
covid_main_longanx <- filter(covid_main_long, variable %in% c("gad7")) %>%
  spread(key = variable, value)

covid_main_longanx <- as.data.frame(covid_main_longanx)
write_rds(covid_main_longanx, "covid_main_longanx.rds")


# model 1 - unconditional model (only anx)
anxmod1 <- lmerTest::lmer(gad7 ~ 1 + (1 | subjectID), data=covid_main_longanx, REML = TRUE, na.action = na.exclude, control=lmerControl(optimizer = "bobyqa"))
summary(anxmod1)

# model 2 - unconditional growth model (anx and time)
anxmod2 <- lmerTest::lmer(gad7 ~ time + (time | subjectID), data=covid_main_longanx, REML = TRUE, na.action = na.exclude, control=lmerControl(optimizer = "bobyqa"))
summary(anxmod2)
stats::confint(anxmod2)
conf_int(anxmod2, vcov = "CR2")
#robust results look the same

# model 5 - same as above but added days since start + all other covariates
anxmod5 <- lmerTest::lmer(gad7 ~ time + dayssincestart + age + sex + education + (time | subjectID), data=covid_main_longanx, REML = TRUE, na.action = na.exclude, control=lmerControl(optimizer = "bobyqa"))
summary(anxmod5)
stats::confint(anxmod5)
conf_int(anxmod5, vcov = "CR2")
#robust results look the same

#a model with only covariates as a baseline for effect size calculation
anxmod15 <- lmerTest::lmer(gad7 ~ dayssincestart + age + sex + education + ( 1 | subjectID), data=covid_main_longanx, REML = TRUE, na.action = na.exclude, control=lmerControl(optimizer = "bobyqa"))

## effect size time (pseudo r2)
#residual variance, without predictor
print(VarCorr(anxmod15), comp = c("Variance"))
#residual variance, with predictor
print(VarCorr(anxmod5), comp = c("Variance"))

(11.25 - 9.47)/11.25
#0.16, 16%


#### 3. Growth curve suicidality ####

## filter si for all occasions into new df for suicidality growth curve
covid_main_longsuic <- filter(covid_main_long, variable %in% c("si")) %>%
  spread(key = variable, value)

covid_main_longsuic <- as.data.frame(covid_main_longsuic)
write_rds(covid_main_longsuic, "covid_main_longsuic.rds")

# model 1 - unconditional model (only suic)
suicmod1 <- glmer(si ~ 1 + (1 | subjectID), data=covid_main_longsuic, family = "binomial", na.action = na.exclude, control = glmerControl(optimizer = "bobyqa"))
summary(suicmod1)

# model 2 - unconditional growth model (suic and time)
suicmod2 <- glmer(si ~ time + (time | subjectID), data=covid_main_longsuic, family = "binomial", na.action = na.exclude, control = glmerControl(optimizer = "bobyqa"))
summary(suicmod2)


### 4. Growth curve self-harm without suicidal intent ####

# in depression growth curve
#THESE HAVE BEEN CHANGED TO INCLUDE BOTH RECENT+LIFETIME 230602
#lifetime
depmod11 <- lmerTest::lmer(phq9 ~ time + without + time*without + (time | subjectID), data=covid_main_longdep, REML = TRUE, na.action = na.exclude, control=lmerControl(optimizer = "bobyqa"))
summary(depmod11)
stats::confint(depmod11)
conf_int(depmod11, vcov = "CR2")
#robust results look the same

depmod7 <- lmerTest::lmer(phq9 ~ time + dayssincestart + age + sex + education + without + time*without + (time | subjectID), data=covid_main_longdep, REML = TRUE, na.action = na.exclude, control=lmerControl(optimizer = "bobyqa"))
summary(depmod7)
stats::confint(depmod7)
conf_int(depmod7, vcov = "CR2")
#robust results look the same


# in anxiety growth curve
#THESE HAVE BEEN CHANGED TO INCLUDE BOTH RECENT+LIFETIME 230602
#lifetime
anxmod11 <- lmerTest::lmer(gad7 ~ time + without + time*without + (time | subjectID), data=covid_main_longanx, REML = TRUE, na.action = na.exclude, control=lmerControl(optimizer = "bobyqa"))
summary(anxmod11)
stats::confint(anxmod11)
conf_int(anxmod11, vcov = "CR2")
#robust results look the same

anxmod7 <- lmerTest::lmer(gad7 ~ time + dayssincestart + age + sex + education + without + time*without + (time | subjectID), data=covid_main_longanx, REML = TRUE, na.action = na.exclude, control=lmerControl(optimizer = "bobyqa"))
summary(anxmod7)
stats::confint(anxmod7)
conf_int(anxmod7, vcov = "CR2")
#robust results look the same

# in suicidality growth curve
#lifetime
suicmod7 <- lmer(suic ~ time + dayssincestart + age + sex + education + lifetimewithout + time*lifetimewithout + (time | subjectID), data=covid_main_longsuic, REML = TRUE, na.action = na.exclude, control = lmerControl(optimizer = "bobyqa"))
summary(suicmod7)

#recent
suicmod8 <- lme(suic ~ time + dayssincestart + age + sex + education + recentwithout, random = ~ time | subjectID, data=covid_main_longsuic, method = "REML", na.action = na.exclude, control=lmeControl(opt = "optim"))
summary(suicmod8)


#### 5. Growth curve self-harm with suicidal intent ####

# in depression growth curve
#THESE HAVE BEEN CHANGED TO INCLUDE BOTH RECENT+LIFETIME 230602
#lifetime
depmod13 <- lmerTest::lmer(phq9 ~ time + with + time*with + (time | subjectID), data=covid_main_longdep, REML = TRUE, na.action = na.exclude, control=lmerControl(optimizer = "bobyqa"))
summary(depmod13)
stats::confint(depmod13)
conf_int(depmod13, vcov = "CR2")
#robust results look the same

depmod9 <- lmerTest::lmer(phq9 ~ time + dayssincestart + age + sex + education + with + time*with + (time | subjectID), data=covid_main_longdep, REML = TRUE, na.action = na.exclude, control=lmerControl(optimizer = "bobyqa"))
summary(depmod9)
stats::confint(depmod9)
conf_int(depmod9, vcov = "CR2")
#robust results look the same

#here testing adding dep and anx as covariates as a 
#posthoc sensitivity analysis
depmod_x <- lmerTest::lmer(phq9 ~ time + dayssincestart + age + sex + education + with + time*with + severedep + severeanx + (time | subjectID), data=covid_main_longdep, REML = TRUE, na.action = na.exclude, control=lmerControl(optimizer = "bobyqa"))
summary(depmod_x)
stats::confint(depmod_x)

# in anxiety growth curve
#THESE HAVE BEEN CHANGED TO INCLUDE BOTH RECENT+LIFETIME 230602
#lifetime
anxmod13 <- lmerTest::lmer(gad7 ~ time + with + time*with + (time | subjectID), data=covid_main_longanx, REML = TRUE, na.action = na.exclude, control=lmerControl(optimizer = "bobyqa"))
summary(anxmod13)
stats::confint(anxmod13)
conf_int(anxmod13, vcov = "CR2")

anxmod9 <- lmerTest::lmer(gad7 ~ time + dayssincestart + age + sex + education + with + time*with + (time | subjectID), data=covid_main_longanx, REML = TRUE, na.action = na.exclude, control=lmerControl(optimizer = "bobyqa"))
summary(anxmod9)
stats::confint(anxmod9)
conf_int(anxmod9, vcov = "CR2")

#here testing adding dep and anx as covariates as a 
#posthoc sensitivity analysis
anxmod_x <- lmerTest::lmer(gad7 ~ time + dayssincestart + age + sex + education + with + time*with + severedep + severeanx + (time | subjectID), data=covid_main_longanx, REML = TRUE, na.action = na.exclude, control=lmerControl(optimizer = "bobyqa"))
summary(anxmod_x)
stats::confint(anxmod_x)


# in suicidality growth curve
#lifetime
suicmod9 <- lme(suic ~ time + dayssincestart + age + sex + education + lifetimewith, random = ~ time | subjectID, data=covid_main_longsuic, method = "REML", na.action = na.exclude, control=lmeControl(opt = "optim"))
summary(suicmod9)

#recent
suicmod10 <- lme(suic ~ time + dayssincestart + age + sex + education + recentwith, random = ~ time | subjectID, data=covid_main_longsuic, method = "REML", na.action = na.exclude, control=lmeControl(opt = "optim"))
summary(suicmod10)


#### VISUALIZATION ####

## plots for growth curve analyses (predicted values, fixed effects)

set_theme(theme_test())

#dep trajectory adjusted
dep_adj <- plot_model(
  depmod5,
  type = "pred",
  terms = c("time"),
  ci.lvl = 0.95) +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  scale_y_continuous(limits = c(10, 18), breaks = seq(10, 18, by = 1)) +
  ggtitle("") +
  theme(legend.position = "bottom") +
  xlab("Time") +
  ylab("PHQ-9 total score")

#dep trajectory unadjusted
dep_unadj <- plot_model(
  depmod2,
  type = "pred",
  terms = c("time"),
  ci.lvl = 0.95) +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  scale_y_continuous(limits = c(10, 18), breaks = seq(10, 18, by = 1)) +
  ggtitle("") +
  theme(legend.position = "bottom") +
  xlab("Time") +
  ylab("PHQ-9 total score")


#anx trajectory adjusted
anx_adj <- plot_model(
  anxmod5,
  type = "pred",
  terms = c("time"),
  ci.lvl = 0.95) +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  scale_y_continuous(limits = c(8, 14), breaks = seq(8, 14, by = 1)) +
  ggtitle("") +
  theme(legend.position = "bottom") +
  xlab("Time") +
  ylab("GAD-7 total score")

#anx trajectory unadjusted
anx_unadj <- plot_model(
  anxmod2,
  type = "pred",
  terms = c("time"),
  ci.lvl = 0.95) +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  scale_y_continuous(limits = c(8, 14), breaks = seq(8, 14, by = 1)) +
  ggtitle("") +
  theme(legend.position = "bottom") +
  xlab("Time") +
  ylab("GAD-7 total score")


#put plots together for supplementary

depplot <- ggarrange(dep_adj, dep_unadj, labels=c("a)", "b)"), align = c("hv"), common.legend = FALSE, nrow = 2)

anxplot <- ggarrange(anx_adj, anx_unadj, labels=c("a)", "b)"), align = c("hv"), common.legend = FALSE)

generaltraj <- ggarrange(dep_adj, dep_unadj, anx_adj, anx_unadj, labels=c("a)", "b)", "c)", "d)"), align = c("hv"), common.legend = FALSE)


#make manual choice of viridis palette colors

viridis_pal <- c("#440154FF", "#55C667FF")

#dep trajectory by self-harm without intent
depwithoutplot <- plot_model(
  depmod7,
  type = "pred",
  terms = c("time", "without"),
  ci.lvl = 0.95) +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  scale_y_continuous(limits = c(10, 19), breaks = seq(10, 19, by = 1)) +
  ggtitle("") +
  scale_fill_manual(values = viridis_pal) +
  scale_color_manual(values = viridis_pal, labels = c("No", "Yes"), name = "Nonsuicidal \n self-injury") +
  theme(legend.position = "bottom") +
  xlab("Time") +
  ylab("PHQ-9 total score")

#dep trajectory by self-harm with intent
depwithplot <- plot_model(
  depmod9,
  type = "pred",
  terms = c("time", "with"),
  ci.lvl = 0.95) +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  scale_y_continuous(limits = c(10, 19), breaks = seq(10, 19, by = 1)) +
  ggtitle("") +
  scale_fill_manual(values = viridis_pal) +
  scale_color_manual(values = viridis_pal, labels = c("No", "Yes"), name = "Suicidal \n self-injury") +
  theme(legend.position = "bottom") +
  xlab("Time") +
  ylab("PHQ-9 total score")

#anx trajectory by self-harm without intent
anxwithoutplot <- plot_model(
  anxmod7,
  type = "pred",
  terms = c("time", "without"),
  ci.lvl = 0.95) +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  scale_y_continuous(limits = c(7, 15), breaks = seq(7, 15, by = 1)) +
  ggtitle("") +
  scale_fill_manual(values = viridis_pal) +
  scale_color_manual(values = viridis_pal, labels = c("No", "Yes"), name = "Nonsuicidal \n self-injury") +
  theme(legend.position = "bottom") +
  xlab("Time") +
  ylab("GAD-7 total score")

#anx trajectory by self-harm with intent
anxwithplot <- plot_model(
  anxmod9,
  type = "pred",
  terms = c("time", "with"),
  ci.lvl = 0.95) +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
  scale_y_continuous(limits = c(7, 15), breaks = seq(7, 15, by = 1)) +
  ggtitle("") +
  scale_fill_manual(values = viridis_pal) +
  scale_color_manual(values = viridis_pal, labels = c("No", "Yes"), name = "Suicidal \n self-injury") +
  theme(legend.position = "bottom") +
  xlab("Time") +
  ylab("GAD-7 total score")

## put plots together for manuscript
withwithoutplot <- ggarrange(depwithplot, anxwithplot, depwithoutplot, anxwithoutplot, labels=c("a)", "b)", "c)", "d)"), align = c("hv"), common.legend = FALSE)


#### POST HOC ANALYSES ####
### Significance test differences with/without FU ###
## after reviewer feedback (August 2024), we conduct t-tests of differences
## in means for Table S2
## Z test for proportions

# x = study sample (1810), y = excluded participants (2308)

#age
tsum.test(mean.x=38.07,   s.x=12.69, n.x=1810,
          mean.y=34.29, s.y=11.74, n.y=2308)

#phq9 tot score
tsum.test(mean.x=14.57,   s.x=6.61, n.x=1810,
          mean.y=15.76, s.y=6.62, n.y=2308)

#gad7 tot score
tsum.test(mean.x=11.50,   s.x=5.72, n.x=1810,
          mean.y=12.24, s.y=5.70, n.y=2308)

#self injury thoughts
# X
p1 <- 940  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 1302  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#lifetime without
# X
p1 <- 1013  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 1369  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#recent without
# X
p1 <- 254  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 374  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#lifetime with
# X
p1 <- 528  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 686  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#recent with
# X
p1 <- 44  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 56  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#gender male
# X
p1 <- 353  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 567  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#gender female
# X
p1 <- 1357  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 1629  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#gender other
# X
p1 <- 100  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 112  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))



#birthplace sweden
# X
p1 <- 1691  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 2142  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#education elementary school
# X
p1 <- 98  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 191  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#education high school
# X
p1 <- 577  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 1002  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#education university
# X
p1 <- 1135  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 1115  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#employment status student
# X
p1 <- 398  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 600  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#employment status unemployed
# X
p1 <- 179  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 286  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#employment status part time
# X
p1 <- 433  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 592  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#employment status full time
# X
p1 <- 722  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 819  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#employment status retired
# X
p1 <- 175  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 156  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#psychiatric disorder bipolar
# X
p1 <- 351  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 423  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#psychiatric disorder depression
# X
p1 <- 1480  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 1865  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#psychiatric disorder social anxiety
# X
p1 <- 336  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 447  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#psychiatric disorder panic disorder
# X
p1 <- 677  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 1046  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#psychiatric disorder generalized anxiety
# X
p1 <- 770  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 1104  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#psychiatric disorder trauma
# X
p1 <- 377  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 484  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#psychiatric disorder eating disorder
# X
p1 <- 407  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 478  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))


#psychiatric disorder neurodevelopmental
# X
p1 <- 381  # Cases
n1 <- 1810 # Total sample
# Y
p2 <- 583  # Cases
n2 <- 2308 # Total sample

# Is the proportion of X equal to the proportion of Y?
prop.test(c(p1, p2), n = c(n1, n2))
