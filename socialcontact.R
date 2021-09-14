rm(list=ls())
library(haven)
library(tidyr)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(survey)
library(twosamples)


# work desktop imports
#data <- file.path("/Users/knbratt/OneDrive - Emory University/COVIDVu/Social Contact Data Analysis/data") 

#laptop imports
data <- file.path("/Users/kristinnelson/OneDrive - Emory University/COVIDVu/Social Contact Data Analysis/data") 

# baseline
bline1 <- read_sas(file.path(data,"analysis_nat_surveyig.sas7bdat")) # cleaned baseline variables (age, gender, race/eth, education, income, totalIg, weights)
bline2 <- read_sas(file.path(data,"baseline_nat_surveyig.sas7bdat")) # raw baseline variables (social distancing vars, occupation)
bline3 <- read_sas(file.path(data,"enumeration_nat_surveyig.sas7bdat")) # enumeration variables (HH size)

# 3-month survey
m3 <- read_sas(file.path(data,"analysis_nat_m3_abigg.sas7bdat")) # cleaned m3 variables (weights)
m3_2 <- read_sas(file.path(data,"month3_nat_surveyig.sas7bdat")) # raw m3 variables (social distancing vars)



# VARIABLES ('_I' notes imputed values for that variable)

# Variable of interest (dataset)
# varname

# Age (bline1)
# p_age

# Sex (bline1)
# gender_i

# State (bline1)
# state

# Income (bline1)
# income_i

# Race/ethnicity (bline1)
# race_ethnicity_i

# Occupation (bline2)
# job_type

# Baseline total Ig (bline1)
# dbs_result2

# Month 3 total Ig (m3)
# m3_TAb

# HH size (bline3)
# hh_num 

# Social distancing vars
# SOCIAL CONTACT (also 'nonphy' prefixes for all the following vars for nonphysical contacts), baseline and month 3 (bline2, m3)
#physical_none
#phy0_home
#phy0_work
#phy0_school
#phy0_other
#phy5_home
#phy5_work
#phy5_school
#phy5_other
#phy10_home
#phy10_work
#phy10_school
#phy10_other
#phy20_home
#phy20_work
#phy20_school
#phy20_other
#phy40_home
#phy40_work
#phy40_school
#phy40_other
#phy60_home
#phy60_work
#phy60_school
#phy60_other
#phy70_home
#phy70_work
#phy70_school
#phy70_other

#####
# Weight, baseline (bline1)
# WTFinal_US_Plus2_TotalIg
#####

#####
# Weight, month 3 (m3)
# WTFinal_US_M3_abigg
#####

###############################################
# Descriptive statistics and variable-checking
###############################################

# Age categories from bline1
table(bline1$AGE_I)

# Create an age variable that matches with age caategories in contact questions, to get contact matrices
  
bline2$agecat_contact <- cut(bline2$p_age, breaks = c(5,10,20,40,60,70,100), right = TRUE)
table(bline2$agecat_contact)

# Categorize HH size variable

bline3$hhnum_cat <- cut(bline3$hh_num, breaks = c(0, 1, 2, 3, 4, 100), right = TRUE)
table(bline3$hhnum_cat)

# Gender
table(bline1$GENDER_I)

# State
table(bline1$state)

# Income
table(bline1$INCOME_I)

# Race/ethnicity
table(bline1$RACE_ETHNICITY_I)

# Job type
table(bline2$job_type)

# Baseline total Ig
table(bline1$dbs_result2)

# Month 3 total Ig
table(m3$m3_TAb)

# HH size
table(bline3$hh_num)


###############################################
# Dataset merging for analysis 
###############################################

# Join bline1, bline2, bline3 
left_join(bline1, bline2, by='p_id') -> bline1_2
left_join(bline1_2, bline3, by='p_id') -> bline_all

# Select only desired variables for analysis

bline_all %>%
    select(p_id,
           
           state, 
           AGE_I, agecat_contact, GENDER_I, INCOME_I, RACE_ETHNICITY_I,
           
           phy0_home, phy0_work, phy0_school, phy0_other,
           phy5_home, phy5_work, phy5_school, phy5_other,
           phy10_home, phy10_work, phy10_school, phy10_other,
           phy20_home, phy20_work, phy20_school, phy20_other,
           phy40_home, phy40_work, phy40_school, phy40_other,
           phy60_home, phy60_work, phy60_school, phy60_other,
           phy70_home, phy70_work, phy70_school, phy70_other,
           
           nonphy0_home, nonphy0_work, nonphy0_school, nonphy0_other,
           nonphy5_home, nonphy5_work, nonphy5_school, nonphy5_other,
           nonphy10_home, nonphy10_work, nonphy10_school, nonphy10_other,
           nonphy20_home, nonphy20_work, nonphy20_school, nonphy20_other,
           nonphy40_home, nonphy40_work, nonphy40_school, nonphy40_other,
           nonphy60_home, nonphy60_work, nonphy60_school, nonphy60_other,
           nonphy70_home, nonphy70_work, nonphy70_school, nonphy70_other,
           
           job_type,
           dbs_result2, 
           hh_num,
           hhnum_cat,
           WTFinal_US_Plus2_TotalIg) -> bline_all

m3 %>%
  select(p_id, 
         m3_TAb,
         WTFinal_US_M3_AbIGg) -> m3

m3_2 %>%
    select(p_id,
                 
          phy0_home, phy0_work, phy0_school, phy0_other,
          phy5_home, phy5_work, phy5_school, phy5_other,
          phy10_home, phy10_work, phy10_school, phy10_other,
          phy20_home, phy20_work, phy20_school, phy20_other,
          phy40_home, phy40_work, phy40_school, phy40_other,
          phy60_home, phy60_work, phy60_school, phy60_other,
          phy70_home, phy70_work, phy70_school, phy70_other,
                  
          nonphy0_home, nonphy0_work, nonphy0_school, nonphy0_other,
          nonphy5_home, nonphy5_work, nonphy5_school, nonphy5_other,
          nonphy10_home, nonphy10_work, nonphy10_school, nonphy10_other,
          nonphy20_home, nonphy20_work, nonphy20_school, nonphy20_other,
          nonphy40_home, nonphy40_work, nonphy40_school, nonphy40_other,
          nonphy60_home, nonphy60_work, nonphy60_school, nonphy60_other,
          nonphy70_home, nonphy70_work, nonphy70_school, nonphy70_other) -> m3_2



# rename m3 contact variables or else they will be same as baseline in merge

m3_2 %>%
  dplyr::rename(phy0_home_3m = phy0_home,
         phy0_work_3m = phy0_work, 
         phy0_school_3m = phy0_school, 
         phy0_other_3m = phy0_other,
         phy5_home_3m = phy5_home,
         phy5_work_3m = phy5_work, 
         phy5_school_3m = phy5_school, 
         phy5_other_3m = phy5_other,
         phy10_home_3m = phy10_home, 
         phy10_work_3m = phy10_work, 
         phy10_school_3m = phy10_school, 
         phy10_other_3m = phy10_other,
         phy20_home_3m = phy20_home, 
         phy20_work_3m = phy20_work, 
         phy20_school_3m = phy20_school, 
         phy20_other_3m = phy20_other,
         phy40_home_3m = phy40_home, 
         phy40_work_3m = phy40_work, 
         phy40_school_3m = phy40_school, 
         phy40_other_3m = phy40_other,
         phy60_home_3m = phy60_home, 
         phy60_work_3m = phy60_work, 
         phy60_school_3m = phy60_school, 
         phy60_other_3m = phy60_other,
         phy70_home_3m = phy70_home, 
         phy70_work_3m = phy70_work, 
         phy70_school_3m = phy70_school, 
         phy70_other_3m = phy70_other,
         
         nonphy0_home_3m = nonphy0_home,
         nonphy0_work_3m = nonphy0_work, 
         nonphy0_school_3m = nonphy0_school, 
         nonphy0_other_3m = nonphy0_other,
         nonphy5_home_3m = nonphy5_home,
         nonphy5_work_3m = nonphy5_work, 
         nonphy5_school_3m = nonphy5_school, 
         nonphy5_other_3m = nonphy5_other,
         nonphy10_home_3m = nonphy10_home, 
         nonphy10_work_3m = nonphy10_work, 
         nonphy10_school_3m = nonphy10_school, 
         nonphy10_other_3m = nonphy10_other,
         nonphy20_home_3m = nonphy20_home, 
         nonphy20_work_3m = nonphy20_work, 
         nonphy20_school_3m = nonphy20_school, 
         nonphy20_other_3m = nonphy20_other,
         nonphy40_home_3m = nonphy40_home, 
         nonphy40_work_3m = nonphy40_work, 
         nonphy40_school_3m = nonphy40_school, 
         nonphy40_other_3m = nonphy40_other,
         nonphy60_home_3m = nonphy60_home, 
         nonphy60_work_3m = nonphy60_work, 
         nonphy60_school_3m = nonphy60_school, 
         nonphy60_other_3m = nonphy60_other,
         nonphy70_home_3m = nonphy70_home, 
         nonphy70_work_3m = nonphy70_work, 
         nonphy70_school_3m = nonphy70_school, 
         nonphy70_other_3m = nonphy70_other) -> m3_2

# combine all m3 variaables
left_join(m3, m3_2, by = 'p_id') -> m3_all 

# add in some variables that are only in baseline dataset to m3 dataset

bline_all %>% 
  select(p_id,
         AGE_I, agecat_contact, GENDER_I, INCOME_I, RACE_ETHNICITY_I,
         job_type, hh_num, hhnum_cat,
         state) -> bline_all_varsform3

left_join(m3_all, bline_all_varsform3, by = 'p_id') -> m3_all 


# add m3 weighting variable to baseline dataset
m3_all %>% 
  select(p_id, WTFinal_US_M3_AbIGg) -> m3_all_varsforbl

left_join(bline_all, m3_all_varsforbl, by = 'p_id') -> bline_all 

# create vars that collapse contacts across locations and ages

# baseline

bline_all %>% 
  
  
  mutate(totphys0 = rowSums(select(., phy0_home, phy0_work, phy0_school, phy0_other), na.rm=TRUE),
         
         totnonphys0 = rowSums(select(.,nonphy0_home, nonphy0_work, nonphy0_school, nonphy0_other), na.rm=TRUE),
         
         totphys5 = rowSums(select(.,phy5_home, phy5_work, phy5_school, phy5_other), na.rm=TRUE),
         totnonphys5 = rowSums(select(.,nonphy5_home, nonphy5_work, nonphy5_school, nonphy5_other), na.rm=TRUE),
  
         totphys10 = rowSums(select(.,phy10_home, phy10_work, phy10_school, phy10_other), na.rm=TRUE),
         totnonphys10 = rowSums(select(.,nonphy10_home, nonphy10_work, nonphy10_school, nonphy10_other), na.rm=TRUE),
  
         totphys20 = rowSums(select(.,phy20_home, phy20_work, phy20_school, phy20_other), na.rm=TRUE),
         totnonphys20 = rowSums(select(.,nonphy20_home, nonphy20_work, nonphy20_school, nonphy20_other), na.rm=TRUE),
  
         totphys40 = rowSums(select(.,phy40_home, phy40_work, phy40_school, phy40_other), na.rm=TRUE),
         totnonphys40 = rowSums(select(.,nonphy40_home, nonphy40_work, nonphy40_school, nonphy40_other), na.rm=TRUE),
  
         totphys60 = rowSums(select(.,phy60_home, phy60_work, phy60_school, phy60_other), na.rm=TRUE),
         totnonphys60 = rowSums(select(.,nonphy60_home, nonphy60_work, nonphy60_school, nonphy60_other), na.rm=TRUE),
  
         totphys70 = rowSums(select(.,phy70_home, phy70_work, phy70_school, phy70_other), na.rm=TRUE),
         totnonphys70 = rowSums(select(.,nonphy70_home, nonphy70_work, nonphy70_school, nonphy70_other), na.rm=TRUE),
         
         totphyshome = rowSums(select(.,phy0_home, phy5_home, phy10_home, phy20_home, phy40_home, phy60_home, phy70_home), na.rm=TRUE),
         totphyswork = rowSums(select(.,phy0_work, phy5_work, phy10_work, phy20_work, phy40_work, phy60_work, phy70_work), na.rm=TRUE),
         totphysschool = rowSums(select(.,phy0_school, phy5_school, phy10_school, phy20_school, phy40_school, phy60_school, phy70_school), na.rm=TRUE),
         totphysother = rowSums(select(.,phy0_other, phy5_other, phy10_other, phy20_other, phy40_other, phy60_other, phy70_other), na.rm=TRUE),
         
         totnonphyshome = rowSums(select(.,nonphy0_home, nonphy5_home, nonphy10_home, nonphy20_home, nonphy40_home, nonphy60_home, nonphy70_home), na.rm=TRUE),
         totnonphyswork = rowSums(select(.,nonphy0_work, nonphy5_work, nonphy10_work, nonphy20_work, nonphy40_work, nonphy60_work, nonphy70_work), na.rm=TRUE),
         totnonphysschool = rowSums(select(.,nonphy0_school, nonphy5_school, nonphy10_school, nonphy20_school, nonphy40_school, nonphy60_school, nonphy70_school), na.rm=TRUE),
         totnonphysother = rowSums(select(.,nonphy0_other, nonphy5_other, nonphy10_other, nonphy20_other, nonphy40_other, nonphy60_other, nonphy70_other), na.rm=TRUE)

          )-> bline_all

  # month 3

m3_all %>% 
  

  mutate(totphys0_3m = rowSums(select(.,phy0_home_3m, phy0_work_3m, phy0_school_3m, phy0_other_3m), na.rm=TRUE),
          totnonphys0_3m = rowSums(select(.,nonphy0_home_3m, nonphy0_work_3m, nonphy0_school_3m, nonphy0_other_3m), na.rm=TRUE),
          
          totphys5_3m = rowSums(select(.,phy5_home_3m, phy5_work_3m, phy5_school_3m, phy5_other_3m), na.rm=TRUE),
          totnonphys5_3m = rowSums(select(.,nonphy5_home_3m, nonphy5_work_3m, nonphy5_school_3m, nonphy5_other_3m), na.rm=TRUE),
          
          totphys10_3m = rowSums(select(.,phy10_home_3m, phy10_work_3m, phy10_school_3m, phy10_other_3m), na.rm=TRUE),
          totnonphys10_3m = rowSums(select(.,nonphy10_home_3m, nonphy10_work_3m, nonphy10_school_3m, nonphy10_other_3m), na.rm=TRUE),
          
          totphys20_3m = rowSums(select(.,phy20_home_3m, phy20_work_3m, phy20_school_3m, phy20_other_3m), na.rm=TRUE),
          totnonphys20_3m = rowSums(select(.,nonphy20_home_3m, nonphy20_work_3m, nonphy20_school_3m, nonphy20_other_3m), na.rm=TRUE),
          
          totphys40_3m = rowSums(select(.,phy40_home_3m, phy40_work_3m, phy40_school_3m, phy40_other_3m), na.rm=TRUE),
          totnonphys40_3m = rowSums(select(.,nonphy40_home_3m, nonphy40_work_3m, nonphy40_school_3m, nonphy40_other_3m), na.rm=TRUE),
          
          totphys60_3m = rowSums(select(.,phy60_home_3m, phy60_work_3m, phy60_school_3m, phy60_other_3m), na.rm=TRUE),
          totnonphys60_3m = rowSums(select(.,nonphy60_home_3m, nonphy60_work_3m, nonphy60_school_3m, nonphy60_other_3m), na.rm=TRUE),
          
          totphys70_3m = rowSums(select(.,phy70_home_3m, phy70_work_3m, phy70_school_3m, phy70_other_3m), na.rm=TRUE),
          totnonphys70_3m = rowSums(select(.,nonphy70_home_3m, nonphy70_work_3m, nonphy70_school_3m, nonphy70_other_3m), na.rm=TRUE),
          
          totphyshome_3m = rowSums(select(.,phy0_home_3m, phy5_home_3m, phy10_home_3m, phy20_home_3m, phy40_home_3m, phy60_home_3m, phy70_home_3m), na.rm=TRUE),
          totphyswork_3m = rowSums(select(.,phy0_work_3m, phy5_work_3m, phy10_work_3m, phy20_work_3m, phy40_work_3m, phy60_work_3m, phy70_work_3m), na.rm=TRUE),
          totphysschool_3m = rowSums(select(.,phy0_school_3m, phy5_school_3m, phy10_school_3m, phy20_school_3m, phy40_school_3m, phy60_school_3m, phy70_school_3m), na.rm=TRUE),
          totphysother_3m = rowSums(select(.,phy0_other_3m, phy5_other_3m, phy10_other_3m, phy20_other_3m, phy40_other_3m, phy60_other_3m, phy70_other_3m), na.rm=TRUE),
          
          totnonphyshome_3m = rowSums(select(.,nonphy0_home_3m, nonphy5_home_3m, nonphy10_home_3m, nonphy20_home_3m, nonphy40_home_3m, nonphy60_home_3m, nonphy70_home_3m), na.rm=TRUE),
          totnonphyswork_3m = rowSums(select(.,nonphy0_work_3m, nonphy5_work_3m, nonphy10_work_3m, nonphy20_work_3m, nonphy40_work_3m, nonphy60_work_3m, nonphy70_work_3m), na.rm=TRUE),
          totnonphysschool_3m = rowSums(select(.,nonphy0_school_3m, nonphy5_school_3m, nonphy10_school_3m, nonphy20_school_3m, nonphy40_school_3m, nonphy60_school_3m, nonphy70_school_3m), na.rm=TRUE),
          totnonphysother_3m = rowSums(select(.,nonphy0_other_3m, nonphy5_other_3m, nonphy10_other_3m, nonphy20_other_3m, nonphy40_other_3m, nonphy60_other_3m, nonphy70_other_3m), na.rm=TRUE) 

          ) -> m3_all

# create vars that collpase across type of contact (phys/non-phys), by age and location
  
# baseline
bline_all %>%
  

  mutate(totcon0 = rowSums(select(.,totphys0, totnonphys0), na.rm=TRUE),
         totcon5 = rowSums(select(.,totphys5, totnonphys5), na.rm=TRUE),
         totcon10 = rowSums(select(.,totphys10, totnonphys10), na.rm=TRUE), 
         totcon20 = rowSums(select(.,totphys20, totnonphys20), na.rm=TRUE), 
         totcon40 = rowSums(select(.,totphys40, totnonphys40), na.rm=TRUE),
         totcon60 = rowSums(select(.,totphys60, totnonphys60), na.rm=TRUE),
         totcon70 = rowSums(select(.,totphys70, totnonphys70), na.rm=TRUE),
                
         tothome = rowSums(select(.,totphyshome, totnonphyshome), na.rm=TRUE),
         totwork = rowSums(select(.,totphyswork, totnonphyswork), na.rm=TRUE),
         totschool = rowSums(select(.,totphysschool, totnonphysschool), na.rm=TRUE),
         totother = rowSums(select(.,totphysother, totnonphysother), na.rm=TRUE),
         
         totphys = rowSums(select(.,totphys0, totphys5, totphys10, totphys20, totphys40, totphys60, totphys70), na.rm=TRUE),
         totnonphys = rowSums(select(.,totnonphys0, totnonphys5, totnonphys10, totnonphys20, totnonphys40, totnonphys60, totnonphys70), na.rm=TRUE)) -> bline_all_t
         
        
bline_all_t %>%

  mutate(totcon_all = rowSums(select(.,tothome, totwork, totschool, totother), na.rm=TRUE)) -> bline_all
         
# month 3

m3_all %>%
  
         mutate(totcon0_3m = rowSums(select(.,totphys0_3m, totnonphys0_3m), na.rm=TRUE),
         totcon5_3m = rowSums(select(.,totphys5_3m, totnonphys5_3m), na.rm=TRUE),
         totcon10_3m = rowSums(select(.,totphys10_3m, totnonphys10_3m), na.rm=TRUE), 
         totcon20_3m = rowSums(select(.,totphys20_3m, totnonphys20_3m), na.rm=TRUE), 
         totcon40_3m = rowSums(select(.,totphys40_3m, totnonphys40_3m), na.rm=TRUE),
         totcon60_3m = rowSums(select(.,totphys60_3m, totnonphys60_3m), na.rm=TRUE),
         totcon70_3m = rowSums(select(.,totphys70_3m, totnonphys70_3m), na.rm=TRUE),
         
         tothome_3m = rowSums(select(.,totphyshome_3m, totnonphyshome_3m), na.rm=TRUE),
         totwork_3m = rowSums(select(.,totphyswork_3m, totnonphyswork_3m), na.rm=TRUE),
         totschool_3m = rowSums(select(.,totphysschool_3m, totnonphysschool_3m), na.rm=TRUE),
         totother_3m = rowSums(select(.,totphysother_3m, totnonphysother_3m), na.rm=TRUE),
         
         totphys_3m = rowSums(select(.,totphys0_3m, totphys5_3m, totphys10_3m, totphys20_3m, totphys40_3m, totphys60_3m, totphys70_3m), na.rm=TRUE),
         totnonphys_3m = rowSums(select(.,totnonphys0_3m, totnonphys5_3m, totnonphys10_3m, totnonphys20_3m, totnonphys40_3m, totnonphys60_3m, totnonphys70_3m), na.rm=TRUE)) -> m3_all_t
  

m3_all_t %>%

         mutate(totcon_all_3m = rowSums(select(.,tothome_3m, totwork_3m, totschool_3m, totother_3m), na.rm=TRUE)) -> m3_all


### Limit baseline dataset to only those with follo-wup data

bline_all %>%
  filter(p_id %in% m3_all$p_id) -> bline_all


### Overall with 29 contact right-truncation approach (based on POLYMOD)

bline_all %>%
  mutate(totcon_all = ifelse(totcon_all >29, 29, totcon_all)) -> bline_all_trunc29

m3_all %>%
  mutate(totcon_all_3m = ifelse(totcon_all_3m >29, 29, totcon_all_3m)) -> m3_all_trunc29


### Overall with 50 contact / age group rtuncation (based on CoMix) = 50*9 age groups = 450 totaal contacts

bline_all %>%
  mutate(totcon_all = ifelse(totcon_all >450, 450, totcon_all)) -> bline_all_trunc50

m3_all %>%
  mutate(totcon_all_3m = ifelse(totcon_all_3m >450, 450, totcon_all_3m)) -> m3_all_trunc50

### Overall removing two extreme outliers

bline_all %>%
  filter(p_id != 11427601  &
           p_id != 11777201) -> bline_all_nooutlier

m3_all %>%
  filter(p_id != 11427601  &
           p_id != 11777201) -> m3_all_nooutlier


# How many truncated? 
#BL
bline_all %>% 
  filter(totcon_all > 29) %>%
  print(n=50) %>%
  select(AGE_I, GENDER_I, INCOME_I, totphys, totnonphys, totwork, tothome, totschool, totother) -> high_c
#220 participants (220/3112 = 7%) report > 29 contacts
#M3
m3_all %>% 
  filter(totcon_all_3m > 29) %>%
  print(n=50) 
#350 participants (249/3112 = 8%) report > 29 contacts

#BL
bline_all %>% 
  filter(totcon_all > 450) %>%
  print(n=50) 
#7 participants (5/3112 = 0.2%) report > 450 contacts
#M3
m3_all %>% 
  filter(totcon_all_3m > 450) %>%
  print(n=50) 
#10 participants (10/3112 = 0.32%) report > 450 contacts


###################
# create a dataset using the survey packaage that adjusts for survey weights for baseline and month 3
# see https://cran.r-project.org/web/packages/survey/survey.pdf for more information 
###################

# Use m3 weights for both baseline and m3, since the sample is m3 participants

bline_all_s <- svydesign(id=~1, weights=~WTFinal_US_M3_AbIGg, data=bline_all)
bline_trunc29_s <- svydesign(id=~1, weights=~WTFinal_US_M3_AbIGg, data=bline_all_trunc29)
bline_trunc50_s <- svydesign(id=~1, weights=~WTFinal_US_M3_AbIGg, data=bline_all_trunc50)
bline_nooutlier_s <- svydesign(id=~1, weights=~WTFinal_US_M3_AbIGg, data=bline_all_nooutlier)



m3_all_s <- svydesign(id=~1, weights=~WTFinal_US_M3_AbIGg, data=m3_all)
m3_trunc29_s  <- svydesign(id=~1, weights=~WTFinal_US_M3_AbIGg, data=m3_all_trunc29)
m3_trunc50_s <- svydesign(id=~1, weights=~WTFinal_US_M3_AbIGg, data=m3_all_trunc50)
m3_nooutlier_s <- svydesign(id=~1, weights=~WTFinal_US_M3_AbIGg, data=m3_all_nooutlier)



###################
# descriptive - contact by age, location, and contact type. Use 50 contact cutoff to align with CoMix data
###################

# Overall contact rates
#BL - ALL
#svymean(~totcon_all, bline_all_s)
#svyquantile(~totcon_all, bline_all_s, c(0.25, 0.75))

#3M - ALL
#svymean(~totcon_all_3m, m3_all_s)
#svyquantile(~totcon_all_3m, bline_all_s, c(0.25, 0.75))


#BL - 50 contact truncate
svymean(~totcon_all, bline_trunc50_s)
svyquantile(~totcon_all, bline_trunc50_s, c(0.25, 0.50, 0.75, 0.9))


#3M - 50 contact truncate
svymean(~totcon_all_3m, m3_trunc50_s)
svyquantile(~totcon_all_3m, m3_trunc50_s, c(0.25, 0.50, 0.75, 0.90))


wtd.t.test(x=bline_all_trunc50$totcon_all, y=m3_all_trunc50$totcon_all_3m, weight=bline_all_trunc50$WTFinal_US_M3_AbIGg, 
           alternative="two.tailed")

Ecume::ks_test(bline_all_trunc50$totcon_all, m3_all_trunc50$totcon_all_3m, thres = 0.005, w_x =  bline_all_trunc50$WTFinal_US_M3_AbIGg, w_y = m3_all_trunc50$WTFinal_US_M3_AbIGg)


# By type
#BL
svymean(~totphys, bline_trunc50_s)
svyquantile(~totphys, bline_trunc50_s, c(0.25, 0.50, 0.75, 0.90))

svymean(~totnonphys, bline_trunc50_s)
svyquantile(~totnonphys, bline_trunc50_s, c(0.25, 0.50, 0.75, 0.90))

#3M 
svymean(~totphys_3m, m3_trunc50_s)
svyquantile(~totphys_3m, m3_trunc50_s, c(0.25, 0.50, 0.75, 0.90))

svymean(~totnonphys_3m, m3_trunc50_s)
svyquantile(~totnonphys_3m, m3_trunc50_s, c(0.25, 0.50, 0.75, 0.90))


wtd.t.test(x=bline_all_trunc50$totphys, y=m3_all_trunc50$totphys_3m, weight=bline_all_trunc50$WTFinal_US_M3_AbIGg, 
           alternative="two.tailed")

Ecume::ks_test(bline_all_trunc50$totphys, m3_all_trunc50$totphys_3m, thres = 0.005, w_x =  bline_all_trunc50$WTFinal_US_M3_AbIGg, w_y = m3_all_trunc50$WTFinal_US_M3_AbIGg)

wtd.t.test(x=bline_all_trunc50$totnonphys, y=m3_all_trunc50$totnonphys_3m, weight=bline_all_trunc50$WTFinal_US_M3_AbIGg, 
           alternative="two.tailed")

Ecume::ks_test(bline_all_trunc50$totnonphys, m3_all_trunc50$totnonphys_3m, thres = 0.005, w_x =  bline_all_trunc50$WTFinal_US_M3_AbIGg, w_y = m3_all_trunc50$WTFinal_US_M3_AbIGg)


# By location
svymean(~tothome, bline_trunc50_s)
svyquantile(~tothome, bline_trunc50_s, c(0.25, 0.50, 0.75, 0.90))

svymean(~totwork, bline_trunc50_s)
svyquantile(~totwork, bline_trunc50_s, c(0.25, 0.50, 0.75, 0.90))

svymean(~totschool, bline_trunc50_s)
svyquantile(~totschool, bline_trunc50_s, c(0.25, 0.50, 0.75, 0.90))

svymean(~totother, bline_trunc50_s)
svyquantile(~totother, bline_trunc50_s, c(0.25, 0.50, 0.75, 0.90))


svymean(~tothome_3m, m3_trunc50_s)
svyquantile(~tothome_3m, m3_trunc50_s, c(0.25, 0.50, 0.75, 0.90))

svymean(~totwork_3m, m3_trunc50_s)
svyquantile(~totwork_3m, m3_trunc50_s, c(0.25, 0.50, 0.75, 0.90))

svymean(~totschool_3m, m3_trunc50_s)
svyquantile(~totschool_3m, m3_trunc50_s, c(0.25, 0.50, 0.75, 0.90))

svymean(~totother_3m, m3_trunc50_s)
svyquantile(~totother_3m, m3_trunc50_s, c(0.25, 0.50, 0.75, 0.90))

wtd.t.test(x=bline_all_trunc50$tothome, y=m3_all_trunc50$tothome_3m, weight=bline_all_trunc50$WTFinal_US_M3_AbIGg, 
           alternative="two.tailed")

Ecume::ks_test(bline_all_trunc50$tothome, m3_all_trunc50$tothome_3m, thres = 0.005, w_x =  bline_all_trunc50$WTFinal_US_M3_AbIGg, w_y = m3_all_trunc50$WTFinal_US_M3_AbIGg)


wtd.t.test(x=bline_all_trunc50$totwork, y=m3_all_trunc50$totwork_3m, weight=bline_all_trunc50$WTFinal_US_M3_AbIGg, 
           alternative="two.tailed")

Ecume::ks_test(bline_all_trunc50$totwork, m3_all_trunc50$totwork_3m, thres = 0.005, w_x =  bline_all_trunc50$WTFinal_US_M3_AbIGg, w_y = m3_all_trunc50$WTFinal_US_M3_AbIGg)


wtd.t.test(x=bline_all_trunc50$totschool, y=m3_all_trunc50$totschool_3m, weight=bline_all_trunc50$WTFinal_US_M3_AbIGg, 
           alternative="two.tailed")

Ecume::ks_test(bline_all_trunc50$totschool, m3_all_trunc50$totschool_3m, thres = 0.005, w_x =  bline_all_trunc50$WTFinal_US_M3_AbIGg, w_y = m3_all_trunc50$WTFinal_US_M3_AbIGg)


wtd.t.test(x=bline_all_trunc50$totother, y=m3_all_trunc50$totother_3m, weight=bline_all_trunc50$WTFinal_US_M3_AbIGg, 
           alternative="two.tailed")

Ecume::ks_test(bline_all_trunc50$totother, m3_all_trunc50$totother_3m, thres = 0.05, w_x =  bline_all_trunc50$WTFinal_US_M3_AbIGg, w_y = m3_all_trunc50$WTFinal_US_M3_AbIGg)


# By age
svyby(~totcon_all, ~AGE_I, bline_trunc50_s, svymean) # mean
ddply(bline_all_trunc50, ~AGE_I, summarise, 
      totcon_all=wtd.quantile(totcon_all, weight = WTFinal_US_M3_AbIGg, probs=c(0.25, 0.5, 0.75, 0.9)))


svyby(~totcon_all_3m, ~AGE_I, m3_trunc50_s, svymean)
ddply(m3_all_trunc50, ~AGE_I, summarise, 
      totcon_all=wtd.quantile(totcon_all_3m, weight = WTFinal_US_M3_AbIGg, probs=c(0.25, 0.5, 0.75, 0.9)))


# AGE_I == 1
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 1, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$AGE_I == 1, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 1, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 1, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$AGE_I == 1, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 1, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$AGE_I == 1, "WTFinal_US_M3_AbIGg"]))

# AGE_I == 2
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 2, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$AGE_I == 2, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 2, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 2, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$AGE_I == 2, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 2, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$AGE_I == 2, "WTFinal_US_M3_AbIGg"]))


# AGE_I == 3
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 3, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$AGE_I == 3, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 3, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 3, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$AGE_I == 3, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 3, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$AGE_I == 3, "WTFinal_US_M3_AbIGg"]))


# AGE_I == 4
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 4, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$AGE_I == 4, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 4, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 4, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$AGE_I == 4, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 4, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$AGE_I == 4, "WTFinal_US_M3_AbIGg"]))


# AGE_I == 5
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 5, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$AGE_I == 5, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 5, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 5, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$AGE_I == 5, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 5, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$AGE_I == 5, "WTFinal_US_M3_AbIGg"]))

# AGE_I == 6
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 6, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$AGE_I == 6, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 6, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 6, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$AGE_I == 6, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$AGE_I == 6, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$AGE_I == 6, "WTFinal_US_M3_AbIGg"]))


# By sex
svyby(~totcon_all, ~GENDER_I, bline_trunc50_s, svymean)
ddply(bline_all_trunc50, ~GENDER_I, summarise, 
      totcon_all=wtd.quantile(totcon_all, weight = WTFinal_US_M3_AbIGg, probs=c(0.25, 0.5, 0.75, 0.9)))

svyby(~totcon_all_3m, ~GENDER_I, m3_trunc50_s, svymean)
ddply(m3_all_trunc50, ~GENDER_I, summarise, 
      totcon_all=wtd.quantile(totcon_all_3m, weight = WTFinal_US_M3_AbIGg, probs=c(0.25, 0.5, 0.75, 0.9)))

# GENDER_I == 1
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$GENDER_I == 1, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$GENDER_I == 1, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$GENDER_I == 1, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$GENDER_I == 1, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$GENDER_I == 1, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$GENDER_I == 1, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$GENDER_I == 1, "WTFinal_US_M3_AbIGg"]))


# GENDER_I == 2
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$GENDER_I == 2, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$GENDER_I == 2, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$GENDER_I == 2, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$GENDER_I == 2, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$GENDER_I == 2, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$GENDER_I == 2, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$GENDER_I == 2, "WTFinal_US_M3_AbIGg"]))



# By race/eth
svyby(~totcon_all, ~RACE_ETHNICITY_I, bline_trunc50_s, svymean)
ddply(bline_all_trunc50, ~RACE_ETHNICITY_I, summarise, 
      totcon_all=wtd.quantile(totcon_all, weight = WTFinal_US_M3_AbIGg, probs=c(0.25, 0.5, 0.75, 0.9)))

svyby(~totcon_all_3m, ~RACE_ETHNICITY_I, m3_trunc50_s, svymean)
ddply(m3_all_trunc50, ~RACE_ETHNICITY_I, summarise, 
      totcon_all=wtd.quantile(totcon_all_3m, weight = WTFinal_US_M3_AbIGg, probs=c(0.25, 0.5, 0.75, 0.9)))


# RACE_ETHNICITY_I == 1
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$RACE_ETHNICITY_I == 1, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$RACE_ETHNICITY_I == 1, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$RACE_ETHNICITY_I == 1, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$RACE_ETHNICITY_I == 1, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$RACE_ETHNICITY_I == 1, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$RACE_ETHNICITY_I == 1, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$RACE_ETHNICITY_I == 1, "WTFinal_US_M3_AbIGg"]))

# RACE_ETHNICITY_I == 2
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$RACE_ETHNICITY_I == 2, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$RACE_ETHNICITY_I == 2, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$RACE_ETHNICITY_I == 2, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$RACE_ETHNICITY_I == 2, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$RACE_ETHNICITY_I == 2, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$RACE_ETHNICITY_I == 2, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$RACE_ETHNICITY_I == 2, "WTFinal_US_M3_AbIGg"]))


# RACE_ETHNICITY_I == 3
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$RACE_ETHNICITY_I == 3, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$RACE_ETHNICITY_I == 3, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$RACE_ETHNICITY_I == 3, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$RACE_ETHNICITY_I == 3, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$RACE_ETHNICITY_I == 3, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$RACE_ETHNICITY_I == 3, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$RACE_ETHNICITY_I == 3, "WTFinal_US_M3_AbIGg"]))

# RACE_ETHNICITY_I == 4
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$RACE_ETHNICITY_I == 4, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$RACE_ETHNICITY_I == 4, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$RACE_ETHNICITY_I == 4, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$RACE_ETHNICITY_I == 4, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$RACE_ETHNICITY_I == 4, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$RACE_ETHNICITY_I == 4, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$RACE_ETHNICITY_I == 4, "WTFinal_US_M3_AbIGg"]))

# RACE_ETHNICITY_I == 5
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$RACE_ETHNICITY_I == 5, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$RACE_ETHNICITY_I == 5, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$RACE_ETHNICITY_I == 5, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$RACE_ETHNICITY_I == 5, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$RACE_ETHNICITY_I == 5, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$RACE_ETHNICITY_I == 5, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$RACE_ETHNICITY_I == 5, "WTFinal_US_M3_AbIGg"]))

# By income
svyby(~totcon_all, ~INCOME_I, bline_trunc50_s, svymean)
ddply(bline_all_trunc50, ~INCOME_I, summarise, 
      totcon_all=wtd.quantile(totcon_all, weight = WTFinal_US_M3_AbIGg, probs=c(0.25, 0.5, 0.75, 0.9)))

svyby(~totcon_all_3m, ~INCOME_I, m3_trunc50_s, svymean)
ddply(m3_all_trunc50, ~INCOME_I, summarise, 
      totcon_all=wtd.quantile(totcon_all_3m, weight = WTFinal_US_M3_AbIGg, probs=c(0.25, 0.5, 0.75, 0.9)))

# INCOME_I == 1
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 1, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 1, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 1, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 1, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 1, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 1, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 1, "WTFinal_US_M3_AbIGg"]))

# INCOME_I == 2
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 2, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 2, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 2, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 2, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 2, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 2, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 2, "WTFinal_US_M3_AbIGg"]))

# INCOME_I == 3
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 3, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 3, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 3, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 3, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 3, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 3, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 3, "WTFinal_US_M3_AbIGg"]))

# INCOME_I == 4
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 4, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 4, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 4, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 4, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 4, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 4, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 4, "WTFinal_US_M3_AbIGg"]))

# INCOME_I == 5
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 5, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 5, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 5, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 5, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 5, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 5, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 5, "WTFinal_US_M3_AbIGg"]))

# INCOME_I == 6
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 6, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 6, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 6, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 6, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 6, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 6, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 6, "WTFinal_US_M3_AbIGg"]))

# INCOME_I == 7
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 7, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 7, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 7, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 7, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 7, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 7, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 7, "WTFinal_US_M3_AbIGg"]))

# INCOME_I == 8
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 8, "totcon_all"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 8, "totcon_all_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 8, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed")

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 8, "totcon_all"]), 
               pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 8, "totcon_all_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$INCOME_I == 8, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$INCOME_I == 8, "WTFinal_US_M3_AbIGg"]))


# Occupation
svyby(~totwork, ~job_type, bline_trunc50_s, svymean)
ddply(bline_all_trunc50, ~job_type, summarise, 
      totcon_all=wtd.quantile(totwork, weight = WTFinal_US_M3_AbIGg, probs=c(0.25, 0.5, 0.75, 0.9)))

svyby(~totwork_3m, ~job_type, m3_trunc50_s, svymean)
ddply(m3_all_trunc50, ~job_type, summarise, 
      totcon_all=wtd.quantile(totwork_3m, weight = WTFinal_US_M3_AbIGg, probs=c(0.25, 0.5, 0.75, 0.9)))

t <- list()
ks <- list()
for(i in 1:21){
wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$job_type == i, "totwork"]), 
           y=pull(m3_all_trunc50[m3_all_trunc50$job_type == i, "totwork_3m"]), 
           weight=pull(bline_all_trunc50[bline_all_trunc50$job_type == i, "WTFinal_US_M3_AbIGg"]), 
           alternative="two.tailed") -> t[[i]]

Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$job_type == i, "totwork"]), 
               pull(m3_all_trunc50[m3_all_trunc50$job_type == i, "totwork_3m"]), 
               thres = 0.05, 
               w_x =  pull(bline_all_trunc50[bline_all_trunc50$job_type == i, "WTFinal_US_M3_AbIGg"]), 
               w_y = pull(m3_all_trunc50[m3_all_trunc50$job_type == i, "WTFinal_US_M3_AbIGg"])) -> ks[[i]]


}

# By serostatus

svyby(~totcon_all, ~dbs_result2, bline_trunc50_s, svymean)

### need to remove indeterminate result to get medians
bline_all_trunc50 %>% 
  filter(dbs_result2 != 5) -> bline_trunc50_noind
bline_trunc50_s_noind <- svydesign(id=~1, weights=~WTFinal_US_Plus2_TotalIg, data=bline_trunc50_noind)
svyby(~totcon_all, ~dbs_result2, bline_trunc50_s_noind, svymean)
ddply(bline_trunc50_noind, ~dbs_result2, summarise, 
      totcon_all=wtd.quantile(totcon_all, weight = WTFinal_US_M3_AbIGg, probs=c(0.25, 0.5, 0.75, 0.9)))

svyby(~totcon_all_3m, ~m3_TAb, m3_trunc50_s, svymean)
tabmedians.svy(totcon_all_3m~m3_TAb, m3_trunc50_s, parenth = "q1q3")
ddply(m3_all_trunc50, ~m3_TAb, summarise, 
      totcon_all=wtd.quantile(totcon_all_3m, weight = WTFinal_US_M3_AbIGg, probs=c(0.25, 0.5, 0.75, 0.9)))

### want to also look at those who were negative at both BL and FU and those with incidence infection (- at BL, + at FU)

select(bline_trunc50_noind, c(p_id, dbs_result2, WTFinal_US_M3_AbIGg, totcon_all)) -> bline_trunc50_noind_serores
select(m3_all_trunc50, c(p_id, m3_TAb, totcon_all_3m)) -> m3_trunc50_serores

left_join(bline_trunc50_noind_serores,  m3_trunc50_serores, by = 'p_id') -> bline_3m_trunc50

bline_3m_trunc50 %>%
  mutate(negboth = ifelse(dbs_result2 == 4 & m3_TAb == 2, 1, 0)) %>%
  mutate(incpos = ifelse(dbs_result2 == 4 & m3_TAb == 1, 1, 0)) ->  bline_3m_trunc50

bline_m3_serores_trunc50_s <- svydesign(id=~1, weights=~ WTFinal_US_M3_AbIGg, data=bline_3m_trunc50)


svyby(~totcon_all, ~negboth, bline_m3_serores_trunc50_s, svymean)
svyby(~totcon_all_3m, ~negboth, bline_m3_serores_trunc50_s, svymean)

svyby(~totcon_all, ~incpos, bline_m3_serores_trunc50_s, svymean)
svyby(~totcon_all_3m, ~incpos, bline_m3_serores_trunc50_s, svymean)

ddply(bline_3m_trunc50, ~negboth, summarise, 
      totcon_all=wtd.quantile(totcon_all, weight = WTFinal_US_M3_AbIGg, probs=c(0.25, 0.5, 0.75, 0.9)))

ddply(bline_3m_trunc50, ~negboth, summarise, 
      totcon_all=wtd.quantile(totcon_all_3m, weight = WTFinal_US_M3_AbIGg, probs=c(0.25, 0.5, 0.75, 0.9)))

ddply(bline_3m_trunc50, ~incpos, summarise, 
      totcon_all=wtd.quantile(totcon_all, weight = WTFinal_US_M3_AbIGg, probs=c(0.25, 0.5, 0.75, 0.9)))

ddply(bline_3m_trunc50, ~incpos, summarise, 
      totcon_all=wtd.quantile(totcon_all_3m, weight = WTFinal_US_M3_AbIGg, probs=c(0.25, 0.5, 0.75, 0.9)))


###can look also at serostaatus by work/other/home contaacts
svyby(~totwork, ~dbs_result2, bline_trunc50_s, svymean)
svyby(~totother, ~dbs_result2, bline_trunc50_s, svymean)
svyby(~tothome, ~dbs_result2, bline_trunc50_s, svymean)

svyby(~totwork_3m, ~m3_TAb, m3_trunc50_s, svymean)
svyby(~totother_3m, ~m3_TAb, m3_trunc50_s, svymean)
svyby(~tothome_3m, ~m3_TAb, m3_trunc50_s, svymean)


# By HH size

svyby(~totcon_all, ~hhnum_cat, bline_trunc50_s, svymean)
ddply(bline_all_trunc50, ~hhnum_cat, summarise, 
      totcon_all=wtd.quantile(totcon_all, weight = WTFinal_US_M3_AbIGg, probs=c(0.25, 0.5, 0.75, 0.9)))

svyby(~totcon_all_3m, ~hhnum_cat, m3_trunc50_s, svymean)
ddply(m3_all_trunc50, ~hhnum_cat, summarise, 
      totcon_all=wtd.quantile(totcon_all_3m, weight = WTFinal_US_M3_AbIGg, probs=c(0.25, 0.5, 0.75, 0.9)))

#(0,1]
  wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$hhnum_cat == "(0,1]", "totcon_all"]), 
             y=pull(m3_all_trunc50[m3_all_trunc50$hhnum_cat == "(0,1]", "totcon_all_3m"]), 
             weight=pull(bline_all_trunc50[bline_all_trunc50$hhnum_cat == "(0,1]", "WTFinal_US_M3_AbIGg"]), 
             alternative="two.tailed")
  
  Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$hhnum_cat == "(0,1]", "totcon_all"]), 
                 pull(m3_all_trunc50[m3_all_trunc50$hhnum_cat == "(0,1]", "totcon_all_3m"]), 
                 thres = 0.05, 
                 w_x =  pull(bline_all_trunc50[bline_all_trunc50$hhnum_cat == "(0,1]", "WTFinal_US_M3_AbIGg"]), 
                 w_y = pull(m3_all_trunc50[m3_all_trunc50$hhnum_cat == "(0,1]", "WTFinal_US_M3_AbIGg"])) 
  
#(1,2] 
  wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$hhnum_cat == "(1,2]", "totcon_all"]), 
             y=pull(m3_all_trunc50[m3_all_trunc50$hhnum_cat == "(1,2]", "totcon_all_3m"]), 
             weight=pull(bline_all_trunc50[bline_all_trunc50$hhnum_cat == "(1,2]", "WTFinal_US_M3_AbIGg"]), 
             alternative="two.tailed")
  
  Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$hhnum_cat == "(1,2]", "totcon_all"]), 
                 pull(m3_all_trunc50[m3_all_trunc50$hhnum_cat == "(1,2]", "totcon_all_3m"]), 
                 thres = 0.05, 
                 w_x =  pull(bline_all_trunc50[bline_all_trunc50$hhnum_cat == "(1,2]", "WTFinal_US_M3_AbIGg"]), 
                 w_y = pull(m3_all_trunc50[m3_all_trunc50$hhnum_cat == "(1,2]", "WTFinal_US_M3_AbIGg"])) 
  
#(2,3] 
  wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$hhnum_cat == "(2,3]", "totcon_all"]), 
             y=pull(m3_all_trunc50[m3_all_trunc50$hhnum_cat == "(2,3]", "totcon_all_3m"]), 
             weight=pull(bline_all_trunc50[bline_all_trunc50$hhnum_cat == "(2,3]", "WTFinal_US_M3_AbIGg"]), 
             alternative="two.tailed")
  
  Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$hhnum_cat == "(2,3]", "totcon_all"]), 
                 pull(m3_all_trunc50[m3_all_trunc50$hhnum_cat == "(2,3]", "totcon_all_3m"]), 
                 thres = 0.05, 
                 w_x =  pull(bline_all_trunc50[bline_all_trunc50$hhnum_cat == "(2,3]", "WTFinal_US_M3_AbIGg"]), 
                 w_y = pull(m3_all_trunc50[m3_all_trunc50$hhnum_cat == "(2,3]", "WTFinal_US_M3_AbIGg"])) 
  
#(3,4] 
  wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$hhnum_cat == "(3,4]", "totcon_all"]), 
             y=pull(m3_all_trunc50[m3_all_trunc50$hhnum_cat == "(3,4]", "totcon_all_3m"]), 
             weight=pull(bline_all_trunc50[bline_all_trunc50$hhnum_cat == "(3,4]", "WTFinal_US_M3_AbIGg"]), 
             alternative="two.tailed")
  
  Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$hhnum_cat == "(3,4]", "totcon_all"]), 
                 pull(m3_all_trunc50[m3_all_trunc50$hhnum_cat == "(3,4]", "totcon_all_3m"]), 
                 thres = 0.05, 
                 w_x =  pull(bline_all_trunc50[bline_all_trunc50$hhnum_cat == "(3,4]", "WTFinal_US_M3_AbIGg"]), 
                 w_y = pull(m3_all_trunc50[m3_all_trunc50$hhnum_cat == "(3,4]", "WTFinal_US_M3_AbIGg"])) 
  
#(4,100] 
  wtd.t.test(x=pull(bline_all_trunc50[bline_all_trunc50$hhnum_cat == "(4,100]", "totcon_all"]), 
             y=pull(m3_all_trunc50[m3_all_trunc50$hhnum_cat == "(4,100]", "totcon_all_3m"]), 
             weight=pull(bline_all_trunc50[bline_all_trunc50$hhnum_cat == "(4,100]", "WTFinal_US_M3_AbIGg"]), 
             alternative="two.tailed")
  
  Ecume::ks_test(pull(bline_all_trunc50[bline_all_trunc50$hhnum_cat == "(4,100]", "totcon_all"]), 
                 pull(m3_all_trunc50[m3_all_trunc50$hhnum_cat == "(4,100]", "totcon_all_3m"]), 
                 thres = 0.05, 
                 w_x =  pull(bline_all_trunc50[bline_all_trunc50$hhnum_cat == "(4,100]", "WTFinal_US_M3_AbIGg"]), 
                 w_y = pull(m3_all_trunc50[m3_all_trunc50$hhnum_cat == "(4,100]", "WTFinal_US_M3_AbIGg"])) 
  

# could compare baseline contact and m3 sero result - but need to combine datasets AND use survey procedures (not sure how to do this)

# contact matrices
svyby(~totcon0, ~AGE_I, bline_trunc50_s, svymean)
tabmedians.svy(~totcon0, ~AGE_I, bline_trunc50_s, parenth = "q1q3")

svyby(~totcon5, ~AGE_I, bline_trunc50_s, svymean)
tabmedians.svy(~totcon5, ~AGE_I, bline_trunc50_s, parenth = "q1q3")

svyby(~totcon10, ~AGE_I, bline_trunc50_s, svymean)
tabmedians.svy(~totcon10, ~AGE_I, bline_trunc50_s, parenth = "q1q3")

svyby(~totcon20, ~AGE_I, bline_trunc50_s, svymean)
tabmedians.svy(~totcon20, ~AGE_I, bline_trunc50_s, parenth = "q1q3")

svyby(~totcon40, ~AGE_I, bline_trunc50_s, svymean)
tabmedians.svy(~totcon40, ~AGE_I, bline_trunc50_s, parenth = "q1q3")

svyby(~totcon60, ~AGE_I, bline_trunc50_s, svymean)
tabmedians.svy(~totcon60, ~AGE_I, bline_trunc50_s, parenth = "q1q3")

svyby(~totcon70, ~AGE_I, bline_trunc50_s, svymean)
tabmedians.svy(~totcon70, ~AGE_I, bline_trunc50_s, parenth = "q1q3")



svyby(~totcon0, ~AGE_I, m3_trunc50_s, svymean)
tabmedians.svy(~totcon0, ~AGE_I, bline_trunc50_s, parenth = "q1q3")

svyby(~totcon5, ~AGE_I, m3_trunc50_s, svymean)
tabmedians.svy(~totcon5, ~AGE_I, bline_trunc50_s, parenth = "q1q3")

svyby(~totcon10, ~AGE_I, m3_trunc50_s, svymean)
tabmedians.svy(~totcon10, ~AGE_I, bline_trunc50_s, parenth = "q1q3")

svyby(~totcon20, ~AGE_I, m3_trunc50_s, svymean)
tabmedians.svy(~totcon20, ~AGE_I, bline_trunc50_s, parenth = "q1q3")

svyby(~totcon40, ~AGE_I, m3_trunc50_s, svymean)
tabmedians.svy(~totcon40, ~AGE_I, bline_trunc50_s, parenth = "q1q3")

svyby(~totcon60, ~AGE_I, m3_trunc50_s, svymean)
tabmedians.svy(~totcon60, ~AGE_I, bline_trunc50_s, parenth = "q1q3")

svyby(~totcon70, ~AGE_I, m3_trunc50_s, svymean)
tabmedians.svy(~totcon70, ~AGE_I, bline_trunc50_s, parenth = "q1q3")


# Population Ns (survey and weighted)
# SURVEY - total N = 3112
table(m3_all_trunc50$AGE_I)
table(m3_all_trunc50$GENDER_I)
table(m3_all_trunc50$RACE_ETHNICITY_I)
table(m3_all_trunc50$INCOME_I)
table(m3_all_trunc50$job_type)
table(m3_all_trunc50$hhnum_cat)
table(bline_all_trunc50$dbs_result2)
table(m3_all_trunc50$m3_TAb)


# WEIGHTED
svytable(~AGE_I, bline_trunc50_s)
svytable(~GENDER_I, bline_trunc50_s)
svytable(~RACE_ETHNICITY_I, bline_trunc50_s)
svytable(~INCOME_I, bline_trunc50_s)
svytable(~job_type, bline_trunc50_s)
svytable(~hhnum_cat, bline_trunc50_s)
svytable(~dbs_result2, bline_trunc50_s)
svytable(~m3_TAb, m3_trunc50_s)


###
# contact matrices - RAW
bline_all_trunc50 %>%
  group_by(agecat_contact) %>%
  summarise("0-4" = mean(totcon0), "5-9" = mean(totcon5), "10-19" = mean(totcon10), "20-39" = mean(totcon20), "40-59" = mean(totcon40), "60-70" = mean(totcon60), "70+" = mean(totcon70)) -> t1

m3_all_trunc50 %>%
  group_by(agecat_contact) %>%
  summarise("0-4" = mean(totcon0_3m), "5-9" = mean(totcon5_3m), "10-19" = mean(totcon10_3m), "20-39" = mean(totcon20_3m), "40-59" = mean(totcon40_3m), "60-70" = mean(totcon60_3m), "70+" = mean(totcon70_3m)) -> t2

contact_table_bl <- knitr::kable(t1, digits = 3) %>%
  kable_styling(bootstrap_options = "striped", font_size = 14) %>%
  scroll_box(width = "1000px", height = "500px")

kableExtra::save_kable(contact_table_bl, file = paste0("rawcontacts_agematrix_baseline_06152021.html", sep=""))

contact_table_m3 <- knitr::kable(t2, digits = 3) %>%
  kable_styling(bootstrap_options = "striped", font_size = 14) %>%
  scroll_box(width = "1000px", height = "500px")

kableExtra::save_kable(contact_table_m3, file = paste0("rawcontacts_agematrix_m3_06152021.html", sep=""))


###
# Violin plots for contact

colors <- list(
  "blue"   = "#00798c",
  "red"    = "#d1495b",
  "yellow" = "#edae49",
  "green"  = "#66a182",
  "navy"   = "#2e4057", 
  "grey"   = "#8d96a3"
)


means.loc <- as_tibble(data.frame(ContactLocation = c("tothome", "totwork", "totschool", "totother"), Val = c(2.7, 10.4, 0.2, 2.7)))

bline_all_trunc50 %>% 
  ggplot() +
  geom_histogram(aes(x=tothome), binwidth = 1, colour="grey60", fill="white") +
  xlab(" ") +
  ylab("Frequency")  +
  scale_y_log10() +
  xlim(c(-1, 100)) +
  geom_vline(xintercept=2.7, linetype="dashed", color="#00798c") +
  annotate(geom="text", x=50, y=800, label="Home",
           color="black") +
  theme(panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
        axis.text=element_text(size=10), 
        axis.title=element_text(size=10), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(colour = "black", size = 10), 
        axis.text.y = element_text(colour = "black", size = 10),
        axis.line.x = element_line(colour = 'black', size=0.25, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.25, linetype='solid')) -> v.loc.bl.h

bline_all_trunc50 %>% 
  ggplot() +
  geom_histogram(aes(x=totwork), binwidth = 1, colour="grey60", fill="white") +
  xlab(" ") +
  ylab("Frequency")  +
  scale_y_log10() +
  xlim(c(-1, 100)) +
  geom_vline(xintercept=10.4, linetype="dashed", color="#00798c")+
  annotate(geom="text", x=50, y=800, label="Work",
           color="black") +
  theme(panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
        axis.text=element_text(size=10), 
        axis.title=element_text(size=10), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(colour = "black", size = 10), 
        axis.text.y = element_text(colour = "black", size = 10),
        axis.line.x = element_line(colour = 'black', size=0.25, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.25, linetype='solid')) -> v.loc.bl.w

bline_all_trunc50 %>% 
  ggplot() +
  geom_histogram(aes(x=totschool), binwidth = 1, colour="grey60", fill="white") +
  xlab(" ") +
  ylab("Frequency")  +
  scale_y_log10() +
  xlim(c(-1, 100)) +
  geom_vline(xintercept=0.2, linetype="dashed", color="#00798c") +
  annotate(geom="text", x=50, y=800, label="School",
           color="black") +
  theme(panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
        axis.text=element_text(size=10), 
        axis.title=element_text(size=10), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(colour = "black", size = 10), 
        axis.text.y = element_text(colour = "black", size = 10),
        axis.line.x = element_line(colour = 'black', size=0.25, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.25, linetype='solid')) -> v.loc.bl.s

bline_all_trunc50 %>% 
  ggplot() +
  geom_histogram(aes(x=totother), binwidth = 1, colour="grey60", fill="white") +
  xlab("Number of Contacts") +
  ylab("Frequency")  +
  scale_y_log10() +
  xlim(c(-1, 100)) +
  geom_vline(xintercept=2.7, linetype="dashed", color="#00798c")+
  annotate(geom="text", x=50, y=800, label="Other",
           color="black") +
  theme(panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
        axis.text=element_text(size=10), 
        axis.title=element_text(size=10), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(colour = "black", size = 10), 
        axis.text.y = element_text(colour = "black", size = 10),
        axis.line.x = element_line(colour = 'black', size=0.25, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.25, linetype='solid')) -> v.loc.bl.o


ggarrange(v.loc.bl.h, v.loc.bl.w, 
          v.loc.bl.s, v.loc.bl.o, nrow=4, ncol=1) -> fig1


m3_all_trunc50 %>% 
  select(tothome_3m, totwork_3m, totschool_3m, totother_3m) %>%
  gather(key="Contactlocation", value="Val") %>%
  ggplot( aes(x=Contactlocation, y=Val)) +
  geom_violin(adjust=3) +
  geom_jitter(alpha = 0.1, color = "grey60") +
  scale_x_discrete(labels = c('Home', 'Work', 'School', 'Other' )) +
  xlab("Contact Location") +
  ylab("Number of contacts") +
  scale_y_log10()  -> v.loc.m3


means.age <- as_tibble(data.frame(AGE_I = 1:6, Val = c(1, 2, 3, 4, 5, 6)))

ggplot(data = bline_all_trunc50, aes(factor(AGE_I), totcon_all), weight = WTFinal_US_M3_AbIGg) +
  geom_violin(adjust=3) +
  geom_jitter(alpha = 0.1, color = "grey60") +
  ylab("Number of contacts") + 
  xlab("Age") +
  scale_y_continuous(trans="log10", name="Number of contacts", breaks=c(0, 1, 10, 100)) +
  scale_x_discrete(labels = c('18 - 24', '25 - 34', '35 - 44', '45 - 54',  '55 - 64', '65+' )) +
  geom_point(data = means.age, aes(x=factor(AGE_I), y=Val)) +
  theme(panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
        axis.text=element_text(size=10), 
        axis.title=element_text(size=10), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(colour = "black", size = 10), 
        axis.text.y = element_text(colour = "black", size = 10),
        axis.line.x = element_line(colour = 'black', size=0.25, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.25, linetype='solid')) -> v.age.bl

ggplot(data = m3_all_trunc50, aes(factor(AGE_I), totcon_all_3m), weight = WTFinal_US_M3_AbIGg) +
  geom_violin(adjust=3) +
  geom_jitter(alpha = 0.1, color = "grey60") +
  ylab("") + 
  xlab("Age") +
  scale_x_discrete(labels = c('18 - 24', '25 - 34', '35 - 44', '45 - 54',  '55 - 64', '65+' )) +
  scale_y_log10() -> v.age.m3

ggplot(data = bline_all_trunc50, aes(factor(RACE_ETHNICITY_I), totcon_all), weight = WTFinal_US_M3_AbIGg) +
  geom_violin(adjust=3) +
  geom_jitter(alpha = 0.1, color = "grey60") +
  ylab("Number of contacts") + 
  xlab("Race/ethnicity") +
  scale_x_discrete(labels = c('Hispanic', 'N-H White', 'N-H Black', 'N-H Asian', 'N-H Other')) +
  scale_y_log10() -> v.race.bl

ggplot(data = m3_all_trunc50, aes(factor(RACE_ETHNICITY_I), totcon_all_3m), weight = WTFinal_US_M3_AbIGg) +
  geom_violin(adjust=3) +
  geom_jitter(alpha = 0.1, color = "grey60") +
  ylab("") + 
  xlab("Race/ethnicity") +
  scale_x_discrete(labels = c('Hispanic', 'N-H White', 'N-H Black', 'N-H Asian', 'N-H Other')) +
  scale_y_log10() -> v.race.m3


ggplot(data = bline_all_trunc50, aes(factor(INCOME_I), totcon_all), weight = WTFinal_US_M3_AbIGg) +
  geom_violin(adjust=3) +
  geom_jitter(alpha = 0.1, color = "grey60") +
  ylab("Number of contacts") + 
  xlab("Annual Income") +
  scale_x_discrete(labels = c('< 10k', '10-25k', '25-50k', '50-75k', '75-100k', '100-150k', '150-200k', '200k+')) +
  scale_y_log10() -> v.income.bl

ggplot(data = m3_all_trunc50, aes(factor(INCOME_I), totcon_all_3m), weight = WTFinal_US_M3_AbIGg) +
  geom_violin(adjust=3) +
  geom_jitter(alpha = 0.1, color = "grey60") +
  ylab("") + 
  xlab("Annual Income") +
  scale_x_discrete(labels = c('< 10k', '10-25k', '25-50k', '50-75k', '75-100k', '100-150k', '150-200k', '200k+')) +
  scale_y_log10() -> v.income.m3

ggplot(data = bline_all_trunc50, aes(factor(GENDER_I), totcon_all), weight = WTFinal_US_M3_AbIGg) +
  geom_violin(adjust=3) +
  geom_jitter(alpha = 0.1, color = "grey60") +
  ylab("Number of contacts") + 
  xlab("Gender") +
  scale_x_discrete(labels = c('Female','Male')) +
  scale_y_log10() -> v.gender.bl


ggplot(data = m3_all_trunc50, aes(factor(GENDER_I), totcon_all_3m), weight = WTFinal_US_M3_AbIGg) +
  geom_violin(adjust=3) +
  geom_jitter(alpha = 0.1, color = "grey60") +
  ylab("") + 
  xlab("Gender") +
  scale_x_discrete(labels = c('Female','Male')) +
  scale_y_log10() -> v.gender.m3

## also by hh size, serostatus?
ggplot(data = bline_all_trunc50, aes(factor(hhnum_cat), totcon_all), weight = WTFinal_US_M3_AbIGg) +
  geom_violin(adjust=3) +
  geom_jitter(alpha = 0.1, color = "grey60") +
  ylab("Number of contacts") + 
  xlab("Household size (not including respondent)") +
  scale_x_discrete(labels = c('1', '2', '3', '4', '5+')) +
  scale_y_log10() -> v.hhnum

ggplot(data = bline_all_trunc50, aes(factor(dbs_result2), totcon_all), weight = WTFinal_US_M3_AbIGg) +
  geom_violin(adjust=3) +
  geom_jitter(alpha = 0.1, color = "grey60") +
  ylab("Number of contacts") + 
  xlab("Serostatus at baseline") +
  scale_x_discrete(labels = c('Positive', 'Negative')) +
  scale_y_log10() -> v.sero.bl

ggplot(data = m3_all_trunc50, aes(factor(m3_TAb), totcon_all_3m), weight = WTFinal_US_M3_AbIGg) +
  geom_violin(adjust=3) +
  geom_jitter(alpha = 0.1, color = "grey60") +
  ylab("") + 
  xlab("Serostatus at follow-up") +
  scale_x_discrete(labels = c('Positive', 'Negative')) +
  scale_y_log10()  -> v.sero.m3


ggarrange(v.age.bl, v.age.m3, 
          v.gender.bl, v.gender.m3, 
          v.race.bl, v.race.m3, 
          v.income.bl, v.income.m3, 
          v.sero.bl, v.sero.m3, nrow=5, ncol=2) -> fig2.bothrounds


ggarrange(v.age.bl, 
          v.gender.bl, 
          v.race.bl, 
          v.income.bl, 
          v.sero.bl, nrow=5, ncol=1) -> fig2

ggsave(fig1, file='covidvusocialcon_fig1.pdf', width = 9, height = 12, units = "in",  dpi = 300)

ggsave(fig2, file='covidvusocialcon_fig2.pdf', width = 9, height = 12, units = "in",  dpi = 300)

ggsave(fig2.bothrounds, file='covidvusocialcon_fig2bothrounds.pdf', width = 9, height = 12, units = "in",  dpi = 300)

# HISTOGRAMS

ggplot(data = bline_m3, aes(x=totcon_all)) + 
  geom_histogram(binwidth=1, color="black", fill="white") +
  xlim(c(-1,75)) +
  ylim(c(-1,500))


ggplot(data = bline_m3, aes(x=totcon_all_3m)) + 
  geom_histogram(binwidth=1, color="black", fill="white") +
  xlim(c(-1,75)) +
  ylim(c(-1,500))


# by location - baseline

ggplot(data = bline_m3, aes(x=tothome)) + 
  geom_histogram(binwidth=1, color="black", fill="white") +
  xlim(c(-1,75)) +
  ylim(c(-1,500)) -> h.b

ggplot(data = bline_m3, aes(x=totwork)) + 
  geom_histogram(binwidth=1, color="black", fill="white") +
  xlim(c(-1,75)) +
  ylim(c(-1,500)) -> w.b

ggplot(data = bline_m3, aes(x=totschool)) + 
  geom_histogram(binwidth=1, color="black", fill="white") +
  xlim(c(-1,75)) +
  ylim(c(-1,500)) -> s.b

ggplot(data = bline_m3, aes(x=totother)) + 
  geom_histogram(binwidth=1, color="black", fill="white") +
  xlim(c(-1,75)) +
  ylim(c(-1,500)) -> o.b

# by location - 3mo

ggplot(data = bline_m3, aes(x=tothome_3m)) + 
  geom_histogram(binwidth=1, color="black", fill="white") +
  xlim(c(-1,75)) +
  ylim(c(-1,500))  -> h.3

ggplot(data = bline_m3, aes(x=totwork_3m)) + 
  geom_histogram(binwidth=1, color="black", fill="white") +
  xlim(c(-1,75)) +
  ylim(c(-1,500))  -> w.3

ggplot(data = bline_m3, aes(x=totschool_3m)) + 
  geom_histogram(binwidth=1, color="black", fill="white") +
  xlim(c(-1,75)) +
  ylim(c(-1,500))  -> s.3

ggplot(data = bline_m3, aes(x=totother_3m)) + 
  geom_histogram(binwidth=1, color="black", fill="white") +
  xlim(c(-1,75)) +
  ylim(c(-1,500))  -> o.3


ggarrange(h.b, w.b, s.b, o.b,
          h.3, w.3, s.3, o.3,
          nrow = 2, ncol = 4)


  