
library(tidyverse)
library(dplyr)
library(readxl)
library(readr)



snap_f20 <- read_excel("Enrollment Snapshot_202009_Census_9-4-20.xlsx", 
                       sheet = "Sheet2")
snap_s21 <- read_excel("Enrollment Snapshot_202101_census_2-5-21.xlsx",
                       sheet = "Enrollment Snapshot")
snap_f21 <- read_excel("Enrollment Snapshot_202109_Census_09-03-21.xlsx",
                       sheet = "Enrollment Snapshot")
snap_s22 <- read_excel("Enrollment Snapshot 202201 Census 01 28 22.xlsx",
                       sheet = "Enrollment Snapshot")
snap_f22 <- read_excel("Enrollment Snapshot_202209_CENSUS_09-09-22.xlsx",
                       sheet = "Enrollment Snapshot")
snap_s23 <- read_excel("Enrollment Snapshot 202301 CENSUS 1-27-23.xlsx",
                       sheet = "Enrollment Snapshot")
snap_f23 <- read_excel("Enrollment Snapshot 202309 Week 29 20 Jul 2023.xlsx",
                       sheet = "Enrollment Snapshot")
hs_gpa <- read_excel("hsGPA.xlsx")

master2 <- read_csv("master2.csv")






temp <- snap_f20 %>% 
  filter(`COHORT TERM` == "202009" & `COHORT CODE` == "NF")
poo <- snap_s21 %>%
  filter(`COHORT TERM` == "202009" & `COHORT CODE` == "NF") %>%
  select(ID, GPA) %>%
  left_join(x = temp, y = ., by = c(ID = 'ID'))

temp2 <- snap_f21 %>% 
  filter(`COHORT TERM`== "202109" & `COHORT CODE` == "NF")
poo2 <- snap_s22 %>%
  filter(`COHORT TERM` == "202109" & `COHORT CODE` == "NF") %>%
  select(ID, GPA) %>%
  left_join(x = temp2, y = ., by = c(ID = 'ID'))

temp3 <- snap_f22 %>% 
  filter(`COHORT TERM`== "202209" & `COHORT CODE` == "NF")
poo3 <-snap_s23 %>%
  filter(`COHORT TERM` == "202209" & `COHORT CODE` == "NF") %>%
  select(ID, GPA) %>% 
  left_join(x = temp3, y = ., by = c(ID = 'ID'))



snap_baby <- bind_rows(
  poo %>% 
    filter(`COHORT TERM` == "202009" & `COHORT CODE`=="NF") %>% 
    select(`COHORT TERM`,ID,GENDER,`REPORT ETHNICITY`,RELIGION,CITIZENSHIP,`COMMUTER DORM`,`TERM CREDIT HRS`,`TRANSFER HRS`,GPA.x,`COLLEGE CODE`,`MAJOR CODE 1`,`ATHLETIC CODE`,`HOME STATE`,`HOME ZIP`,`ATRIB USCH DESC`,`HOME COUNTRY`,`CURRENT STREET2`,`CURRENT ZIP`,`INST AID AMT`,`NET REVENUE`, GPA.y),
  poo2 %>% 
    filter(`COHORT TERM` == "202109" & `COHORT CODE`=="NF") %>% 
    select(`COHORT TERM`,ID,GENDER,`REPORT ETHNICITY`,RELIGION,CITIZENSHIP,`COMMUTER DORM`,`TERM CREDIT HRS`,`TRANSFER HRS`,GPA.x,`COLLEGE CODE`,`MAJOR CODE 1`,`ATHLETIC CODE`,`HOME STATE`,`HOME ZIP`,`ATRIB USCH DESC`,`HOME COUNTRY`,`CURRENT STREET2`,`CURRENT ZIP`,`INST AID AMT`,`NET REVENUE`, GPA.y),
  poo3 %>% 
    filter(`COHORT TERM` == "202209" & `COHORT CODE`=="NF") %>% 
    select(`COHORT TERM`,ID,GENDER,`REPORT ETHNICITY`,RELIGION,CITIZENSHIP,`COMMUTER DORM`,`TERM CREDIT HRS`,`TRANSFER HRS`,GPA.x,`COLLEGE CODE`,`MAJOR CODE 1`,`ATHLETIC CODE`,`HOME STATE`,`HOME ZIP`,`ATRIB USCH DESC`,`HOME COUNTRY`,`CURRENT STREET2`,`CURRENT ZIP`,`INST AID AMT`,`NET REVENUE`, GPA.y)
)


snap_toddler <-hs_gpa %>% 
  select(-`Transcript GPA`) %>% 
  left_join(x=snap_baby,y=.,by=c(ID='Banner ID')) %>% 
  rename(`HS GPA` =`Recalculated GPA`)


snap_master <- bind_rows(
  snap_toddler %>% 
    filter(`COHORT TERM`=="202009") %>% 
    mutate(ret_spring = (ID %in% snap_s21$ID )) %>% 
    mutate(ret_fall = (ID %in% snap_f21$ID )),  
  snap_toddler %>% 
    filter(`COHORT TERM`=="202109") %>% 
    mutate(ret_spring = (ID %in% snap_s22$ID)) %>% 
    mutate(ret_fall = (ID %in% snap_f22$ID)) , 
  snap_toddler %>% 
    filter(`COHORT TERM`=="202209") %>% 
    mutate(ret_spring = (ID %in% snap_s23$ID)) %>% 
    mutate(ret_fall = (ID %in% snap_f23$ID)))



snap_master <- master2 %>% 
  select(fall_SI, fall_Coaching_Appts, `Fall At-Risk Progress Reports`, `Fall Tutoring`, PTO, `Withdrawal Form Reason`, withdrawal_date, BID...1) %>% 
  left_join(x = snap_master, y = ., by = c(ID = "BID...1"))


snap_master$`COHORT TERM` <- as.factor(snap_master$`COHORT TERM`)
snap_master$GENDER <- as.factor(snap_master$GENDER)
snap_master$`REPORT ETHNICITY` <- as.factor(snap_master$`REPORT ETHNICITY`)
snap_master$RELIGION <- as.factor(snap_master$RELIGION)
snap_master$CITIZENSHIP <- as.factor(snap_master$CITIZENSHIP)
snap_master$`COMMUTER DORM` <- as.factor(snap_master$`COMMUTER DORM`)
snap_master$`COLLEGE CODE` <- as.factor(snap_master$`COLLEGE CODE`)
snap_master$`MAJOR CODE 1`<- as.factor(snap_master$`MAJOR CODE 1`)
snap_master$`ATHLETIC CODE` <- as.factor(snap_master$`ATHLETIC CODE`)
snap_master$`HOME STATE` <- as.factor(snap_master$`HOME STATE`)
snap_master$`HOME ZIP` <- as.factor(snap_master$`HOME ZIP`)
snap_master$`ATRIB USCH DESC` <- as.factor(snap_master$`ATRIB USCH DESC`)
snap_master$`HOME COUNTRY` <- as.factor(snap_master$`HOME COUNTRY`)
snap_master$`CURRENT STREET2` <- as.factor(snap_master$`CURRENT STREET2`)
snap_master$`CURRENT ZIP` <- as.factor(snap_master$`CURRENT ZIP`)
snap_master$`HS GPA` <- as.numeric(snap_master$`HS GPA`)
snap_master$`Withdrawal Form Reason` <- as.factor(snap_master$`Withdrawal Form Reason`)
snap_master$PTO <- as.factor(snap_master$PTO)

snap_master <- snap_master %>% 
  mutate(Ethnicity = recode(`REPORT ETHNICITY`, 
                            'American Indian or Alaskan Native' = 'other',
                            'Indian or Alaskan Native' = 'other',
                            'Asian' = 'other',
                            'More than 1 race' = 'other',
                            'Native Hawaiian or other Pacific Islander' = 'other',
                            'Non-resident alien' = 'other'))

snap_master <- snap_master %>% 
  mutate(gpa_buckets = cut(GPA.y,
                           breaks = c(-0.01,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4)
                          ))

snap_master <- snap_master %>% 
  mutate(religion = recode(RELIGION,
                            'Baptist' = 'Other Christian',
                            'Church of Christ' = 'Other Christian',
                            'Congregational' = 'Other Christian',
                            'Disciples of Christ'  = 'Other Christian',
                            'Episcopalian/Anglican' = 'Other Christian',
                            'Lutheran' = 'Other Christian',
                            'Methodist' = 'Other Christian',
                            'Orthodox Catholic' = 'Other Christian',
                            'Other Protestant' = 'Other Christian',
                            'Presbyterian'= 'Other Christian',
                            'Unitarian' = 'Other Christian',
                            'Buddhist' = 'Other',
                            'Hindu' = 'Other',
                            'Islam' = 'Other',
                            'Jewish' = 'Other',
                            'NA' = 'Refused'
                            ))
  
snap_master <- snap_master %>% 
  mutate(hsgpa_buckets = cut(`HS GPA`,
                           breaks = c(2,3,3.25,3.5,3.75,4,4.25,4.5,4.75,6)
  ))

save(snap_master, file='snapmaster.Rda')

