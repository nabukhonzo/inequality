####coding basics####
2+3
#coding basic----

#### Load Libraries ####
library(tidyr)
library(dplyr)
library(janitor)
library(lubridate)
library(readxl)

#### Set working Directory ####
setwd('/Users/data')

#### Read in Data ####
bthsdata <- readxl::read_excel('births_data.xlsx', 
                               sheet = "Births_data", guess_max = 10000 )

#### Identify Duplicates and and store for investigation ####
bthsdata_duplicates <- bthsdata[duplicated(bthsdata),]

#### Remove Duplicates and empty rows ####
bthsdata <- bthsdata[!duplicated(bthsdata),]
bthsdata <- bthsdata[!apply(is.na(bthsdata) | bthsdata == "", 1, all),]

#### Correct dates with missing days and months ####
bthsdata <- bthsdata %>%
  mutate(DOB = case_when(
    as.character(substr(DOB, 5, 8)) == "0000" ~ as.numeric(paste0(substr(DOB, 1, 4), 0714)),
    as.character(substr(DOB, 5, 6)) == "00" ~ as.numeric(paste0(substr(DOB, 1, 4), 07, substr(DOB, 7, 8))),
    as.character(substr(DOB, 7, 8)) == "00" ~ as.numeric(paste0(substr(DOB, 1, 6) ,14)),
    T ~ as.numeric(DOB)))

bthsdata <- bthsdata %>%
  mutate(DOBM = case_when(
    as.character(substr(DOBM, 5, 8)) == "0000" ~ as.numeric(paste0(substr(DOBM, 1, 4), 0714)),
    as.character(substr(DOBM, 5, 6)) == "00" ~ as.numeric(paste0(substr(DOBM, 1, 4), 07, substr(DOBM, 7, 8))),
    as.character(substr(DOBM, 7, 8)) == "00" ~ as.numeric(paste0(substr(DOBM, 1, 6) ,14)),
    T ~ as.numeric(DOBM)))

bthsdata <- bthsdata %>%
  mutate(DOR = case_when(
    as.character(substr(DOR, 5, 8)) == "0000" ~ as.numeric(paste0(substr(DOR, 1, 4), 0714)),
    as.character(substr(DOR, 5, 6)) == "00" ~ as.numeric(paste0(substr(DOR, 1, 4), 07, substr(DOR, 7, 8))),
    as.character(substr(DOR, 7, 8)) == "00" ~ as.numeric(paste0(substr(DOR, 1, 6) ,14)),
    T ~ as.numeric(DOR)))

#### Derive new variables ####
### split Date of birth in to Year, Month and Day ###
bthsdata <- bthsdata %>%
  mutate(DOBYR = substr(DOB,1, 4)) %>%
  mutate(DOBMT = substr(DOB,5, 6)) %>%  
  mutate(DOBDY = substr(DOB,7, 8))

#### create 5 year age group ####
bthsdata <- bthsdata %>%
  mutate(AGEGROUP = cut(as.numeric(substr(AGEBM, 1, 2)), 
                        breaks = c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,Inf),
                        # c(0, 1, seq(5, 85, 5), Inf), 
                        labels = c("<1", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29", 
                                   "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", 
                                   "65-69", "70-74", "75-79", "80-84", "85+"),
                        right = F))

#### Calculate Registration Delays in days
bthsdata <- bthsdata %>%
  mutate(REGDELAY = lubridate::interval(lubridate::ymd(DOB), lubridate::ymd(DOR)) / days(1))


### frequency counts of sex of child by date of registration 
count_by_sex <- bthsdata %>%
  group_by(SEX, substr(DOR, 1, 4)) %>%
  summarise(COUNT = n())

#### read in Populations data to use to calculate rates ####
#### Read in CSV File ####
popsdata <- read.csv('populations2.csv', header = T, 
                     sep = ",", stringsAsFactors = F, na.strings = c("","NA"))

#### Derive 5 year agegroups ####
popsdata <- popsdata %>%
  mutate(`5YEARAGE`  = cut(as.numeric(Age), 
                           breaks = c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,Inf),
                           labels = c("<1", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29", 
                                      "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", 
                                      "65-69", "70-74", "75-79", "80-84", "85+"),
                           right = F))

#### creating populations table ####
pops_by_gor <- as.data.frame(popsdata %>%
                               filter(Areatype == 'REG' & Year == 2019 & Sex == 2) %>%
                               select(Popn, Sex, Code, `5YEARAGE`) %>%
                               group_by( Sex, Code, `5YEARAGE`) %>%
                               summarise(Popn = sum(Popn)))

#### creating births subset table ####
bths_subset <- as.data.frame(bthsdata %>%
                               filter(SOURCETABLE == 2019 & is.na(SBIND)) %>%
                               select( AGEGROUP, GOR) %>%
                               group_by( AGEGROUP, GOR) %>%
                               summarise(NUMOFBTHS = n()))

#### joining dataframes together ####
data <- right_join(pops_by_gor, bths_subset, by=c('Code'= 'GOR', 
                                                  `5YEARAGE` = 'AGEGROUP')) %>%
  replace(., is.na(.), 0)


#### calculating ASFR rate ####
data <- data %>%
  mutate(ASFR = (NUMOFBTHS/Popn))

#### calculating TFR for England ####
tfrdata <- as.data.frame(data %>%
                           filter(substr(Code,1,1) == 'E') )

tfrate <- as.data.frame(round(sum(tfrdata[, 'ASFR'])*5,2)) %>%
  rename('TFR' = "round(sum(tfrdata[, \"ASFR\"]) * 5, 2)")

