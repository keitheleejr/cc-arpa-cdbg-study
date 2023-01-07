# load libraries
library(tidyverse)
library(scales)
library(readxl)
library(ggthemes)


# load data
colnames <- as.character(read_excel("qualtrics-data.xlsx", 
                                    na = "",
                                    n_max = 1, 
                                    col_names = FALSE)) # creates data frame of column names

qualtrics_data <- read_excel("qualtrics-data.xlsx", 
                             skip = 2, # skips variable names and questions
                             col_names = colnames) %>% # merges imported data with the column names above
  filter(Status == "IP Address") # filter out SPAM and Survey Preview

# age

qualtrics_data <- qualtrics_data %>% 
  mutate(age = case_when(
    grepl("[^0-9]", age) ~ "NA", # [^0-9] means "any character except the digits 0 - 9
    TRUE ~ age
  )) %>% 
  mutate(age = as.numeric(age)) 

qualtrics_data %>% 
  filter(age > 0) %>% 
  summarize(mean_age = mean(age),
            median_age = median(age),
            range_age = range(age),
            sd_age = sd(age),
            n())

# housing

table(qualtrics_data$housing)
table(qualtrics_data$housing)/sum(table(qualtrics_data$housing))*100

# residents

qualtrics_data$residents <- as.numeric(qualtrics_data$residents)
table(qualtrics_data$residents)
table(qualtrics_data$residents)/sum(table(qualtrics_data$residents))*100

# under18

table(qualtrics_data$under18)
table(qualtrics_data$under18)/sum(table(qualtrics_data$under18))*100

# under18count

table(qualtrics_data$under18count)
table(qualtrics_data$under18count)/sum(table(qualtrics_data$under18count))*100


# singleparent

table(qualtrics_data$singleparent)
table(qualtrics_data$singleparent)/sum(table(qualtrics_data$singleparent))*100

# over65

table(qualtrics_data$over65)
table(qualtrics_data$over65)/sum(table(qualtrics_data$over65))*100


# over65count

table(qualtrics_data$over65count)
table(qualtrics_data$over65count)/sum(table(qualtrics_data$over65count))*100

# gfi1 - gfi8

qualtrics_data <- qualtrics_data %>% 
  mutate(lmi = coalesce(gfi1,gfi2,gfi3,gfi4,gfi5,gfi6,gfi7,gfi8)) %>% 
  mutate(lmi_logical = case_when(
    grepl("Greater", lmi) ~ "False", 
    grepl("Less Than",lmi) ~ "True"
  ))

qualtrics_data %>% 
  select(lmi,lmi_logical)

table(qualtrics_data$lmi_logical)/sum(table(qualtrics_data$lmi_logical))*100

map_area_six <- qualtrics_data %>% 
  filter(grepl("6",mapnumber)) %>% 
  select(mapnumber, lmi, lmi_logical)

table(map_area_six$lmi_logical)/sum(table(map_area_six$lmi_logical))*100


# gfi2022
# 
# non-sense; should have used brackets
#
# gsub("[$,]","",qualtrics_data$gfi2022)
# qualtrics_data$gfi_2022_expected <- as.numeric(qualtrics_data$gfi2022)

# employaffected

table(qualtrics_data$employaffected)
table(qualtrics_data$employaffected)/sum(table(qualtrics_data$employaffected))*100

# tempreducthours

table(qualtrics_data$tempreducthours)
table(qualtrics_data$tempreducthours)/sum(table(qualtrics_data$tempreducthours))*100

# permreducthours

table(qualtrics_data$permreducthours)
table(qualtrics_data$permreducthours)/sum(table(qualtrics_data$permreducthours))*100

# tempoow

table(qualtrics_data$tempoow)
table(qualtrics_data$tempoow)/sum(table(qualtrics_data$tempoow))*100

# permoow

table(qualtrics_data$permoow)
table(qualtrics_data$permoow)/sum(table(qualtrics_data$permoow))*100

# workremote

table(qualtrics_data$workremote)
table(qualtrics_data$workremote)/sum(table(qualtrics_data$workremote))*100

# howfoundwork

table(qualtrics_data$howfoundwork)
table(qualtrics_data$howfoundwork)/sum(table(qualtrics_data$howfoundwork))*100

# childcare

table(qualtrics_data$childcare)
table(qualtrics_data$childcare)/sum(table(qualtrics_data$childcare))*100

# services

table(qualtrics_data$services)
table(qualtrics_data$services)/sum(table(qualtrics_data$services))*100

# servicesfirsttime

table(qualtrics_data$servicesfirsttime)
table(qualtrics_data$servicesfirsttime)/sum(table(qualtrics_data$servicesfirsttime))*100

# covidvaccineavail

table(qualtrics_data$covidvaccineavail)
table(qualtrics_data$covidvaccineavail)/sum(table(qualtrics_data$covidvaccineavail))*100

# internetaccess

table(qualtrics_data$internetaccess)
table(qualtrics_data$internetaccess)/sum(table(qualtrics_data$internetaccess))*100

# internetaccesshome

table(qualtrics_data$internetaccesshome)
table(qualtrics_data$internetaccesshome)/sum(table(qualtrics_data$internetaccesshome))*100

# internetaccesout

table(qualtrics_data$internetaccesout)
table(qualtrics_data$internetaccesout)/sum(table(qualtrics_data$internetaccesout))*100

# foodsecurity

table(qualtrics_data$foodsecurity)
table(qualtrics_data$foodsecurity)/sum(table(qualtrics_data$foodsecurity))*100
