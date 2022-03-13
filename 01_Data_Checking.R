### Standardization of Data Cleaning with R
rm(list = ls())
today <- Sys.Date()

# load packages
library(tidyverse)
library(lubridate)
library(tidyr)
library(readxl)
library(openxlsx)
source("functions/cleaning_functions.R")

# initalize common variables
survey_start_date <- "1/1/2022"
survey_end_date <- "5/1/2022"

# load raw data
data <- read.csv("input/testdf.csv", stringsAsFactors = FALSE)
checklist <- readxl::read_excel("input/check_list.xlsx")
logbook <- as.data.frame(logbook())

## Standard Checks
# Check 1 - survey time taken
check_survey_time <- time_check(data, time_min = 15, time_max = 40) %>% 
  filter(CHECK_interview_duration != "Okay") %>% 
  log_sheet(question.name = "interview_duration", issue = "survey time filled out of range", action = "delete")
logbook <- rbind(logbook, check_survey_time)

# Check 2 - other responses
check_others <- check_other_responses(data)
logbook <- rbind(logbook, check_others)

# Check 3 - outlier checks



## Specific checks
# check 4 - ki age
check_age <- data %>% 
  filter(ki_age > 110) %>% log_sheet(question.name = "ki_age", issue = "please confirm the reported ki age", action = "flag")
logbook <- rbind(logbook, check_age)

# check 5 - income check



## Export logbood
write.xlsx(logbook, paste0("output/logbook_", today,".xlsx"))
