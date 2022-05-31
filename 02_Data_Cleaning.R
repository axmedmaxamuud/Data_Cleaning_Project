### Cleaning Data based on the data checks
rm(list = ls())
today <- Sys.Date()

# load packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(clog)

# load raw data and updated cleaning log files
data <- read.csv("input/testdf.csv")
clog <- read_excel("output/logbook_2022-03-02.xlsx")

# setting up cleang log file
my_cleaninglog <- cleaninglog(ids = clog$uuid,
                              variables = clog$question.name,
                              new_values = clog$new.value,
                              name = clog$feedback,
                              change = clog$action,
                              data_id_column_name = "uuid")

# change data based on the cleaninglog
clean_data <- clog_clean(data, my_cleaninglog)

# reverse cleaned data and produce cleaning log file
change_log <- clog_get_cleaninglog(clean_data)




