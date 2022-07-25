
library(tidyverse)
library(stringr)
library(lubridate)

setwd("N:/Research/Kampf/Private/field_data/bennett")

#pull in all filenames with composite
filenames <- list.files(".", pattern="composite", full.names=F)

#read all the csvs
dataframes <- lapply(filenames, read.csv)

#name all the csv
names(dataframes) <- substr(filenames, 1, 14)

#extract csvs from list to global env
lapply(names(dataframes), function(x) assign(x, dataframes[[x]], envir = .GlobalEnv))
rm(dataframes) #remove dataframes list

################################################
## precip
################################################

me_rain_compos2 <- me_rain_compos %>%
  mutate(site = "me") %>%
  rename(datetime = 1) %>%
  mutate(datetime = parse_date_time(datetime, 
                                  orders = c('%m/%d/%Y %H:%M',
                                             '%m/%d/%y %I:%M:%S %p')))

mm_rain_compos2 <- mm_rain_compos %>%
  mutate(site = "mm") %>%
  mutate(datetime = parse_date_time(datetime, 
                                    orders = c('%m/%d/%Y %H:%M',
                                               '%m/%d/%y %I:%M:%S %p')))
  
         
