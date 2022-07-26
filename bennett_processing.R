
library(tidyverse)
library(stringr)
library(lubridate)
library(plotly)

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

#me rain
me_rain <- me_rain_compos %>%
  mutate(site = "me") %>%
  rename(datetime = 1) %>%
  mutate(datetime = parse_date_time(datetime, 
                                  orders = c('%m/%d/%Y %H:%M',
                                             '%m/%d/%y %I:%M:%S %p')))
rm(me_rain_compos)

#mm rain
mm_rain <- mm_rain_compos %>%
  mutate(site = "mm") %>%
  mutate(datetime = ymd_hm(datetime))

rm(mm_rain_compos)
  
#mw rain
mw_rain <- mw_rain_compos %>%
  mutate(site = "mw") %>%
  mutate(datetime = parse_date_time(datetime, 
                                    orders = c('%m/%d/%Y %H:%M',
                                               '%Y-%m-%d %H:%M')))
rm(mw_rain_compos)

#ue rain
ue_rain <- ue_rain_compos %>%
  mutate(site = "ue") %>%
  mutate(datetime = parse_date_time(datetime, 
                                    orders = c('%m/%d/%Y %H:%M',
                                               '%m/%d/%y %I:%M:%S %p')))
rm(ue_rain_compos)

#um rain
um_rain <- um_rain_compos %>%
  mutate(site = "um") %>%
  mutate(datetime = mdy_hm(datetime))

rm(um_rain_compos)

#uw rain
uw_rain <- uw_rain_compos %>%
  mutate(site = "uw") %>%
  mutate(datetime = mdy_hm(datetime))
  
rm(uw_rain_compos)
  
#bind all rain data together
rain <- bind_rows(me_rain, mm_rain, mw_rain, ue_rain, um_rain, uw_rain)

rain_daily <- rain %>%
  mutate(precip_mm = 0.254,
         date = date(datetime)) %>%
  group_by(site, date) %>%
  summarize(dailyp_mm = sum(precip_mm)) 

rain_plot <- ggplot(rain_daily, aes(x=date, y=dailyp_mm, color=site)) +
  geom_point()

ggplotly(rain_plot)

################################################
## stage
################################################

#me stage
me_stage <- me_stage_compo %>%
  mutate(site = 'me',
         datetime = parse_date_time(datetime, 
                                    orders = c('%m/%d/%Y %H:%M',
                                               '%m-%d-%Y %H:%M:%S')))
rm(me_stage_compo)

#me_upper stage
me_upper_stage <- me_upper_stage %>%
  mutate(site = 'me_upper',
         datetime = mdy_hms(datetime))

#mm stage
mm_stage <- mm_stage_compo %>%
  mutate(site = 'mm',
         datetime = parse_date_time(datetime, 
                                    orders = c('%m/%d/%Y %H:%M',
                                               '%m/%d/%Y %H:%M:%S')))
rm(mm_stage_compo)

#mm upper stage
mm_upper_stage <- mm_upper_stage %>%
  mutate(site = 'mm_upper',
         datetime = mdy_hms(datetime))

#mw stage
mw_stage <- mw_stage_compo %>%
  mutate(site = 'mw',
         datetime = parse_date_time(datetime, 
                                    orders = c('%m/%d/%Y %H:%M',
                                               '%m/%d/%Y %H:%M:%S')))
rm(mw_stage_compo)

#mw_upper stage
mw_upper_stage <- mw_upper_stage %>%
  mutate(site = 'mw_upper',
         datetime = mdy_hms(datetime))

#ue stage
ue_stage <- ue_stage_compo %>%
  mutate(site = 'ue',
         datetime = parse_date_time(datetime, 
                                    orders = c('%m/%d/%Y %H:%M',
                                               '%m/%d/%Y %H:%M:%S')))
rm(ue_stage_compo)

#ue_upper stage (not downloaded yet)

#um stage
um_stage <- um_stage_compo %>%
  mutate(site = 'um',
         datetime = parse_date_time(datetime, 
                                    orders = c('%m/%d/%Y %H:%M',
                                               '%m/%d/%Y %H:%M:%S')))

rm(um_stage_compo)

#um_lower stage
um_lower_stage <- um_lower_stage %>%
  mutate(site = 'um_lower',
         datetime = parse_date_time(datetime, 
                                    orders = c('%m/%d/%Y %H:%M',
                                               '%m/%d/%Y %H:%M:%S')))
#uw stage
uw_stage <- uw_stage_compo %>%
  mutate(site = 'uw',
         datetime = parse_date_time(datetime, 
                                    orders = c('%m/%d/%Y %H:%M',
                                               '%m-%d-%Y %H:%M:%S')))

rm(uw_stage_compo)

#uw_upper stage (not downloaded yet)

stage <- bind_rows(me_stage, me_upper_stage, mm_stage, mm_upper_stage,
                   mw_stage, mw_upper_stage, ue_stage, um_stage, um_lower_stage,
                   uw_stage)

stage_plot <- ggplot(stage, aes(x=datetime, y=Stage_mm, color=site)) +
  geom_line()

ggplotly(stage_plot)
