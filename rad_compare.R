## comparing shortwave rad at several CPF weather stations

library(tidyverse)
library(dplyr)
library(lubridate)
library(plotly)
library(ggplot2)
library(RNRCS)


## pull in jw hourly met
jw_hr <- read.table(
  "C:/Users/sears/Documents/Research/CPF/Data_downloads/joewright_met_hr_20220227.dat",
  sep = ",", header=TRUE, skip="1") %>%
  slice(., -(1:2)) %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(site = 'jw')

## pull in tc hourly met
tc_hr <- read.table(
  "C:/Users/sears/Documents/Research/CPF/Data_downloads/tunnelcreek_met2_hr_20220227.dat",
  sep = ",", header=TRUE, skip="1")%>%
  slice(., -(1:2)) %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(site = 'tc')

## pull in mc hourly met
mc_hr <- read.csv("C:/Users/sears/Documents/Research/CPF/Data_downloads/mtncampus_20220223.csv") %>%
  mutate(TIMESTAMP = ymd_hm(TIMESTAMP)) %>%
  mutate(hour = hour(TIMESTAMP),
         yday = yday(TIMESTAMP)) %>%
  group_by(yday, hour) %>%
  summarize_all(list(mean)) %>%
  mutate(TIMESTAMP = floor_date(TIMESTAMP, "hour")) %>%
  mutate(site = 'mc')

## compare incoming SW between sites, put in 1 df
mc_tc <- bind_rows(mc_hr, tc_hr)

#bind all 3, then keep only rad data, filter to start on Nov1
mc_tc_jw <- bind_rows(mc_tc, jw_hr) %>%
  select(c(TIMESTAMP, SWin_Avg, SWout_Avg, 
           LWin_Avg, LWout_Avg,
           SWalbedo_Avg, site)) %>%
  filter(TIMESTAMP > ymd_hms("2021-11-01 00:00:00"))

## plot SW in 1 plot
ggplot(mc_tc_jw, aes(x = TIMESTAMP, y= SWin_Avg, color = site)) +
  geom_line(size=1)

## separate SW in plots
ggplot(mc_tc_jw, aes(x = TIMESTAMP, y= SWin_Avg)) +
  geom_line(size=1) +
  facet_wrap(~site)

## SW out as one plot
ggplot(mc_tc_jw, aes(x = TIMESTAMP, y= SWout_Avg, color = site)) +
  geom_line(size=1)

## separate SW out plots
ggplot(mc_tc_jw, aes(x = TIMESTAMP, y= SWout_Avg)) +
  geom_line(size=1) +
  facet_wrap(~site)

## plot albedo in 1 plot
ggplot(mc_tc_jw, aes(x = TIMESTAMP, y= SWalbedo_Avg, color = site)) +
  geom_line(size=1)

## plot albebo in 1 plot but limit the y vals
ggplot(mc_tc_jw, aes(x = TIMESTAMP, y= SWalbedo_Avg, color = site)) +
  geom_line(size=1) +
  ylim(0,1)

##plot albedo in 1 plot but limit the y vals
ggplot(mc_tc_jw, aes(x = TIMESTAMP, y= SWalbedo_Avg)) +
  geom_line(size=1) +
  ylim(0,1) +
  facet_wrap(~site)

## plot net SW in facet plots
ggplot(mc_tc_jw, aes(x = TIMESTAMP, y= SWin_Avg-SWout_Avg)) +
  geom_line(size=1) +
  facet_wrap(~site)


