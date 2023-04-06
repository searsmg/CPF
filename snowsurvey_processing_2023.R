## processing script for 2023 snow surveys

## packages
library(tidyverse)
library(lubridate)
library(plotly)
library(ggplot2)
library(stringr)

## Plot size
PlotWidth = 16.5
PlotHeight = 9

## set wd where the csv is saved and where the output will go
setwd("/Users/megansears/Documents/CPF/Snowsurvey_2023")

## read in geode data - this will be the same every survey
geode_all <- read_csv(
  "/Users/megansears/Documents/CPF/Snowsurvey_2022/geode_master_2022.csv") %>%
  rename(id = Name) %>%
  select(id, aspect, elev_m) #later we can keep lat/long for spatial stuff

## read in csv from snow survey by month, get Date, select and rename columns
# don't use x and y in this df bc it is NOT from the geode
jan <- read_csv("/Users/megansears/Documents/CPF/Snowsurvey_2023/jan/survey_jan2023.csv") %>%
  mutate(Datetime = mdy_hms(`Date and Time`)) %>%
  mutate(Date = as.Date(Datetime)) %>%
  select(-c(1:6, 10:11, 27)) %>%
  rename(transect = `Transect Name`,
         id = `Point ID`,
         burn = `Burn status`,
         depth1_cm = `Depth 1 (cm)`,
         depth2_cm = `Depth 2 (cm)`,
         depth3_cm = `Depth 3 (cm)`,
         depth4_cm = `Depth 4 (cm)`,
         depth5_cm = `Depth 5 (cm)`,
         diam_cm = `Snow core diameter (cm)`,
         coredepth1_cm = `Snow core 1 depth (cm)`,
         coreweight1_g = `Snow core 1 weight (g)`,
         coredepth2_cm = Snow.core.2.depth..cm.,
         coreweight2_g = Snow.core.2.weight..g.,
         coredepth3_cm = Snow.core.3.depth..cm.,
         coreweight3_g = Snow.core.3.weight..g.) %>%
  mutate(month = 1)

# adding aspect and elev to feb
jan_all <- left_join(jan, geode_all, by = "id")

all <- jan_all

# mar <- read.csv("D:/CPF/Snowsurvey_2022/Mar2022/survey_mar2022.csv") %>%
#   mutate(Datetime = mdy_hms(Date.and.Time)) %>%
#   mutate(Date = as.Date(Datetime)) %>%
#   select(-c(1:6, 11, 27)) %>%
#   rename(transect = Transect.Name,
#          id = Point.ID,
#          burn = Burn.status,
#          burn_other = Other...Burn.status,
#          depth1_cm = Depth.1..cm.,
#          depth2_cm = Depth.2..cm.,
#          depth3_cm = Depth.3..cm.,
#          depth4_cm = Depth.4..cm.,
#          depth5_cm = Depth.5..cm.,
#          diam_cm = Snow.core.diameter..cm.,
#          coredepth1_cm = Snow.core.1.depth..cm.,
#          coreweight1_g = Snow.core.1.weight..g.,
#          coredepth2_cm = Snow.core.2.depth..cm.,
#          coreweight2_g = Snow.core.2.weight..g.,
#          coredepth3_cm = Snow.core.3.depth..cm.,
#          coreweight3_g = Snow.core.3.weight..g.) %>%
#   mutate(month = 3)
# 
# # adding aspect and elev to mar
# mar_all <- left_join(mar, geode_all, by = "id")
# 
# # bind feb and mar together
# all <- bind_rows(feb_all, mar_all)
# 
# # add in the april data 
# apr <- read.csv("D:/CPF/Snowsurvey_2022/Apr2022/survey_apr2022.csv") %>%
#   mutate(Datetime = mdy_hms(Date.and.Time)) %>%
#   mutate(Date = as.Date(Datetime)) %>%
#   select(-c(1:6, 11, 27)) %>%
#   rename(transect = Transect.Name,
#          id = Point.ID,
#          burn = Burn.status,
#          burn_other = Other...Burn.status,
#          depth1_cm = Depth.1..cm.,
#          depth2_cm = Depth.2..cm.,
#          depth3_cm = Depth.3..cm.,
#          depth4_cm = Depth.4..cm.,
#          depth5_cm = Depth.5..cm.,
#          diam_cm = Snow.core.diameter..cm.,
#          coredepth1_cm = Snow.core.1.depth..cm.,
#          coreweight1_g = Snow.core.1.weight..g.,
#          coredepth2_cm = Snow.core.2.depth..cm.,
#          coreweight2_g = Snow.core.2.weight..g.,
#          coredepth3_cm = Snow.core.3.depth..cm.,
#          coreweight3_g = Snow.core.3.weight..g.) %>%
#   mutate(month = 4)
# 
# apr_all <- left_join(apr, geode_all, by = "id")
# 
# all <- bind_rows(all, apr_all)
# 
# #add in May
# may <- read.csv("D:/CPF/Snowsurvey_2022/May2022/survey_may2022.csv") %>%
#   mutate(Datetime = mdy_hms(Date.and.Time)) %>%
#   mutate(Date = as.Date(Datetime)) %>%
#   select(-c(1:6, 11, 27)) %>%
#   rename(transect = Transect.Name,
#          id = Point.ID,
#          burn = Burn.status,
#          burn_other = Other...Burn.status,
#          depth1_cm = Depth.1..cm.,
#          depth2_cm = Depth.2..cm.,
#          depth3_cm = Depth.3..cm.,
#          depth4_cm = Depth.4..cm.,
#          depth5_cm = Depth.5..cm.,
#          diam_cm = Snow.core.diameter..cm.,
#          coredepth1_cm = Snow.core.1.depth..cm.,
#          coreweight1_g = Snow.core.1.weight..g.,
#          coredepth2_cm = Snow.core.2.depth..cm.,
#          coreweight2_g = Snow.core.2.weight..g.,
#          coredepth3_cm = Snow.core.3.depth..cm.,
#          coreweight3_g = Snow.core.3.weight..g.) %>%
#   mutate(month = 5)
# 
# may_all <- left_join(may, geode_all, by = "id")
# 
# all <- bind_rows(all, may_all)

#############################################################################

## look at transitional data first
all_pers <- all %>%
  filter(transect == "Persistent")

## define aspect dir
all_pers <- all_pers %>%
  mutate(aspect_dir = case_when(
    between(aspect, 0, 22.5) ~"North",
    between(aspect, 22.5, 67.5) ~ "Northeast",
    between(aspect, 67.5, 112.5) ~ "East",
    between(aspect, 112.5, 157.5) ~ "Southeast",
    between(aspect, 157.5, 202.5) ~ "South",
    between(aspect, 202.5, 247.5) ~ "Southwest",
    between(aspect, 247.5, 292.5) ~ "West",
    between(aspect, 292.5, 337.5) ~ "Northwest",
    between(aspect, 337.5, 360) ~ "North"))

## getting rid of partially and all burned and saying burned
all_pers$burn <- str_replace_all(all_pers$burn, "Partially_burned_some_needles", 
                               "Burned")

all_pers$burn <- str_replace_all(all_pers$burn, "All_burned_no_needles", 
                                 "Burned")

## average across for depths by point id
all_pers <- all_pers %>%
  rowwise() %>%
  mutate(avg_depth_cm = mean(c(depth1_cm,
                               depth2_cm,
                               depth3_cm,
                               depth4_cm,
                               depth5_cm),
                               na.rm=T),
         avg_weight_g = mean(c(coreweight1_g,
                              coreweight2_g,
                              coreweight3_g)),
         avg_cordepth_cm = mean(c(coredepth1_cm,
                                  coredepth2_cm,
                                  coredepth3_cm))) %>%
  mutate(vol = pi*(diam_cm/2)^2*avg_cordepth_cm,
         dens = avg_weight_g / vol,
         swe = dens * avg_cordepth_cm)

#get rid of stuff we don't care about right now
all_pers_sum <- all_pers %>%
  select(transect, id, burn, Date, month, elev_m,
         aspect_dir, avg_depth_cm, dens, swe)

## PERS - look at variability in depth
PLOT = "all_pers_d"
ggplot(all_pers_sum, aes(x=id, y=avg_depth_cm, color=burn, shape=aspect_dir, label=month)) +
  geom_point(size=5) +
  ggtitle("persistent") + 
  geom_text(hjust=0, vjust=2)

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

## PERS - looking at variability in swe
PLOT = "all_pers_swe"
ggplot(all_pers_sum, aes(x=id, y=swe, color=burn, shape=aspect_dir, label=month)) +
  geom_point(size=5)+
  ggtitle("persistent") +
  geom_text(hjust=0, vjust=2)

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

PLOT = "depth_monthfacet"
ggplot(all_pers_sum, aes(x=id, y=avg_depth_cm, color=burn, shape=aspect_dir)) +
  geom_point(size=5) +
  ggtitle("persistent") + 
  facet_wrap(~month)

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

PLOT = "swe_monthfacet"
ggplot(all_pers_sum, aes(x=id, y=swe, color=burn, shape=aspect_dir)) +
  geom_point(size=5) +
  ggtitle("persistent") + 
  facet_wrap(~month)

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

#boxplots of burned vs unburned by month
PLOT = "pers_depth_boxplot"
ggplot(all_pers_sum, aes(x=as.factor(month), y=avg_depth_cm, fill=burn)) +
  geom_boxplot() +
  ggtitle("persistent")

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

PLOT = "pers_swe_boxplot"
ggplot(all_pers_sum, aes(x=as.factor(month), y=swe, fill=burn)) +
  geom_boxplot() +
  ggtitle("persistent")

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

#############################################################################

## look at transitional
all_trans <- all %>%
  filter(transect %in% c("Transitional_burn", "Transitional_unburn"))

## merge in the geode lat long then merge, rename aspect dirs
all_trans <- all_trans %>%
  mutate(aspect_dir = case_when(
    between(aspect, 0, 22.5) ~"North",
    between(aspect, 22.5, 67.5) ~ "Northeast",
    between(aspect, 67.5, 112.5) ~ "East",
    between(aspect, 112.5, 157.5) ~ "Southeast",
    between(aspect, 157.5, 202.5) ~ "South",
    between(aspect, 202.5, 247.5) ~ "Southwest",
    between(aspect, 247.5, 292.5) ~ "West",
    between(aspect, 292.5, 337.5) ~ "Northwest",
    between(aspect, 337.5, 360) ~ "North"))

## getting rid of partially and all burned and saying burned
all_trans$burn <- str_replace_all(all_trans$burn, "Partially_burned_some_needles", 
                                 "Burned")

all_trans$burn <- str_replace_all(all_trans$burn, "All_burned_no_needles", 
                                 "Burned")

## average across for depths by point id
all_trans <- all_trans %>%
  rowwise() %>%
  mutate(avg_depth_cm = mean(c(depth1_cm,
                               depth2_cm,
                               depth3_cm,
                               depth4_cm,
                               depth5_cm),
                             na.rm=T),
         avg_weight_g = mean(c(coreweight1_g,
                             coreweight2_g,
                             coreweight3_g)),
         avg_cordepth_cm = mean(c(coredepth1_cm,
                                coredepth2_cm,
                                coredepth3_cm))) %>%
  mutate(vol = pi*(diam_cm/2)^2*avg_cordepth_cm,
         dens = avg_weight_g / vol,
         swe = dens * avg_cordepth_cm)

#get rid of stuff we don't care about right now
all_trans_sum <- all_trans %>%
  select(transect, id, burn, Date, month, elev_m,
         aspect_dir, avg_depth_cm, dens, swe)

## TRANS - look at variability in depth
PLOT = "all_trans_d"
ggplot(all_trans_sum, aes(x=id, y=avg_depth_cm, color=burn, shape=aspect_dir, label=month)) +
  geom_point(size=5) +
  ggtitle("transitional") + 
  geom_text(hjust=0, vjust=2)

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

## TRANS - looking at variability in swe
PLOT = "all_trans_swe"
ggplot(all_trans_sum, aes(x=id, y=swe, color=burn, shape=aspect_dir, label=month)) +
  geom_point(size=5)+
  ggtitle("transitional") +
  geom_text(hjust=0, vjust=2)

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

## trans depth by month facet
PLOT = "depth_monthfacet_trans"
ggplot(all_trans_sum, aes(x=id, y=avg_depth_cm, color=burn, shape=aspect_dir)) +
  geom_point(size=5) +
  ggtitle("transitional") + 
  facet_wrap(~month)

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

## trans swe by month facet
PLOT = "swe_monthfacet_trans"
ggplot(all_trans_sum, aes(x=id, y=swe, color=burn, shape=aspect_dir)) +
  geom_point(size=5) +
  ggtitle("transitional") + 
  facet_wrap(~month)

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

#boxplots of burned vs unburned by month
PLOT = "trans_depth_boxplot"
ggplot(all_trans_sum, aes(x=as.factor(month), y=avg_depth_cm, fill=burn)) +
  geom_boxplot() + 
  ggtitle("transitional")

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

PLOT = "trans_swe_boxplot"
ggplot(all_trans_sum, aes(x=as.factor(month), y=swe, fill=burn)) +
  geom_boxplot() +
  ggtitle("transitional")

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

