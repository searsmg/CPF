## processing script for 2022 snow surveys

library(dplyr)
library(lubridate)
library(plotly)
library(ggplot)
library(stringr)

#Plot size
PlotWidth = 16.5
PlotHeight = 9

## set wd where the csv is saved and where the output will go
setwd("C:/Users/sears/Documents/Research/CPF/Snowsurvey_2022/Feb2022")

## read in csv, get Date, select and rename columns
feb <- read.csv("survey_feb2022.csv") %>%
  mutate(Datetime = mdy_hms(Date.and.Time)) %>%
  mutate(Date = as.Date(Datetime)) %>%
  select(-c(1:6, 11, 27)) %>%
  rename(transect = Transect.Name,
         id = Point.ID,
         burn = Burn.status,
         burn_other = Other...Burn.status,
         depth1_cm = Depth.1..cm.,
         depth2_cm = Depth.2..cm.,
         depth3_cm = Depth.3..cm.,
         depth4_cm = Depth.4..cm.,
         depth5_cm = Depth.5..cm.,
         diam_cm = Snow.core.diameter..cm.,
         coredepth1_cm = Snow.core.1.depth..cm.,
         coreweight1_g = Snow.core.1.weight..g.,
         coredepth2_cm = Snow.core.2.depth..cm.,
         coreweight2_g = Snow.core.2.weight..g.,
         coredepth3_cm = Snow.core.3.depth..cm.,
         coreweight3_g = Snow.core.3.weight..g.)

## look at persistent data first
feb_pers <- feb %>%
  filter(transect == "Persistent")

## getting rid of partially and all burned and saying burned
feb_pers$burn <- str_replace_all(feb_pers$burn, "Partially_burned_some_needles", 
                               "Burned")

feb_pers$burn <- str_replace_all(feb_pers$burn, "All_burned_no_needles", 
                                 "Burned")

#average across for depths by point id
feb_pers <- feb_pers %>%
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

# PERS - look at variability in depth
PLOT = "feb_pers_d"
ggplot(feb_pers, aes(x=id, y=avg_depth_cm, color=burn)) +
  geom_point(size=5) +
  ggtitle("persistent")

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)


## PERS - looking at variability in swe
PLOT = "feb_pers_swe"
ggplot(feb_pers, aes(x=id, y=swe, color=burn)) +
  geom_point(size=5)+
  ggtitle("persistent")

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

## look at transitional
feb_trans <- feb %>%
  filter(transect %in% c("Transitional_burn", "Transitional_unburn"))

## getting rid of partially and all burned and saying burned
feb_trans$burn <- str_replace_all(feb_trans$burn, "Partially_burned_some_needles", 
                                 "Burned")

feb_trans$burn <- str_replace_all(feb_trans$burn, "All_burned_no_needles", 
                                 "Burned")

#average across for depths by point id
feb_trans <- feb_trans %>%
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
  

## TRANS - look at variability in depth
PLOT ="feb_trans_d"
ggplot(feb_trans, aes(x=id, y=avg_depth_cm, color=burn)) +
  geom_point(size=5)+
  ggtitle("transitional")

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)


## TRANS - looking at variability in swe
PLOT = "feb_trans_swe"
ggplot(feb_trans, aes(x=id, y=swe, color=burn)) +
  geom_point(size=5)+
  ggtitle("transitional")

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)


