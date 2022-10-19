
library(tidyverse)
library(stringr)
library(lubridate)
library(plotly)
library(Rainmaker)

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
                                             '%m/%d/%y %I:%M:%S %p',
                                             '%m-%d-%y %H:%M',
                                             '%Y-%m-%d %H:%M')),
         precip_mm = 0.254)

rm(me_rain_compos)

#mm rain
mm_rain <- mm_rain_compos %>%
  mutate(site = "mm") %>%
  mutate(datetime = ymd_hm(datetime),
         precip_mm = 0.254)

rm(mm_rain_compos)
  
#mw rain
mw_rain <- mw_rain_compos %>%
  mutate(site = "mw") %>%
  mutate(datetime = parse_date_time(datetime, 
                                    orders = c('%m/%d/%Y %H:%M',
                                               '%Y-%m-%d %H:%M')),
         precip_mm = 0.254)

rm(mw_rain_compos)

#ue rain
ue_rain <- ue_rain_compos %>%
  mutate(site = "ue") %>%
  mutate(datetime = parse_date_time(datetime, 
                                    orders = c('%m/%d/%Y %H:%M',
                                               '%m/%d/%y %I:%M:%S %p',
                                               '%Y-%m-%d %H:%M')),
         precip_mm = 0.254)

rm(ue_rain_compos)

#um rain
um_rain <- um_rain_compos %>%
  mutate(site = "um") %>%
  mutate(datetime = mdy_hm(datetime),
         precip_mm = 0.254)

rm(um_rain_compos)

#uw rain
uw_rain <- uw_rain_compos %>%
  mutate(site = "uw") %>%
  mutate(datetime = parse_date_time(datetime, 
                                    orders = c('%m/%d/%Y %H:%M',
                                               '%Y-%m-%d %H:%M')),
         precip_mm = 0.254)
  
rm(uw_rain_compos)
  
#bind all rain data together
rain <- bind_rows(me_rain, mm_rain, mw_rain, ue_rain, um_rain, uw_rain)

rain_daily <- rain %>%
  mutate(date = date(datetime)) %>%
  group_by(site, date) %>%
  summarize(dailyp_mm = sum(precip_mm)) 

rain_plot <- ggplot(rain_daily, aes(x=date, y=dailyp_mm, color=site)) +
  geom_point() + 
  ylim(0,50)

ggplotly(rain_plot)

## need to write to csv so can be used to determine rainfall metrics
write.csv(rain, 'bennett_all_rain.csv')
write.csv(rain_daily, 'rain_daily.csv')

################################################
## rainfall metrics
###############################################

#get events
me_events <- RMprep(me_rain, prep.type = 2, date.type = 2,
                tz = 'MST')


me_events <- RMevents(me_events, ieHr = 6,
                      rainthresh = .254,
                      rain = 'precip_mm',
                      time = 'datetime')

me_events <- me_events$storms2

me_events <- me_events %>%
  mutate(duration_hr = as.numeric((EndDate - StartDate) / 60 / 60),
         StartDate = as.numeric(StartDate),
         EndDate = as.numeric(EndDate)) 

#####################################################
#resample each event to 5, 15, 30, 60 min intervals

intensity <- function(df_event, duration_hr, start = 'start', end,
                      df_precip, datetime, P_mm) {
  for (i in 1:nrow(df_event)) {
    t5_rows=as.numeric(round(df_event[i,duration_hr]*60/5))+1
    start=as.numeric(df_event[i,start])
    end=as.numeric(df_event[i,end])
    t5_time=as.data.frame(seq(from=start,to=end,by=300))
    event=filter(df_precip,event==i)
    for (k in 1:t5_rows) {
      sub=filter(event,datetime>=t5_time[k,1]&datetime<t5_time[k+1,1])
      t5_time[k,P_mm]=nrow(sub)*0.254
    }
    df_event[i,'MI5']=max(t5_time[,P_mm],na.rm=T)*12
    
    t15_rows=as.numeric(round(df_event[i,duration_hr]*60/15))+1
    t15_time=as.data.frame(seq(from=start,to=end,by=900))
    event=filter(df_precip,event==i)
    if (df_event[i,duration_hr]>0.25) {
      for (k in 1:t15_rows) {
        sub=filter(event,datetime>=t15_time[k,1]&datetime<t15_time[k+1,1])
        t15_time[k,P_mm]=nrow(sub)*0.254
      }
      df_event[i,'MI15']=max(t15_time[,P_mm],na.rm=T)*4
    } else df_event[i,'MI15']=sum(event[,P_mm])*4
    
    t30_rows=as.numeric(round(df_event[i,duration_hr]*2))+1
    t30_time=as.data.frame(seq(from=start,to=end,by=1800))
    event=filter(df_precip,event==i)
    if (df_event[i,duration_hr]>0.5) {
      for (k in 1:t30_rows) {
        sub=filter(event,datetime>=t30_time[k,1]&datetime<t30_time[k+1,1])
        t30_time[k,P_mm]=nrow(sub)*0.254
      }
      df_event[i,'MI30']=max(t30_time[,P_mm],na.rm=T)*2
    } else { df_event[i,'MI30']=sum(event[,P_mm])*2 }
    
    t60_rows=as.numeric(round(df_event[i,duration_hr]))+1
    t60_time=as.data.frame(seq(from=start,to=end,by=3600))
    event=filter(df_precip,event==i)
    if (df_event[i,duration_hr]>1) {
      for (k in 1:t60_rows) {
        sub=filter(event,datetime>=t60_time[k,1]&datetime<t60_time[k+1,1])
        t60_time[k,P_mm]=nrow(sub)*0.254
      } 
      df_event[i,'MI60']=max(t60_time[,P_mm],na.rm=T)
    } else {df_event[i,'MI60']=sum(event[,P_mm])}
  }
}
#################################################


intensity(me_events, me_events$duration_hr, start = me_events$StartDate, me_events$EndDate,
          me_rain, me_rain$datetime, me_rain$precip_mm)


#get intensities 

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
ue_upper_stage <- ue_upper_stage %>%
  mutate(site = 'ue_upper',
         datetime = mdy_hms(datetime))

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

#uw_upper stage
uw_upper_stage <- uw_upper_stage %>%
  mutate(site = 'uw_upper',
         datetime = mdy_hms(datetime))


stage <- bind_rows(me_stage, me_upper_stage, mm_stage, mm_upper_stage,
                   mw_stage, mw_upper_stage, ue_stage, ue_upper_stage, um_stage, um_lower_stage,
                   uw_stage, uw_upper_stage)

stage_plot <- ggplot(stage, aes(x=datetime, y=Stage_mm, color=site)) +
  geom_line()

ggplotly(stage_plot)
