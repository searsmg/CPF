###processing for Joe Wright unburned met station

library(tidyverse)
library(dplyr)
library(lubridate)
library(plotly)
library(epitools)

##setup
jw_15min <- read.table(
  "C:/Users/sears/Documents/Research/CPF/Data_downloads/joewright_met_15min_20220112.dat",
  sep = ",", header=TRUE, skip="1")

jw_15min <- jw_15min[-c(1, 2), ]

#put date format in POSIXct and change rest of columns to be numeric
jw_15min <- jw_15min %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) %>%
  mutate_if(is.character,as.numeric)

#tipping bucket is in a different data table output
jwrain_5min <- read.table(
  "C:/Users/sears/Documents/Research/CPF/Data_downloads/joewright_rain_20220112.dat",
  sep = ",", header=TRUE, skip=2)

jwrain_5min <- jwrain_5min[-c(1), ]

jwrain_5min <- jwrain_5min %>%
  mutate(TS = ymd_hms(TS)) %>%
  mutate_if(is.character,as.numeric)

#delete later
mc <- read.csv("C:/Users/sears/Documents/Research/CPF/Data_downloads/mtncampus_20220223.csv") %>%
  mutate(datetime = ymd_hm(TIMESTAMP)) 


##check data using plotly
#wind speed (m/s)
ws <- plot_ly(data=jw_15min, x=~TIMESTAMP, y=~WS_ms_Avg, type="scatter")
ws

#wind direction
wd <- plot_ly(data=jw_15min, x=~TIMESTAMP, y=~WindDir, type="scatter")
wd

#SW in and SW out
sw <- plot_ly(data=jw_15min, x=~TIMESTAMP, y=~SWin_Avg, type="scatter", mode="lines", name="SW_in")
sw <- sw %>% add_trace(y=~SWout_Avg, type="scatter", mode="lines",name="SW_out")
sw <- sw %>% layout(yaxis=list(title = "Radiation (W/m2)"))
sw

ggplot() +
  geom_line(data=mc, aes(x=datetime, y=SWin_Avg), color="blue") +
  geom_line(data=jw_15min, aes(x=TIMESTAMP, y=SWin_Avg))

ggplot() +
  geom_line(data=mc, aes(x=datetime, y=SWout_Avg), color="blue") +
  geom_line(data=jw_15min, aes(x=TIMESTAMP, y=SWout_Avg))

ggplot() +
  geom_line(data=mc, aes(x=datetime, y=SWnet_Avg), color="blue") +
  geom_line(data=jw_15min, aes(x=TIMESTAMP, y=SWnet_Avg))

ggplot() +
  geom_line(data=mc, aes(x=datetime, y=SWalbedo_Avg), color="blue") +
  geom_line(data=jw_15min, aes(x=TIMESTAMP, y=SWalbedo_Avg))

#LW in and LW out
lw <- plot_ly(data=jw_15min, x=~TIMESTAMP, y=~LWin_Avg, type="scatter", mode="lines", name="LW_in")
lw <- lw %>% add_trace(y=~LWout_Avg, type="scatter", mode="lines",name="LW_out")
lw <- lw %>% layout(yaxis=list(title = "Radiation (W/m2)"))
lw

#net radiation
nr <- plot_ly(data=jw_15min, x=~TIMESTAMP, y=~NR_Avg, type="scatter", mode="lines")
nr

#soil moisture @ 5, 20, 50 cm
sm <- plot_ly(data=jw_15min, x=~TIMESTAMP, y=~SM150_5_Avg, type="scatter", mode="lines", name="SM_5")
sm <- sm %>% add_trace(y=~SM150_20_Avg, type="scatter", mode="lines",name="SM_20")
sm <- sm %>% add_trace(y=~SM150_50_Avg, type="scatter", mode="lines",name="SM_50")
sm <- sm %>% layout(yaxis=list(title = "Soil moisture"))
sm

#soil temp
Ts <- plot_ly(data=jw_15min, x=~TIMESTAMP, y=~SoilT5_C_Avg, type="scatter", mode="lines")
Ts

#air temp
Ta <- plot_ly(data=jw_15min, x=~TIMESTAMP, y=~AirTC_Avg, type="scatter", mode="lines")
Ta

#RH
rh <- plot_ly(data=jw_15min, x=~TIMESTAMP, y=~RH, type="scatter", mode="lines")
rh

##switch to rain output
#rain @ 5 min interval
tips <- plot_ly(data=jwrain_5min, x=~TS, y=~count, type="scatter", mode="lines")
tips

#calculate actual rainfall amount - 1 tip = .254 mm of rain
rain <- plot_ly(data=jwrain_5min, x=~TS, y=~count*.254, type="scatter", mode="lines")
rain
