###processing/QC for TC burned met station

library(dplyr)
library(lubridate)
library(plotly)

##setup
#TC has 2 dataloggers - need to combine
tc1_15min <- read.table(
  "C:/Users/sears/Documents/Research/CPF/Data_downloads/tunnelcreek_met_15min_20210903.dat",
  sep = ",", header=TRUE, skip="1")

tc1_15min <- tc1_15min[-c(1, 2), ]

#put date format in POSIXct for tc1
tc1_15min <- tc1_15min %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP))

tc2_15min <- read.table(
  "C:/Users/sears/Documents/Research/CPF/Data_downloads/tunnelcreek_met2_15min_20210917.dat",
  sep = ",", header=TRUE, skip="1")

tc2_15min <- tc2_15min[-c(1, 2), ]

#put date format in POSIXct and change rest of columns to be numeric for tc2
tc2_15min <- tc2_15min %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) %>%
  mutate_if(is.character,as.numeric)

#tipping bucket is in a different data table output
tcrain_5min <- read.table(
  "C:/Users/sears/Documents/Research/CPF/Data_downloads/tunnelcreek_rain_20210917.dat",
  sep = ",", header=TRUE, skip=2)

tcrain_5min <- tcrain_5min[-c(1), ]

tcrain_5min <- tcrain_5min %>%
  mutate(TS = ymd_hms(TS)) %>%
  mutate_if(is.character,as.numeric)

#add geonor
tcgeonor <- read.table(
  "C:/Users/sears/Documents/Research/CPF/Data_downloads/tunnelcreek_geonor_20210917.dat",
  sep = ",", header=TRUE, skip=1)

#remove header stuff
tcgeonor <- tcgeonor[-c(1, 2), ]

tcgeonor <- tcgeonor %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) %>%
  mutate_if(is.character,as.numeric)

##start with met1
#soil moisture @ 5, 20, 50 cm
sm <- plot_ly(data=tc1_15min, x=~TIMESTAMP, y=~SM300_5_AVG, type="scatter", mode="lines", name="SM_5")
sm <- sm %>% add_trace(y=~SM300_20_AVG, type="scatter", mode="lines",name="SM_20")
sm <- sm %>% add_trace(y=~SM300_50_AVG, type="scatter", mode="lines",name="SM_50")
sm <- sm %>% layout(yaxis=list(title = "Soil moisture"))
sm

#Ts @ 5 cm
Ts <- plot_ly(data=tc1_15min, x=~TIMESTAMP, y=~Tsoil_5_AVG, type="scatter", mode="lines")
Ts

#Rn from net R lite
Rn <- plot_ly(data=tc1_15min, x=~TIMESTAMP, y=~Rn_WM2_AVG, type="scatter")
Rn

#wind speed (note, wind direction not working)
ws <- plot_ly(data=tc1_15min, x=~TIMESTAMP, y=~WS_ms_AVG, type="scatter")
ws

##start met2
#snow depth_m
sd <- plot_ly(data=tc2_15min, x=~TIMESTAMP, y=~SnowDepth_m_Avg, type="scatter")
sd

#air temp C
Ta <- plot_ly(data=tc2_15min, x=~TIMESTAMP, y=~AirTC_Avg, type="scatter")
Ta

#RH
rh <- plot_ly(data=tc2_15min, x=~TIMESTAMP, y=~RH, type="scatter")
rh

#SW in and SW out
sw <- plot_ly(data=tc2_15min, x=~TIMESTAMP, y=~SWin_Avg, type="scatter", mode="lines", name="SW_in")
sw <- sw %>% add_trace(y=~SWout_Avg, type="scatter", mode="lines",name="SW_out")
sw <- sw %>% layout(yaxis=list(title = "Radiation (W/m2)"))
sw

#LW in and LW out
lw <- plot_ly(data=tc2_15min, x=~TIMESTAMP, y=~LWin_Avg, type="scatter", mode="lines", name="LW_in")
lw <- lw %>% add_trace(y=~LWout_Avg, type="scatter", mode="lines",name="LW_out")
lw <- lw %>% layout(yaxis=list(title = "Radiation (W/m2)"))
lw

#net radiation
nr <- plot_ly(data=tc2_15min, x=~TIMESTAMP, y=~NR_Avg, type="scatter", mode="lines")
nr

##rain from 5min rain
#rain @ 5 min interval
tips <- plot_ly(data=tcrain_5min, x=~TS, y=~count, type="scatter", mode="lines")
tips

#add conversion of tips to rainfall amount
rain <- plot_ly(data=tcrain_5min, x=~TS, y=~count*.254, type="scatter", mode="lines")
rain

#geonor rain data
geonor <- plot_ly(data=tcgeonor, x=~TIMESTAMP, y=~Geonor_Depth_Average, type="scatter", mode="lines")
geonor
