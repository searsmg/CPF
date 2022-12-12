library(tidyverse)


#columns site, date, P, MI5, MI15, MI30, MI60, Flow
#flow values are 0 (no) or 1 (yes)
data=read_csv('D:/Downloads/threshold_data2.csv')

#creates possible threshold values
thresholds <- (seq(0, 30, by = 0.1))

#separates out sites
mbk1=subset(data,Site=='MBK1')
mbk2=subset(data,Site=='MBK2')


#identify threshold for one site
site=mbk2
site=filter(site,!is.na(site$Flow)) #flow is just 0 or 1
site=filter(site,!is.na(site$MI60))
num=nrow(site)

#flow_pred is the threshold prediction of flow for MI15, 30, 60
#correct indicatese whether the threshold prediction is correct
flow_pred_15=data.frame(matrix(0, ncol = num, nrow = 301)) #make df that has 301 rows for 0 to 30 by 0.1, column for each event  
flow_pred_30=data.frame(matrix(0, ncol = num, nrow = 301))  
flow_pred_60=data.frame(matrix(0, ncol = num, nrow = 301))  
correct_15=data.frame(matrix(0, ncol = num, nrow = 301)) #same as above  
correct_30=data.frame(matrix(0, ncol = num, nrow = 301))  
correct_60=data.frame(matrix(0, ncol = num, nrow = 301))  

for(j in seq(from=1, to=num, by=1)){
  for(i in seq(from=1, to=301, by=1)){
    flow_pred_15[i,j]=ifelse(site[j,5]>thresholds[i],1,0) #if MI15 is > possible thresh then 1, 0  
    correct_15[i,j]=ifelse(flow_pred_15[i,j]==site[j,8],1,0) #if above equals flow (0 or 1), then 1 
  }
}

#computes fraction correct for each threshold value; also computes kappa stat
p0_15=data.frame(matrix(0, ncol = 1, nrow = 301))
pflow_obs=sum(site[c(1:num),8])/num
pnoflow_obs=1-pflow_obs
pflow_15=data.frame(matrix(0, ncol = 1, nrow = 301))
pnoflow_15=data.frame(matrix(0, ncol = 1, nrow = 301))
pe_15=data.frame(matrix(0, ncol = 1, nrow = 301))
kappa_15=data.frame(matrix(0, ncol = 1, nrow = 301))

for(i in seq(from=1, to=301, by=1)){
  p0_15[i,1]=sum(correct_15[i,c(1:num)])/num
  pflow_15[i,1]=sum(flow_pred_15[i,c(1:num)])/num
  pnoflow_15[i,1]=1-pflow_15[i,1]
  pe_15[i,1]=pflow_obs*pflow_15[i,1]+pnoflow_obs*pnoflow_15[i,1]
  kappa_15[i,1]=(p0_15[i,1]-pe_15[i,1])/(1-pe_15[i,1])
}  

summary=tibble(site=c('mbk1'),
               T_15=0,
               kappa_15=0,
               T_30=0,
               kappa_30=c(0),
               T_60=c(0),
               kappa_60=c(0))

#this identifies threshold value based on highest kappa
summary[8,1]='mbk2'
row=which.max(kappa_15[,1])
summary[8,2]=thresholds[row,1]
summary[8,3]=max(kappa_15)
row=which.max(kappa_30[,1])
summary[8,4]=thresholds[row,1]
summary[8,5]=max(kappa_30)
row=which.max(kappa_60[,1])
summary[8,6]=thresholds[row,1]
summary[8,7]=max(kappa_60)

write_csv(summary,'summary_thresholds.csv')