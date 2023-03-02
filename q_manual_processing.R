
# process manual discharge measurements

library(tidyverse)

setwd("D:/CPF/manual_Q")

q <- read_csv("D:/CPF/manual_Q/tunnel_q_20220829.csv") %>%
  mutate(dist_m = dist_m - lag(dist_m)) %>%
  replace(is.na(.), 0) %>%
  mutate(depth_m = depth_cm / 100) %>%
  select(-depth_cm) %>%
  mutate(q_m3s = dist_m * depth_m * v_ms) 

q_total_m3s <- sum(q$q_m3s)
q_Ls <- q_total_m3s * 1000
