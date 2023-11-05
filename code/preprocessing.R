# load packages 
library(tidyverse)

# read data
data <- read.table("data/exp.txt")

# preprocessing 
data <- data %>% 
  group_by(paper, subject, problem) %>% 
  mutate(sample = trial , 
         attended = option ,
         start = ifelse(sample == 1 & attended == 0, 0, ifelse(sample == 1 & attended == 1, 1, NA)) , 
         start_o = ifelse(is.na(start), first(start), start) ,
         switch = ifelse(attended != lag(attended), 1, 0) , 
         switch_n = sum(switch, na.rm = TRUE) , 
         switch_p = round(switch_n/((max(sample) - 1)), 2) , 
         stop = ifelse(sample == max(sample), 1, 0)
         ) %>%
  select(!c(trial, option))