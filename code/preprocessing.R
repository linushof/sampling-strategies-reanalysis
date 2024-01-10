# load packages 
library(pacman)
p_load(tidyverse)

# read data
data <- read.table("data/exp.txt") %>% as_tibble()

# check data
unique_paper <- data %>% distinct(paper) %>% nrow()
unique_data_sets <- data %>% distinct(paper, id) %>% nrow()
unique_paper < unique_data_sets # more data sets than paper -> include data sets as grouping variable

# pre-processing
clean_data <- data %>% 
  group_by(paper, id, subject, problem) %>% 
  rename(sample = trial, # sample: number of sampled outcome within a trial
         attended = option) %>% # attended: option from which was sampled
  mutate(start = ifelse(sample == 1 & attended == 0, 0, ifelse(sample == 1 & attended == 1, 1, NA)) , # identify start option
         start_o = ifelse(is.na(start), first(start), start) ,
         switch = ifelse(attended != lag(attended), 1, 0) , # identify switch (0 = no switch, 1 = switch)
         switch_n = sum(switch, na.rm = TRUE) , # number of switches in a trial
         switch_p = round(switch_n/((max(sample) - 1)), 2) , # proportion of switches in a trial
         stop = ifelse(sample == max(sample), 1, 0)
         )
clean_data %>% distinct(paper, id, subject, problem) %>% nrow()

