## Data visualization and analysis
# Whalen DEQ project 2024


### NOTES to self ####
# 1. estimate flow rate of all rivers at their sampling points
# 2. email JRA for 2024 data from Harvell Dam to spot check

######################

# load packages
library(tidyverse)
library(lubridate)

# read in the data
d <- read_csv("data/data_qaqc.csv")
meta <- read_csv("data/metadata.csv")
env <- read_csv("data/env_weekly.csv")


# merge metadata, environmental with observed data
names(d)[ names(d) %in% names(meta) ]
names(env)[ names(env) %in% names(meta) ]
meta_env <- left_join( meta, env )
d_env <- left_join( d, meta_env )

# set dates
d_env$date <- ymd( d_env$date )
d_env$datetime <- ymd_hms( d_env$datetime )



## Visualization and Analysis
# 1) compare sites (specifically, Fleets Branch)
# 2) trends over time in measured data
# 3) visualize/analysis association between weather events and water quality



#### Simple visualizations
## Main data comparisons are between Harvell Dam and Fleets Branch sites
#
# Water temperature
d_env %>% 
  filter( site_name %in% c("Fleets Branch", "Harvell Dam")) %>% 
  filter( measurement == "water_temperature") %>% 
  ggplot( aes(x = date, y = value) ) +
  facet_wrap( ~ site_name) +
  geom_point() + geom_smooth()
# pH
d_env %>% 
  filter( site_name %in% c("Fleets Branch", "Harvell Dam")) %>% 
  filter( measurement == "pH") %>% 
  ggplot( aes(x = date, y = value, col = site_name) ) +
  geom_point() + geom_smooth()
# DO - DO usually slightly higher at Harvell Dam than Fleets Branch
d_env %>% 
  filter( site_name %in% c("Fleets Branch", "Harvell Dam")) %>% 
  filter( measurement == "do_mgl") %>% 
  ggplot( aes(x = date, y = value, col = site_name) ) +
  geom_point() + geom_smooth()
# E coli
d_env %>% 
  filter( site_name %in% c("Fleets Branch", "Harvell Dam")) %>% 
  filter( measurement == "total_Ecoli") %>% 
  ggplot( aes(x = date, y = cfu100, col = site_name) ) +
  geom_point() + geom_smooth() + 
  ylab("total E.coli colonies\nColiscan method") +
  scale_y_log10()
d_env %>% 
  filter( site_name %in% c("Harvell Dam")) %>% 
  filter( measurement == "total_Ecoli") %>% 
  ggplot( aes(x = date, y = cfu100, col = site_name) ) +
  geom_point() + geom_smooth() + 
  ylab("total E.coli colonies\nColiscan method") 


### Further data manipulation and analysis



 