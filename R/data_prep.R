## Whalen VA DEQ grant 2024
# Quality control script to prepare final data for the end-of-year report


# load packages
library(tidyverse)
library(lubridate)

# read data files
# use function read_csv()
draw = read_csv(file = "data/data_entry - data.csv")
metaraw = read_csv(file = "data/data_entry - metadata.csv")

# are all ids in the metadata found in the data and vice versa
unique(draw$id)[!(unique(draw$id) %in% unique(metaraw$id))]
unique(metaraw$id)[!(unique(metaraw$id) %in% unique(draw$id))]


#
# summarize by group
# use pipes  -  %>%  = control-shift-m
draw %>% 
  group_by(measurement) %>% 
  summarize( min.value = min(value, na.rm = TRUE),
             max.value = max(value, na.rm = TRUE))
# data query using dollar sign
!is.numeric(draw$value)
sort(unique(draw$value))



# prepare metadata file
meta <- metaraw 
meta$date <- ymd(meta$date)
meta$time <- hm( paste( stringr::str_sub(meta$time, 1,-3)  , stringr::str_sub(meta$time, -2) ) )
meta$datetime <- ymd_hms(paste(meta$date, meta$time))
meta <- meta %>% 
  select(id, date, datetime, site_name, site_conditions, site_notes,
         river_height_feet)
# write to disk
write_csv(meta, "data/metadata.csv")
                        
