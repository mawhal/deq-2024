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
             max.value = max(value, na.rm = TRUE) )
# data query using dollar sign
!is.numeric(draw$value)



# prepare metadata file
meta <- metaraw 
meta$date <- ymd(meta$date)
meta$time <- hm( paste( stringr::str_sub(meta$time, 1,-3)  , stringr::str_sub(meta$time, -2) ) )
meta$time[is.na(meta$time)] <- hms("12H 0M 0S")
meta$datetime <- ymd_hms(paste(meta$date, meta$time))
meta <- meta %>% 
  select(id, date, datetime, site_name, site_conditions, site_notes,
         river_height_feet)
# look for duplicates
duplicated( meta ) # row 56 is duplicated as a stand-by for potentially missing data
# based only on id
meta[ duplicated(meta$id), ]


# write to disk
write_csv(meta, "data/metadata.csv")

# prepare final data file
d <- draw
d %>% 
  filter( measurement == "total_Ecoli" ) %>% 
  filter( is.na(value) )

# outliers?
by( d$value, d$measurement, FUN = summary)

# use the standard by James River Association
# CFU / 100ml
d$cfu100 <- d$value/3 * 100
d$cfu100[ d$measurement != "total_Ecoli" ] <- NA


# write to disk
write_csv( d, "data/data_qaqc.csv")
