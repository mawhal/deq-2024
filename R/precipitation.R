## precipitation data from NOAA
# Whalen DEQ grant 2024

# Weather Station Information for Petersburg, Virginia
## From NOAA (National Oceanic and Atmospheric Administration)
# Name	PETERSBURG, VA US   Network:ID	GHCND:USC00446656  position: 37.2388 N, -77.3925 S
#  <https://www.ncdc.noaa.gov/cdo-web/search?datasetid=GHCND>

# Data downloaded from above website on 2 January 2024

# load packages
library(tidyverse)
library(lubridate)  
  
# read precip data
praw <- read_csv("data/precip_data_3888616.csv")
# rename
names(praw) <- tolower( names(praw) )
# format date
praw$date <- ymd(praw$date)
                        
# metadata
pmeta <- praw %>% select( station, name, date, 
                          prcp_attributes, snwd_attributes,
                          tmax_attributes, tmin_attributes,
                          tobs_attributes, wt01_attributes,
                          wt03_attributes, wt11_attributes ) %>% 
  distinct()

pdata <- praw %>% select( date,
                          prcp,snow,snwd,
                          tmax,tmin,tobs,
                          wt01,wt03,wt11 )
summary(pdata)
# keep precipitation (centimeters, tmax (max temperature in degrees Celsius), 
# tmin (minimum temperature in degrees Celsius), tobs (observed temperature in degrees Celsius)
pdata <- pdata %>% select( date, precip_cm = prcp, tmax, tmin, tobs )

pairs(pdata)

## save/write data to disk
write_csv(pdata, "data/precip.csv")


# prepare data for analysis


# use data in metadata file to search for sample dates

# use the precip for 0 and 1 day prior, plus averages for 1-7 days prior to each sampling date
meta <- read_csv("data/metadata.csv")

# make an appropriate sequence of dates
dates = sort(unique(meta$date))
date.list <- lapply( dates, function(z) seq.Date(from = z-7, to = z, by = 1) )
pdates <- data.frame( sample.date = rep(dates,each = 8), date = do.call("c",date.list) )


# Access the precipitation data for the list of dates 
precip.data.all <- left_join( pdates, pdata )

test <- precip.data.all[precip.data.all$sample.date == dates[1],]

# function to calculate averages for up to one week prior to sampling
week_average <- function(z, n = 8) {
  return( c( sameday = z[n],
  oneday  = z[n-1],
  mean1   = mean(z[(n-1):n]),
  mean2   = mean(z[(n-2):n]),
  mean3   = mean(z[(n-3):n]),
  mean4   = mean(z[(n-4):n]),
  mean5   = mean(z[(n-5):n]),
  mean6   = mean(z[(n-6):n]),
  mean7   = mean(z[(n-7):n]) )
  )
}
week_average(test$precip_cm)

# apply across all variables desired
precip_weekly <- tapply( precip.data.all$precip_cm, 
        list(precip.data.all$sample.date), 
        FUN =  week_average )
precip_weekly <- do.call( rbind, precip_weekly)
precip_weekly <- data.frame( date = rownames(precip_weekly), precip_weekly)

tobs_weekly <- tapply( precip.data.all$tobs, 
                         list(precip.data.all$sample.date), 
                         FUN =  week_average )
tobs_weekly <- do.call( rbind, tobs_weekly)
tobs_weekly <- data.frame( date = rownames(tobs_weekly), tobs_weekly)
apply(tobs_weekly,2,sd)
# consider mean5 for tobs because it reduces variance a bit



tmax_weekly <- tapply( precip.data.all$tmax, 
                       list(precip.data.all$sample.date), 
                       FUN =  week_average )
tmax_weekly <- do.call( rbind, tmax_weekly)
tmax_weekly <- data.frame( date = rownames(tmax_weekly), tmax_weekly)
with(tmax_weekly, plot(sameday,mean7))
apply(tmax_weekly,2,sd)
# consider mean7 for tmax because it reduces variance and what seems like potential outliers.

# collect all relevant pieces
env_weekly <- data.frame( precip_weekly,
                          select(tobs_weekly, tobs = sameday, tobs_mean = mean5),
                          select(tmax_weekly, tmax = sameday, tmax_mean = mean7) )
               
write_csv(env_weekly, "data/env_weekly.csv")           
                          