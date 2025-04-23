## Whalen VA DEQ grant 2024
# Quality control script to prepare final data for the end-of-year report


# load packages
library(tidyverse)
library(lubridate)
library(cowplot)


# read data 
meta <- read_csv("data/metadata.csv")
d <- read_csv("data/data_qaqc.csv")

# use the standard by James River Association and others
# CFU / 100ml
dcoli <- d %>% 
  filter( measurement == "R-card total_Ecoli" ) 
dcoli$measurement = "rcard_cfu"
dcoli$value <- dcoli$value/3 * 100
# merge with dataset
dfull <- bind_rows(d, dcoli)
d <- dfull %>% arrange(id, measurement, replicate)



# filter coliscan and rcard methods
dmeth <- d %>% 
  filter( measurement %in% c("cfu_per_100ml", "rcard_cfu" ) )


# pivot wider
dwide <- dmeth %>% 
  pivot_wider(names_from = "measurement", values_from = "value")

# select columns and filter data to samples with both measurements
# after averaging replicates, which are mostly subsamples from the same sample
d <- dwide %>% select( id, replicate, 
                       cfu_coliscan = cfu_per_100ml, 
                       cfu_rcard = rcard_cfu ) %>%
  group_by(id) %>% 
  summarize(cfu_coliscan = mean(cfu_coliscan, na.rm = T),
            cfu_rcard = mean(cfu_rcard, na.rm = T)) %>% 
  filter( !is.na(cfu_coliscan) & !is.na(cfu_rcard) )



### Statistics and plotting

# raw space
a <- ggplot( data = d, aes(x = cfu_coliscan, y = cfu_rcard)) +
  geom_point(size = 2) +
  ggtitle("raw scale")
# log-log space
b <- ggplot( data = d, aes(x = cfu_coliscan, y = cfu_rcard)) +
  geom_point(size = 2) +
  scale_x_log10() + scale_y_log10() +
  ggtitle("log10 scale")

cowplot::plot_grid(a,b, align = "hv")

# go back to a long format
dlong <- d %>% 
  pivot_longer( cfu_coliscan:cfu_rcard, 
                names_to = "method", values_to = "value" )
ggplot( data = dlong, aes(x = value)) + 
  facet_wrap(~method) +
  geom_histogram()
ggplot( data = dlong, aes(x = log(value))) + 
  facet_wrap(~method) +
  geom_histogram()

# null hypothesis: Coliscan and Rcard use the same technology and are intended to produce the same results,
#                  so these measurements should be the same
# alternative hypothesis: measurements differ

## UNTRANSFORMED DATA
t.test( y = d$cfu_rcard, x = d$cfu_coliscan, paired = TRUE )
    # when a paired two-sample approach is used, there is a significant difference
    # magnitude of difference = Rcard estimates more by 372 CFU
# correlation
cor.test( y = d$cfu_rcard, x = d$cfu_coliscan )
# correlation = 0.82 and is significant


## TRANSFORMED DATA
t.test( y = log(d$cfu_rcard), x = log(d$cfu_coliscan), paired = TRUE )
      # no longer a significant difference
# correlation
cor.test( y = log(d$cfu_rcard), x = log(d$cfu_coliscan) )
# correlation = 0.86 and is significant



# Write data to disk
write_csv(d, "data/Harvell_dam_2024_coliscan_rcard.csv")

