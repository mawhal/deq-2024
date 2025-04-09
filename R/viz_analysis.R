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

# color scheme - see <https://www.pmscolorguide.com/coated/pms-2728-c> and VSU style guide
vsu <- c("#0047BB","#EE7624")

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
# 2) trends over time in measured data - see ggplot figures below
# 3) visualize/analysis association between weather events and water quality


## Summarize data 
# take means for all samples and subsamples
# important for Coliscan because these were sample splits rather than separate samples
dsite <- d_env %>% 
  group_by(id,date,site_name, datetime,
           measurement, 
           sum1,sum2,sum3,sum4,sum5,sum6,sum7,sum8,
           tobs,tmax) %>% 
  summarize( value = mean(value),
             river_height = mean(river_height_feet) )


#### Simple visualizations

# make boxplot for each measurement (facets)
# where each panel/facet compares the sites
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}


dsite %>%
  filter( site_name %in% c("Fleets Branch", "Harvell Dam")) %>% 
  filter( measurement %in% c("do_mgl","do_percent",
                             "pH","cfu_per_100ml","turbidity","water_temperature")) %>% 
  ggplot( aes(x = site_name, y = value, fill = site_name)) +
    facet_wrap( ~ measurement, scales = "free_y") + 
    geom_boxplot(notch = T) +
  scale_fill_manual(values = colorspace::lighten(vsu, amount = 0.3), name = "Site") +
  theme( axis.text.x = element_blank() ) + 
  xlab("Site")
ggsave("figs/all_measures_2sites.svg", width = 6, height = 3)

dsite %>%
  filter( site_name %in% c("Fleets Branch", "Harvell Dam")) %>% 
  filter( measurement %in% c("cfu_per_100ml")) %>% 
  ggplot( aes(x = site_name, y = value, fill = site_name)) +
  facet_wrap( ~ measurement, scales = "free_y") + 
  geom_boxplot(notch = T) + 
  xlab("Site") +
  scale_y_log10() +
  scale_fill_manual(values = colorspace::lighten(vsu, amount = 0.3), name = "Site") +
  theme( axis.text.x = element_blank() )
ggsave("figs/all_measures_Ecoli.svg", width = 4, height = 3)


# for other sites, only include times that are near the other two sites
dsite$month <- month(dsite$date)
dsite$week <- week(dsite$date)
with(dsite, table(week,site_name))
dweeks <- dsite %>% 
  filter( week %in% c(12,14,17,23,30,44)) # these weeks were found through a visual scan of the data
# which dates have data from all of te sites used here.
lubridate::ymd( "2024-01-01" ) + lubridate::weeks( c(12,14,17,23,30,44) )

dweeks %>% 
  filter( measurement %in% c("do_mgl","cfu_per_100ml","turbidity","water_temperature")) %>% 
  ggplot( aes(x = site_name, y = value, fill = site_name)) +
  facet_wrap( ~ measurement, scales = "free_y") + 
  geom_boxplot() +
  theme( axis.text.x = element_blank() ) +
  xlab("Site") + scale_fill_manual( values = c("gray",colorspace::lighten(vsu, amount = 0.3),"white"), name = "Site")
ggsave("figs/all_measures_all_sites.svg", width = 6, height = 3)

## Main data comparisons are between Harvell Dam and Fleets Branch sites
#

# look at precipitation, which one is most correlated with Ecoli?
cormat1 <- dsite %>% 
  ungroup() %>% 
  filter(measurement == "total_Ecoli", site_name == "Harvell Dam") %>% 
  select(sum1,sum2,sum3,sum4,sum5,sum6,sum7,sum8,
         value) %>% 
  cor(use = "complete.obs")
cormat2 <- d_env %>% 
  ungroup() %>% 
  filter(measurement == "total_Ecoli", site_name == "Fleets Branch") %>% 
  select(sum1,sum2,sum3,sum4,sum5,sum6,sum7,sum8,
         value) %>% 
  cor(use = "complete.obs")
rowMeans( 
  data.frame( cormat1[1:8,"value"],cormat2[1:8,"value"])
)






# water temperature
dsite %>% 
  filter( site_name %in% c("Fleets Branch", "Harvell Dam")) %>% 
  filter( measurement == "water_temperature") %>% 
  ggplot( aes(x = datetime, y = value, col = site_name) ) +
  geom_smooth(alpha = 0.3) + geom_point() + geom_line() + 
  ylab( expression(paste( "Water temperature (",degree,"C)") ) ) +
  xlab( "Sampling time") +
  scale_color_manual(values = vsu, name = "Sampling site") +
  theme(legend.position = "top")
ggsave( "figs/water_temperature_2site.svg", width = 5, height = 2.75)

# oxygen
dsite %>% 
  filter( site_name %in% c("Fleets Branch", "Harvell Dam")) %>% 
  filter( measurement == "do_mgl") %>% 
  ggplot( aes(x = datetime, y = value, col = site_name) ) +
  geom_smooth(alpha = 0.3) + geom_point() + geom_line() + 
  ylab( "Dissolved oxygen (mg/L)" ) +
  xlab( "Sampling time") +
  scale_color_manual(values = vsu, name = "Sampling site") +
  theme(legend.position = "top")
ggsave( "figs/water_oxygen_2site.svg", width = 5, height = 2.75)

# precipitation
env %>% 
  ggplot( aes(x = date, y = sum4) ) +
  geom_smooth(alpha = 0.3) + geom_point() + geom_line() + 
  ylab( "Precipitation over prev. 3 days (cm)" ) +
  xlab( "Sampling time") +
  scale_color_manual(values = vsu, name = "Sampling site") +
  theme(legend.position = "top")

# E coli
dsite %>% 
  filter( site_name %in% c("Fleets Branch", "Harvell Dam")) %>% 
  filter( measurement == "cfu_per_100ml") %>% 
  ggplot( aes(x = date, y = value, col = site_name) ) +
  geom_point( size = 2 ) + geom_smooth( se = F ) + 
  ylab("total E.coli colonies\nColiscan method") +
  scale_color_manual(values = vsu) +
  scale_y_log10() 
ggsave("figs/Ecoli_trend.svg", height = 3, width = 5)
dsite %>% 
  filter( site_name %in% c("Harvell Dam")) %>% 
  filter( measurement == "cfu_per_100ml") %>% 
  ggplot( aes(x = date, y = value) ) +
  geom_smooth( col = vsu[2], se = F ) + 
  geom_point( col = vsu[2] ) + 
  ylab("total E.coli colonies\nColiscan method")  + 
  ylim(c(0,525))
dsite %>% 
  filter( site_name %in% c("Fleets Branch")) %>% 
  filter( measurement == "cfu_per_100ml") %>% 
  ggplot( aes(x = date, y = value) ) +
  geom_smooth( col = vsu[1], se = F ) + 
  geom_point( col = vsu[1] ) + 
  ylab("total E.coli colonies\nColiscan method") + 
  ylim(c(0,3500))


# does weather and temperature predict the quantity of E. coli for each stream?
dcoli <- dsite %>% 
  filter(measurement == "cfu_per_100ml") %>% 
  filter(site_name %in% c("Fleets Branch", "Harvell Dam"))
dwide <- dsite %>% 
  ungroup() %>% 
  filter(site_name %in% c("Fleets Branch", "Harvell Dam")) %>% 
  pivot_wider( names_from = measurement, values_from = value )
dwide$cfu100 <- dwide$total_Ecoli / 3 * 100
hist(log(dwide$cfu100))
dwide$logcfu100 <- log(dwide$cfu100)

lm1 <- lm( logcfu100 ~ water_temperature + sum6 + site_name, data = dwide)
summary(lm1)
plot(lm1)


### temperature and precipitation as explanatory variables for E.coli
dwide %>% 
  filter( site_name %in% c("Fleets Branch", "Harvell Dam")) %>% 
  ggplot( aes(x = water_temperature, y = logcfu100, col = site_name) ) +
  geom_point( size = 2 ) + geom_smooth( se = T, method = "lm" ) + 
  ylab("CFU100\nColiscan method") +
  scale_color_manual(values = vsu) +
  scale_y_log10()
ggsave("figs/Ecoli_temperature.svg", height = 3, width = 5)
dwide %>% 
  filter( site_name %in% c("Fleets Branch", "Harvell Dam")) %>% 
  ggplot( aes(x = sum6, y = logcfu100, col = site_name) ) +
  geom_point( size = 2 ) + geom_smooth( se = T, method = "lm" ) + 
  ylab("CFU100\nColiscan method") +
  scale_color_manual(values = vsu) +
  scale_y_log10() 
ggsave("figs/Ecoli_precipitation.svg", height = 3, width = 5)
