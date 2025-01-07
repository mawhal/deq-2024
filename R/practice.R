# this is a comment
# R will not try to run this code

# let's start with simple logical arguments and math calculations
2==2
2>2
2>=2
2+2
2*2
2^3

# vectors and data types
class(2)
class(2.5)
class("2.5")
"2.5" == "2.5"
"yes" == "yes"
"yes" == "no"
# to make a vector, use c()
c(2,4,5,6)
c(2,4,5,6) + 1
c(2,4,5,6) + c(1,2,3,4)
c(2,4,5,6) + c(1,2,3)
# hold alt-shift then down to copy a line

# objects and summary functions
x = c(2,4,5,6)
# summary functions on objects
sum(x)
min(x)
max(x)
range(x)
mean(x)
plot(x)
y = seq(1,4,by = 1)
par(mar=c(5,4,2,2)+0.1)
plot(x,y)

# linear regression
lm1 = lm(y ~ x)
summary(lm1)
plot(x,y)
abline(lm1,col="blue")

# data.frames
temp = rnorm(100, mean = 20, sd = 3 )
temp
day = 1:100
df = data.frame(day,temp)

# packages
# install.packages("tidyverse")
library(tidyverse)

# read data files
# use function read_csv()
draw = read_csv(file = "data/data_entry - data.csv")

# summarize by group
# use pipes  -  %>%  = control-shift-m
draw %>% 
  group_by(measurement) %>% 
  summarize( min.value = min(value, na.rm = TRUE),
             max.value = max(value, na.rm = TRUE))
# data query using dollar sign
!is.numeric(draw$value)
sort(unique(draw$value))
