setwd("C:/Users/806826/Documents/Data/LMD prediction")
#save.image(file="lmd.rda")
#load(file = "lmd.rda") 


# Combining all sensor data  ----------------------------------------------


library(data.table)
library(readbulk)

path <- "C:/Users/806826/Documents/Data/LMD prediction/sensor_jantoapr"

multmerge = function(path){
  filenames=list.files(path=path, full.names=TRUE)
  rbindlist(lapply(filenames, read.table))
}

allsensor <- multmerge(path)
allsensor <- data.frame(str_split_fixed(allsensor$V1, ",", 9))
#no_zeros <- apply(allsensor, 1, function(x) all(x !=0 ))
#allsensor1 <- allsensor[no_zeros,]

colnames(allsensor) <- c("deviceid", "tripdate", "tripid", "accrms", "accxrms", "accyrms", "acczrms", "endtime", "starttime")
allsensor1 <- allsensor[-1,]

length(unique(allsensor$deviceid))


# Analyzing data  ---------------------------------------------------------

sensors <- read.csv("allsensor.csv")
sensors <- sensors[-1,-1]
s <- sensors %>%
  mutate(endtime = as.character(endtime)) %>%
  mutate(starttime = as.character(starttime)) %>%
  mutate(endtime = as.numeric(endtime)) %>%
  mutate(starttime = as.numeric(starttime)) %>%
  mutate(duration = (endtime-starttime)/1000)

# List of deviceid with more than 50% of <2 seconds duration
less2sec <- s %>%
  group_by(deviceid) %>%
  mutate(percent = mean(duration < 2)) %>%
  distinct(deviceid, percent) %>%
  arrange(desc(percent))

View(less2sec)
               
library(ggplot2)
ggplot(sensors, aes(x=duration), geom_density())
boxplot(sensors$duration)



library(dplyr)
sensors1 <- sensors %>% sample_frac(0.1)
sensors1 <- sensors1 %>% 
  mutate(deviceid = as.character(deviceid)) %>%
  mutate(tripdate = as.Date(tripdate)) %>%
  mutate(tripid = as.numeric(tripid)) %>%
  mutate(accrms = as.numeric(accrms)) %>%
  mutate(accxrms = as.numeric(accxrms)) %>%
  mutate(accyrms = as.numeric(accyrms)) %>%
  mutate(acczrms = as.numeric(acczrms)) %>%
  mutate(endtime = as.integer(endtime)) %>%
  mutate(starttime = as.integer(starttime)) %>%
  mutate(duration = endtime-starttime) #new feature

sensors3 <- sensors %>% sample_frac(0.3)
sensors3 <- sensors3 %>% 
  mutate(deviceid = as.character(deviceid)) %>%
  mutate(tripdate = as.Date(tripdate)) %>%
  mutate(tripid = as.numeric(tripid)) %>%
  mutate(accrms = as.numeric(accrms)) %>%
  mutate(accxrms = as.numeric(accxrms)) %>%
  mutate(accyrms = as.numeric(accyrms)) %>%
  mutate(acczrms = as.numeric(acczrms)) %>%
  mutate(endtime = as.integer(endtime)) %>%
  mutate(starttime = as.integer(starttime)) %>%
  mutate(duration = endtime-starttime) #new feature
