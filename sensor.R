setwd("C:/Users/806826/Documents/Data/LMD prediction")
library(dplyr)
#save.image(file="lmd.rda")
#load(file = "lmd.rda") 

# Analyzing data  ---------------------------------------------------------
sensors3 <- read.csv("sensors3.csv")

# Remove outliers of duration < 2 seconds
sensors3 <- sensors3[-which(sensors3$duration<2),]

sensors4 <- sensors3 %>%
  group_by(deviceid, tripdate) %>%
  summarize(tripcount = max(tripid), 
            duration_sd = sd(duration), 
            duration_mean = mean(duration),
            duration_median = median(duration), 
            rms_sd = sd(accrms),
            rms_mean = mean(accrms),
            rms_median = median(accrms)
            )
sensors4 <- data.frame(sensors4)
  

# Outliers that are 2sec or less, and more than 2 mins 
outliers <- sensors3 %>%
  group_by(deviceid) %>%
  mutate(less3sec = mean(duration < 3)) %>%
  mutate(more2min = mean(duration >120)) %>%
  distinct(deviceid, less3sec, more2min) %>%
  arrange(desc(more2min))

View(outliers)


# Visualisation -----------------------------------------------------------

library(ggplot2)
ggplot(sensors, aes(x=duration), geom_density())
boxplot(sensors$duration)


