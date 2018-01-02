library(dplyr)
library(stringr)
library(ggplot2)
library(lattice)
library(data.table)
library(readbulk)

setwd("C:/Users/806826/Documents/Data/LMD prediction")
#save.image(file="1.rda")
#load(file = "1.rda")


# company -----------------------------------------------------------------

company <- read.csv("Company.csv")
company$mco_id <- as.factor(company$mco_id)
company <- company[,c(1,3)]

##company: mco_id, name 


# faultdesc  ----------------------------------------------------------------

faultdesc <- read.csv("Fault_Desc.csv")

colnames(faultdesc)[1] <- "f_nbr"
faultdesc$f_nbr <- as.factor(faultdesc$f_nbr)

##faultdesc: f_nbr, Remarks 



# lifterror ---------------------------------------------------------------

path <- "C:/Users/806826/Documents/Data/LMD prediction/5yrs data"

multmerge = function(path){
  filenames=list.files(path=path, full.names=TRUE)
  rbindlist(lapply(filenames, fread))
}
  
lifterror <- multmerge(path)

#which(colnames(lifterror)=="lift_id")
#which(colnames(lifterror)=="fault_nbr")
#which(colnames(lifterror)=="insert_date")
#which(colnames(lifterror)=="report_desc")

lerror <- lifterror[,c(8,11,12,26)]

colnames(lerror)[which(names(lerror) == "fault_nbr")] <- "f_nbr"
#lerror$f_nbr <- gsub('.{2}$', '', lerror$f_nbr)  #remove last 2 chars

lerror <- lerror %>% 
  mutate(lift_id = as.character(lift_id)) %>%
  mutate(f_nbr = as.factor(f_nbr)) %>%
  mutate(report_desc = as.character(report_desc))

##lerror: lift_id, f_nbr, report_desc, postal_code, date 


# lift --------------------------------------------------------------------

lift <- read.csv("Lifts Oct17.csv")

keepcol <- c("lift_id", "mco_id", "building_id", "landings", "speed", "commission_date", "load_capacity", "person_capacity",
             "drive_type", "car_control_1", "car_control_2", "dvr_type")
lift <- lift[, names(lift) %in% keepcol]

lift <- lift %>%
  mutate(lift_id = as.character(lift_id)) %>%
  mutate(mco_id = as.factor(mco_id)) %>%
  mutate(building_id = as.character(building_id)) %>%
  mutate(landings = as.numeric(landings)) %>%
  mutate(speed = as.numeric(speed)) %>%
  rename(comm_date = commission_date) %>%
  mutate(comm_date = substr(comm_date, 1,10)) %>%
  mutate(comm_date = as.Date(comm_date, "%d/%m/%Y")) %>%
  mutate(load_capacity = as.numeric(load_capacity)) %>%
  mutate(person_capacity = as.numeric(person_capacity)) %>%
  mutate(car_control_1 = as.factor(car_control_1)) %>%
  mutate(car_control_2 = as.factor(car_control_2))

##lift: lift_id, mco_id, building_id, lmd_comm_date, landings, speed, commission_date, load_capacity, person_capacity,
#drive_type, storey, car_control_1, car_control_2, dvr_type



# building ----------------------------------------------------------------

building <- read.csv("Buildings Oct17.csv")

keepcol2 <- c("building_id", "town_council_code", "storey", "building_type", "building_date", "total_dwell_unit")
building <- building[, names(building) %in% keepcol2]

# mtcars %>% select(wt, gear, everything()) 
# --> shift "wt", "gear" to left and everything else to the right

building <- building %>%
  mutate(building_id = as.character(building_id)) %>%
  mutate(building_date = gsub('.{5}$', '', building_date)) %>%
  mutate(building_date = as.Date(building_date, "%d/%m/%Y")) %>%
  mutate(total_dwell_unit = as.numeric(total_dwell_unit)) %>%
  filter(building_type == "Block")

# building: building_id, town_council_code, storey, building_type, building_date, total_dwell_unit


# Feature engineering wrt time ----------------------------------------------------------

faultdesc <- faultdesc[, !names(faultdesc) %in% c("Chronic.Lifts", "Description")]
lerror_faultdesc <- lerror %>% 
  left_join(faultdesc, by = "f_nbr") %>%
  filter(Remarks == "Breakdown") #TAKE ONLY BREAKDOWN RESULTS. 

# making target label as "Breakdown"/"OK" 
lerror_faultdesc$f_nbr <- as.factor(lerror_faultdesc$f_nbr)
lerror_faultdesc$Remarks <- as.character(lerror_faultdesc$Remarks)
lerror_faultdesc$Remarks[which(is.na(lerror_faultdesc$Remarks) | lerror_faultdesc$Remarks == "")] <- "OK"
lerror_faultdesc$Remarks <- as.factor(lerror_faultdesc$Remarks)

# separate insert_date into date and time 
lerror_faultdesc <- lerror_faultdesc %>%
  mutate(f_date = substr(insert_date, 1,11)) %>%
  mutate(f_date = as.Date(f_date, "%d/%b/%Y")) %>%
  mutate(f_time = as.POSIXct(substr(insert_date, 1,17), format="%d/%b/%Y %H:%M"))

lerror_faultdesc <- na.omit(lerror_faultdesc)

# 1) day of week (mon-sun)
prejoin <- lerror_faultdesc %>%
  mutate(f_day = weekdays(f_date))

# 2) time of fault (am/pm)
prejoin <- prejoin %>%
  mutate(f_hour = format(f_time, "%H"))

# 3) breakdown count in past 
lerror_faultdesc$breakdown <- ifelse(lerror_faultdesc$Remarks=="Breakdown", 1, 0) 

prejoin <- lerror_faultdesc %>% 
  group_by(lift_id) %>%
  arrange(lift_id, f_date) %>%
  mutate(hist_count = cumsum(breakdown))  

# 4) no. days from previous breakdown
prejoin <- prejoin %>%
  mutate(prevlag = f_date - lag(f_date))

# 5) frequency of breakdown ie. time it takes for a breakdown to occur
prejoin <- prejoin %>% 
  mutate(freq = (f_date - min(f_date))/hist_count) 

# 6) average weekly breakdowns

# 7) no days till next BD 
prejoin <- prejoin %>% 
  mutate(days_nextbd = lead(f_date)-f_date)

# Y2 = BD in next 7 days (y/n)
prejoin <- prejoin %>% 
  mutate(sevendays = ifelse((lead(f_date)-f_date)<8, 1, 0))

# Y3 = BD in next 14 days (y/n)

# how many lifts in 'lift' table does not appear in 'lerror' table at all?
require(data.table)
setDT(lift,lerror)
setkey(lift,lift_id)
setkey(lerror,lift_id)
lift[!lerror]

# count of breakdowns by day
date_breakdown <- prejoin %>% 
  tally(breakdown ==1)

# Joining together  -------------------------------------------------------------

# Inspect NAs
# sapply(alljoin, function(x) ifelse(sum(is.na(x))/nrow(alljoin)<0.05, "Y", "N"))> --> % of NAs in each col
# na.omit(alljoin) --> takes only completed rows 
# alljoin <- alljoin %>% drop_na(landings, speed, make_code, drive_type, time, loadperpax, building_age)

alljoin <- prejoin %>% 
  left_join(lift, by= "lift_id") %>% 
  left_join(building, by = "building_id") %>%
  left_join(company, by ="mco_id")
  
# VARIABLES: 
  # 1) lift: lift_id, mco_id, building_id, landings, speed, commission_date, load_capacity, person_capacity,
  #          drive_type, storey, car_control_1, car_control_2, dvr_type
  # 2) building: building_id, town_council_code, storey, building_type, building_date, total_dwell_unit
  # 3) company: mco_id, name 


# PREDICTIVE MODELING -----------------------------------------------------

# prepare data for modeling
alljoin2 <- alljoin[, -which(names(alljoin) %in% c("lift_id", "rn", "insert_date", "report_desc", "Remarks", "building_type"))]
alljoin2 <- alljoin2 %>% select(sevendays, everything()) 

# Datacamp  ---------------------------------------------------------------

# jumble up the data
rows <- sample(nrow(alljoin2))
alljoin2 <- alljoin2[rows,]
alljoin2 <- na.omit(alljoin2)

# 80/20 split
set.seed(42)
split <- round(nrow(alljoin2) * .7)
train <- alljoin2[1:split,]
test <- alljoin2[(split+1):nrow(alljoin2),]

# logistic regression
require(caret)
glm_model <- glm(sevendays~., family = "binomial", alljoin2)
p <- predict(glm_model, test, type= "response")
p_class <- ifelse(p>0.5, "BD", "OK") #threshold: 0.1 means high true positive and 
                                     #                     high false positive
                                    #threshold: 0.9 means low true positive and 
                                    #                     low false positive
                                                      
confusionMatrix(predicted, actual)

#ROC 
colAUC(predicted_probabilities, actual, plotROC = TRUE)

# train data
require(caret)
model_1 <- train(
  sevendays~., alljoin2,
  method = "rf",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)




# 1) Random Forest Modeling ----------------------------------------------------------------

library(randomForest)

# Split data 
na.omit(alljoin2)
n <- nrow(alljoin2)
set.seed(1212)
trainIndex <- sample(1:n, size = round(0.7*n), replace=FALSE)
train <- alljoin2[trainIndex, ]
test <- alljoin2[-trainIndex, ]

# Use randomForest package 
rf.train.1 <- train[, c("landings", "speed", "make_code", "drive_type", "time", "loadperpax", "building_age")]
rf.label <- train[, "Remarks"]
set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1) #speed has no predictive power. prolly due to uneven distribution  

# Use Caret package
require(caret)
set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)
table(rf.label)






# ARIMA time series -------------------------------------------------------

library(forecast)
library(tseries)




# Abandoned codes

myfunc <- function(x){
  with(x, sapply(f_date, function(y) 
    sum(breakdown[f_date < f_date(y)])))
}

test <- alljoin %>%
  do(data.frame(., past_breakdowns = myfunc(.))) %>%
  select(c(past_breakdowns, lift_id))




test2 <- alljoin %>% 
  group_by(lift_id, f_date) %>%
  arrange(f_date) %>%
  summarise(count_day = sum(breakdown)) %>%
  mutate(hist_count = cumsum(count_day))
