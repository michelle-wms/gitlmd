library(dplyr)
library(stringr)
library(ggplot2)
library(lattice)
library(data.table)
library(readbulk)
library(caret)
library(here)

#here::here()
#save.image(file=".Rdata")    load(file = "lmd.rda") 

# Cleaning ----------------------------------------------------------------

##############-----------------  COMPANY

company <- read.csv("LMD Data/Company.csv")
company$mco_id <- as.factor(company$mco_id)
company <- company[,c(1,3)]

# variables: mco_id, name 


##############-----------------  FAULTDESC

faultdesc <- read.csv("LMD Data/Fault_Desc.csv")

colnames(faultdesc)[1] <- "f_nbr"
faultdesc$f_nbr <- as.factor(faultdesc$f_nbr)

faultdesc<- faultdesc %>% 
  filter(Remarks == "Breakdown", Mechanical_or_Human != "h") %>%
  select(f_nbr, Remarks) 

# variables: f_nbr, Remarks 


##############----------------- lifterror

#path <- "Lift Error Data - 2012 to 2017"

#multmerge = function(path){
#  filenames=list.files(path=path, full.names=TRUE)
#  rbindlist(lapply(filenames, fread))
#}

#lifterror <- multmerge(path)

#saveRDS(lifterror, here("lifterror.rds"))
lifterror <- readRDS(here("lifterror.rds"))

lerror <- lifterror[,c(8,11,12,26)]

colnames(lerror)[which(names(lerror) == "fault_nbr")] <- "f_nbr" #lerror$f_nbr <- gsub('.{2}$', '', lerror$f_nbr)  
                                                                  #remove last 2 chars

lerror <- lerror %>% 
  mutate(lift_id = as.character(lift_id)) %>%
  mutate(f_nbr = as.factor(f_nbr)) %>%
  mutate(report_desc = as.character(report_desc))

# variables: lift_id, f_nbr, report_desc, postal_code, date 


##############-----------------  LIFT

lift <- read.csv("LMD Data/Lifts Oct17.csv")
lift_full <- read.csv("LMD Data/Lifts Oct17.csv")

keepcol <- c("lift_id", "make_code", "mco_id", "building_id", "landings", "speed", "commission_date", "load_capacity", "person_capacity",
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

# variables: lift_id, mco_id, building_id, lmd_comm_date, landings, speed, commission_date, load_capacity, person_capacity,
#drive_type, storey, car_control_1, car_control_2, dvr_type



##############----------------- BUILDING

building <- read.csv("LMD Data/Buildings Oct17.csv")

keepcol2 <- c("building_id", "town_council_code", "storey", "building_type", "building_date", "total_dwell_unit")
building <- building[, names(building) %in% keepcol2]

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
  filter(Remarks == "Breakdown") 

lerror_faultdesc$breakdown <- ifelse(lerror_faultdesc$Remarks=="Breakdown", 1, 0) 

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

prejoin <- lerror_faultdesc %>%
  mutate(f_day = weekdays(f_date)) %>%
  mutate(f_day = as.factor(f_day)) %>% # 1) monday to sunday
  mutate(f_hour = format(f_time, "%H")) %>%
  mutate(f_hour = as.numeric(f_hour)) %>% # # 2) time of fault (am/pm)
  mutate(f_mth = as.factor(format(f_time, "%m"))) %>% # month
  mutate(f_day = as.numeric(format(f_time, "%d"))) %>%  # day
  group_by(lift_id) %>%
  arrange(lift_id, f_date) %>%
  mutate(hist_count = cumsum(breakdown)) # 4) no. days from previous breakdown     

# 5) average days it takes for a breakdown to occur from commission to the fault date (lower = bad) 
prejoin <- prejoin %>% 
  mutate(freq = as.numeric((f_date - min(f_date))/hist_count)) 

# Y2 = BD in next 7 days (y/n)
prejoin <- prejoin %>% 
  mutate(sevendays = ifelse((lead(f_date)-f_date)<8 & (lead(f_date)-f_date)>0, 1, 0))

# Remove prejoin lifts which do not appear in lifts oct 17 table"
uniquelift<- unique(lift$lift_id)
prejoin2 <- prejoin[prejoin$lift_id %in% uniquelift, ]

# Joining together  -------------------------------------------------------------

# Inspect NAs
# sapply(alljoin, function(x) ifelse(sum(is.na(x))/nrow(alljoin)<0.05, "Y", "N"))> --> % of NAs in each col
# na.omit(alljoin) --> takes only completed rows 
# alljoin <- alljoin %>% drop_na(landings, speed, make_code, drive_type, time, loadperpax, building_age)

alljoin <- prejoin2 %>% 
  left_join(lift, by= "lift_id") %>% 
  left_join(building, by = "building_id") %>%
  left_join(company, by ="mco_id") %>%
  mutate(building_age = as.numeric(f_date - building_date))

# VARIABLES: 
# 1) lift: lift_id, mco_id, building_id, landings, speed, commission_date, load_capacity, person_capacity,
#          drive_type, storey, car_control_1, car_control_2, dvr_type
# 2) building: building_id, town_council_code, storey, building_type, building_date, total_dwell_unit
# 3) company: mco_id, name 

# pax per landing
alljoin <- alljoin %>%
  mutate(unitsperlanding = total_dwell_unit/landings)

# prepare data for modeling
alljoin_train <- alljoin[, -which(names(alljoin) %in% c("mco_id", "car_control_1","car_control_2","days_nextbd","f_date","building_date","f_time","comm_date","building_id","f_nbr","breakdown","lift_id", "rn", "insert_date", "report_desc", "Remarks", "building_type", "town_council_code", "storey", "f_day", "f_hour", "f_mth"))]

alljoin_train <- alljoin_train %>% 
  select(sevendays, everything()) %>%  # select sevendays as first col, then everything after
  mutate(sevendays = as.factor(sevendays)) %>%
  rename(mco_name = short_name) 




## Preparing training data  -------------------------------------------------

# jumble up the data
# rows <- sample(nrow(alljoin_train))
# alljoin_train <- alljoin_train[rows,]
# alljoin_train <- na.omit(alljoin_train)
#levels(alljoin2$sevendays) <- make.names(levels(factor(alljoin2$sevendays)))

# 70/30 split
set.seed(42)
alljoin_sampled <- sample_frac(alljoin_train, 0.1)
alljoin_sampled <- na.omit(alljoin_sampled)
split <- round(nrow(alljoin_sampled) * .7)
train <- alljoin_sampled[1:split,]
test <- alljoin_sampled[(split+1):nrow(alljoin_sampled),]

# Downsampling
set.seed(1103)
ds_train <- downSample(x = train[,-1], y = train$sevendays , yname = "sevendays")
table(ds_train$sevendays)

library(DMwR)
set.seed(1103)
smote_train <- SMOTE(sevendays ~ ., data = train)
table(smote_train$sevendays)



## Modeling ----------------------------------------------------------------

glm.fit <- glm(sevendays ~., data = train, family = binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, type = "response") 
glm.probs[1:5]
glm.pred = ifelse(glm.probs>0.5, 1, 0)
table(glm.pred, train$sevendays)


# Gradient boosting
library(xgboost)
set.seed(100)
predictors <- data.matrix(train[, -1]) 
label <- as.numeric(train[["sevendays"]])-1 

xgb <- xgboost(data=predictors, label=label,
               objective = "binary:logistic",
               subsample=.63, 
               eta=0.1,
               nrounds=100,
               eval_metric = "auc")
xgb

pred_prob <- predict(xgb, data.matrix(test[, -1]))
prediction <- as.numeric(pred_prob > 0.5)
testlabel<- as.numeric(test[["sevendays"]])-1 

require(caret)
confusionMatrix(prediction, testlabel)
require(caTools)
colAUC(pred_prob, testlabel, plotROC = TRUE)

# Cross validation 
set.seed(100)
cv <- xgb.cv(data=predictors, label=label,
             objective = "binary:logistic",
             subsample=.63, 
             eta=0.1,
             nrounds=100,
             nfold = 5,
             
             eval_metric = "auc")
pred_prob <- predict(cv, data.matrix(test[, -1]))
prediction <- as.numeric(pred_prob > 0.6)
testlabel<- as.numeric(test[["sevendays"]])-1 

require(caret)
confusionMatrix(prediction, testlabel)
require(caTools)
colAUC(pred_prob, testlabel, plotROC = TRUE)


# Using tunegrid to find best mtry
model <- train(
  quality~.,
  tuneGrid = data.frame(mtry = c(2,3,7)),
  data = wine, method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)
plot(model)



#Confusion matrix                                                      
confusionMatrix(predicted, actual)

#ROC 
colAUC(predicted_probabilities, test[["Class"]], plotROC = TRUE)

























# # Decision Tree (rpart) -------------------------------------------------

set.seed(100)
library(rpart)
tree <- rpart(sevendays ~., train, control=rpart.control(cp=0.001))
plot(tree, uniform=TRUE, margin=0.5)
text(tree)
tree                   


set.seed(100)
library(randomForest)
rf <- randomForest(sevendays~., train) 
error_df <- data.frame(error_rate = rf$err.rate[,"OOB"], num_trees = 1:rf$ntree)
ggplot(error_df, aes(x=num_trees, y=error_rate)) + geom_line()



# 1) Random Forest Modeling ----------------------------------------------------------------

require(randomForest)

rf.train.1 <- train[, -1]
rf.label <- train[, 1]
set.seed(1234)
rf.1 <- randomForest(sevendays~., train, importance = TRUE, ntree = 200)
rf.1
varImpPlot(rf.1) #speed has no predictive power. prolly due to uneven distribution  

require(caret)

set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)
table(rf.label)






# ARIMA time series -------------------------------------------------------

library(forecast)
library(tseries)

# OTHER codes

# how many lifts in 'lift' table does not appear in 'lerror' table at all?
require(data.table)
setDT(lift,lerror)
setkey(lift,lift_id)
setkey(lerror,lift_id)
lift[!lerror]

# count of breakdowns by day
date_breakdown <- prejoin %>% 
  tally(breakdown ==1)

# logistic regression
require(caret)
glm_model <- glm(sevendays~., family = "binomial", alljoin2)
p <- predict(glm_model, test, type= "response")
p_class <- ifelse(p>0.5, "BD", "OK") #threshold: 0.1 means high true positive and high false positive







library(doParallel)
nThreads <- detectCores(logical = TRUE)
cl <- makeCluster(nThreads)
registerDoParallel(cl)
#fit1 <- train(bbbDescr, logBBB, "rf")
stopCluster(cl)
registerDoSEQ()
