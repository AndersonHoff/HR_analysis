### HR prediction routines ####
library(magrittr)
library(dplyr)

traindata=read.csv("train_data.csv",stringsAsFactors = F,header = T)
#str(traindata)
#head(traindata)
#glimpse(traindata)

traindata <- traindata %>% 
  select(is_promoted, everything()) %>%
  select(-employee_id)

colnames(traindata) <- c("Is_Promoted", "Department", "Region", "Education", 
                         "Gender", "Recruitment_Channel", "Number_of_Trainings", "Age", 
                         "Previous_Year_Rating", "Length_of_Service", "KPIs_met_80", "Awards_Won", 
                         "Average_Training_Score")

for(i in 1:13){
  if(class(traindata[,i])=="character")
  {traindata[,i]=as.factor(traindata[,i]) }
}

#traindata$Is_Promoted <- as.factor(traindata$Is_Promoted)
traindata$KPIs_met_80 <- as.factor(traindata$KPIs_met_80)
traindata$Awards_Won <- as.factor(traindata$Awards_Won)

for(i in 1:13){
  if(class(traindata[,i])=="integer")
  {traindata[,i]=as.numeric(traindata[,i])}
}

rm(i)
####Ver quantidade de NAs na coluna

sum(is.na(traindata$Number_of_Trainings))
sum(is.na(traindata$Previous_Year_Rating))
sum(is.na(traindata$Age))
sum(is.na(traindata$Length_of_Service))
sum(is.na(traindata$KPIs_met_80))
sum(is.na(traindata$Awards_Won))
sum(is.na(traindata$Average_Training_Score))

levels(traindata$Department)
levels(traindata$Region)
levels(traindata$Education)
levels(traindata$Gender)
levels(traindata$Recruitment_Channel)
levels(traindata$KPIs_met_80)
levels(traindata$Awards_Won)

####### Insere 0 para NA na coluna "previous_year_rating"

traindata$Previous_Year_Rating[is.na(traindata$Previous_Year_Rating)] <- 0

##Inserir None em education (missing values)

levels(traindata$Education)
traindata$Education = factor(traindata$Education,levels = c(levels(traindata$Education),"None"))
levels(traindata$Education)[1]
levels(traindata$Education)[1] <- NA
traindata$Education[is.na(traindata$Education)] <- "None"

####################### CARET #################
library(caret)
library(pROC)

#We’re going to use two models: gbm (Generalized Boosted Models) and 
#glmnet (Generalized Linear Models). Approaching a new data set using 
#different models is one way of getting a handle on your data. Gbm uses 
#boosted trees while glmnet uses regression. 

dataDummy <- dummyVars("~.",data=traindata, fullRank=F)
hrdf <- as.data.frame(predict(dataDummy,traindata))
print(names(hrdf))
prop.table(table(hrdf$Is_Promoted))

hrdf$Is_Promoted <- ifelse(hrdf$Is_Promoted==1,'yes','no')
hrdf$Is_Promoted <- as.factor(hrdf$Is_Promoted)
outcomeName <- 'Is_Promoted'
predictorsNames <- names(hrdf)[names(hrdf) != outcomeName]

set.seed(2020)
splitIndex <- createDataPartition(hrdf[,outcomeName], p = .7, list = FALSE, times = 1)
trainhr <- hrdf[ splitIndex,]
practicehr  <- hrdf[-splitIndex,]

objControl <- trainControl(method='cv', number=10, returnResamp='none', 
                           summaryFunction = twoClassSummary, classProbs = TRUE)

objModel <- train(trainhr[,predictorsNames], trainhr[,outcomeName], 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))

summary(objModel)
print(objModel)

predictions <- predict(object=objModel, practicehr[,predictorsNames], type='raw')
head(predictions)

print(postResample(pred=predictions, obs=as.factor(practicehr[,outcomeName])))
table(predictions)

#############
testdata=read.csv("test_data.csv",stringsAsFactors = F,header = T)

for(i in 1:13){
  if(class(testdata[,i])=="character")
  {
    testdata[,i]=as.factor(testdata[,i])
  }
}

for(i in 1:13){
  if(class(testdata[,i])=="integer")
  {
    testdata[,i]=as.numeric(testdata[,i])
  }
}

sum(is.na(testdata))

sum(is.na(testdata$previous_year_rating))

testdata$previous_year_rating[is.na(testdata$previous_year_rating)] <- 0

levels(testdata$department)
levels(testdata$region)
levels(testdata$education)
levels(testdata$recruitment_channel)
testdata$education = factor(testdata$education, levels = c(levels(testdata$education),"None"))
levels(testdata$education)[1]
levels(testdata$education)[1] <- NA
testdata$education[is.na(testdata$education)] <- "None"

testdata <- testdata %>% 
  select(-employee_id)

colnames(testdata) <- c("Department", "Region", "Education", 
                         "Gender", "Recruitment_Channel", "Number_of_Trainings", "Age", 
                         "Previous_Year_Rating", "Length_of_Service", "KPIs_met_80", "Awards_Won", 
                         "Average_Training_Score")

testdata$KPIs_met_80 <- as.factor(testdata$KPIs_met_80)
testdata$Awards_Won <- as.factor(testdata$Awards_Won)

testDummy <- dummyVars("~.",data=testdata, fullRank=F)
test <- as.data.frame(predict(testDummy,testdata))

predictorsNames2 <- names(test)[names(test) != outcomeName]

testdata$result <- predict(object=objModel, test[,predictorsNames], type='raw')

testdata <- testdata %>%
  mutate(result=ifelse(result=='yes',1,0))

write.table(cbind(test_data$employee_id,testdata$result),file="HR_caret.csv", 
            sep = ",", quote = FALSE, col.names = c('employee_id','is_promoted'), 
            row.names=F)
##### result - 0.43

############################# Neural net #####################
library(neuralnet)
neuraldata <- traindata

str(neuraldata)

normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}

neuraldata$Age <- (neuraldata$Age - min(neuraldata$Age))/
  (max(neuraldata$Age - min(neuraldata$Age)))
neuraldata$Number_of_Trainings <- (neuraldata$Number_of_Trainings - min(neuraldata$Number_of_Trainings))/
  (max(neuraldata$Number_of_Trainings - min(neuraldata$Number_of_Trainings)))
neuraldata$Previous_Year_Rating <- (neuraldata$Previous_Year_Rating - min(neuraldata$Previous_Year_Rating))/
  (max(neuraldata$Previous_Year_Rating - min(neuraldata$Previous_Year_Rating)))
neuraldata$Length_of_Service <- (neuraldata$Length_of_Service - min(neuraldata$Length_of_Service))/
  (max(neuraldata$Length_of_Service - min(neuraldata$Length_of_Service)))
neuraldata$Average_Training_Score <- (neuraldata$Average_Training_Score - min(neuraldata$Average_Training_Score))/
  (max(neuraldata$Average_Training_Score - min(neuraldata$Average_Training_Score)))

neuraldata <- model.matrix(~. ,data = neuraldata)

library(rsample)
set.seed(2020)
split <- initial_split(neuraldata, prop = 0.7)

neuraltrain <- training(split)
neuraltest  <- testing(split)

col_list <- paste(c(colnames(neuraldata[,-c(1,2)])),collapse="+")
col_list <- paste(c("yyes~",col_list),collapse="")
f <- formula(col_list)

nmodel <- neuralnet(Is_Promoted ~ ., data=neuraltrain,
                    hidden=1,
                    threshold = 0.01,
                    learningrate.limit = NULL,
                    learningrate.factor = list(minus = 0.5, plus = 1.2),
                    algorithm = "rprop+")

