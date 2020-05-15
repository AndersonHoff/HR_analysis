#if(!require(dplyr)) {install.packages("dplyr")} else {library(dplyr)}

###### PREPARING DATA #######################

traindata=read.csv("train_data.csv",stringsAsFactors = F,header = T)
str(traindata)
head(traindata)
glimpse(traindata)

traindata <- traindata %>% 
  select(is_promoted, everything())

colnames(traindata) <- c("Is_Promoted", "Employee_ID", "Department", "Region", "Education", 
                         "Gender", "Recruitment_Channel", "Number_of_Trainings", "Age", 
                         "Previous_Year_Rating", "Length_of_Service", "KPIs_met_80", "Awards_Won", 
                         "Average_Training_Score")

for(i in 1:14){
  if(class(traindata[,i])=="character")
  {traindata[,i]=as.factor(traindata[,i]) }
}

traindata$Is_Promoted <- as.factor(traindata$Is_Promoted)
traindata$KPIs_met_80 <- as.factor(traindata$KPIs_met_80)
traindata$Awards_Won <- as.factor(traindata$Awards_Won)

for(i in 1:14){
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

######### Save prepared data 

write.csv(traindata, file="HR_traindata.csv", quote = FALSE, row.names=F)
