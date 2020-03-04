if(!require(dplyr)) {install.packages("dplyr")} else {library(dplyr)}

###### PREPARING DATA #######################

traindata=read.csv("train_data.csv",stringsAsFactors = F,header = T)
str(traindata)

traindata <- traindata %>% 
  select(is_promoted, everything())

for(i in 1:14){
  if(class(traindata[,i])=="character")
  {traindata[,i]=as.factor(traindata[,i]) }
}

for(i in 1:14){
  if(class(traindata[,i])=="integer")
  {traindata[,i]=as.numeric(traindata[,i])}
}

rm(i)
####Ver quantidade de NAs na coluna

sum(is.na(traindata$no_of_trainings))
sum(is.na(traindata$previous_year_rating))
sum(is.na(traindata$age))
sum(is.na(traindata$length_of_service))
sum(is.na(traindata$KPIs_met..80.))
sum(is.na(traindata$awards_won.))
sum(is.na(traindata$avg_training_score))

levels(traindata$department)
levels(traindata$region)
levels(traindata$education)
levels(traindata$gender)
levels(traindata$recruitment_channel)

####### Insere 0 para NA na coluna "previous_year_rating"

traindata$previous_year_rating[is.na(traindata$previous_year_rating)] <- 0

##Inserir None em education (missing values)

levels(traindata$education)
traindata$education = factor(traindata$education,levels = c(levels(traindata$education),"None"))
levels(traindata$education)[1]
levels(traindata$education)[1] <- NA
traindata$education[is.na(traindata$education)] <- "None"

######### Save prepared data 

colnames(traindata) <- c("Is Promoted", "Employee ID", "Department", "Region", "Education", 
               "Gender", "Recruitment Channel", "Number of Trainings", "Age", 
                "Previous Year Rating", "Length of Service", "KPIs met .80", "Awards Won", 
                "Average Training Score")

write.csv(traindata, file="HR_traindata.csv", quote = FALSE, row.names=F)
