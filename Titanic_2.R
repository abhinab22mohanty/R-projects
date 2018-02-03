#Reading datafiles into dataframes
setwd("~/R Datasets")
titanic.train <- read.csv('train.csv',stringsAsFactors = FALSE,header = TRUE)
titanic.test <- read.csv('test.csv',stringsAsFactors = FALSE,header = TRUE)

#Setting seperation column for training and test dataset  
titanic.train$IsTrainSet <- TRUE
titanic.test$Survived <- NA
titanic.test$IsTrainSet <- FALSE

titanic.total <- rbind(titanic.train,titanic.test)

titanic.total[titanic.total$Embarked=='','Embarked'] <-'S'

#To check if there are any zero values
#titanic.total[titanic.total$Fare==0 | is.na(titanic.total$Fare),'Fare']

#Median Fare for passengers segregated per Pclass
aggregate(Fare~Pclass,titanic.total,median)

#Calculate median for each Pcalss seperately
#if(titanic.train$Pclass==1)
fare.median.Pclass1 <- median(subset(titanic.total$Fare,titanic.total$Pclass==1))
fare.median.Pclass2 <- median(subset(titanic.total$Fare,titanic.total$Pclass==2))
fare.median.Pclass3 <- median(subset(titanic.total$Fare,titanic.total$Pclass==3),na.rm = TRUE)

#Adding values to the zero Fares Pclasswise

titanic.total[is.na(titanic.total$Fare),"Fare"] <- 0

titanic.total[(titanic.total$Fare==0)& as.numeric(titanic.total$Pclass)==1,"Fare"] <- fare.median.Pclass1
titanic.total[(titanic.total$Fare==0)& as.numeric(titanic.total$Pclass)==2,"Fare"] <- fare.median.Pclass2
titanic.total[(titanic.total$Fare==0)& as.numeric(titanic.total$Pclass)==3,"Fare"] <- fare.median.Pclass3



#Seperating Titles from names
titanic.total$Title <- ifelse((grepl("Mr.",titanic.total$Name,ignore.case = TRUE) & !grepl("Mrs.",titanic.total$Name,ignore.case = TRUE)),"Mr",
                              ifelse((grepl("Mrs.",titanic.total$Name,ignore.case = TRUE)),"Mrs",
                                     ifelse((grepl("Miss",titanic.total$Name,ignore.case = TRUE)),"Miss",
                                            ifelse((grepl("Master",titanic.total$Name,ignore.case = TRUE)),"Master","Others"))))

titanic.total$Title <- as.factor(titanic.total$Title)
titanic.total$Pclass <- as.factor(titanic.total$Pclass)
titanic.total$Sex <- as.factor(titanic.total$Sex)


#Age prediction model
age.model <- lm(Age ~ Title + Pclass + Sex + SibSp-1, data = titanic.total)
summary(age.model)
#titanic.train$testAge <- predict(age.model,newdata = titanic.train)

for(i in 1:nrow(titanic.total))
{
  if(is.na(titanic.total[i,"Age"]))
  {
    titanic.total[i,"Age"] <- round(predict(age.model,newdata = titanic.total[i,]))
  }
}

#Converting negative Age values to positive
titanic.total[which(titanic.total$Age<0),'Age'] <- abs(titanic.total[which(titanic.total$Age<0),'Age'])

#creating column Age Class as indicator column for children and adults

titanic.total$AgeClass <- ifelse(((titanic.total$Age > 0 & titanic.total$Age <=13)),"Juvenile",
                                 ifelse(((titanic.total$Age >13)),"Adult","Others"))

titanic.total$AgeClass <- as.factor(titanic.total$AgeClass)

titanic.total$FamilySize <- titanic.total$SibSp + titanic.total$Parch + 1
titanic.total$FamilyCat <- cut(titanic.total$FamilySize,c(0,1,4,11))

#factorizing columns to total dataframe

#titanic.actual.train <- titanic.train[,c("Survived","FamilyCat","Pclass","Sex","Title","AgeClass")]
titanic.total$Survived <- as.factor(titanic.total$Survived)
titanic.total$FamilyCat <- as.factor(titanic.total$FamilyCat)
titanic.total$Pclass <- as.factor(titanic.total$Pclass)
titanic.total$Sex <- as.factor(titanic.total$Sex)
titanic.total$Title <- as.factor(titanic.total$Title)
titanic.total$AgeClass <- as.factor(titanic.total$AgeClass)

#tuneRF(titanic.actual.train[-1],titanic.actual.train$Survived, ntreeTry=100, stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)

#Data Split
titanic.test<- titanic.total[titanic.total$IsTrainSet==FALSE,]
titanic.train<- titanic.total[titanic.total$IsTrainSet==TRUE,]

#write.xlsx(titanic.total, file = "Total_Titanic.xlsx", row.names = FALSE)
#A randomForest model for titanic prediction

library(randomForest)
titanic.model <- randomForest(Survived~Pclass+Sex+Title+AgeClass+FamilyCat,data = titanic.train,type = classification,ntree=500,mtry=2)
titanic.prediction <- predict(titanic.model, newdata = titanic.test)


#Creating the output dataframe
PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- titanic.prediction 

write.csv(output.df, file = "kaggle_titanic.csv", row.names = FALSE)
