#Data Read
titanic.train <- read.csv('train.csv',stringsAsFactors = FALSE,header = TRUE)
titanic.test <- read.csv('test.csv',stringsAsFactors = FALSE,header = TRUE)

#Data Clean
titanic.train$IsTrainSet <- TRUE
titanic.test$Survived <- NA
titanic.test$IsTrainSet <- FALSE

titanic.total <- rbind(titanic.train,titanic.test)

#To check whether a particular variable has null or blank values using tables
#titanic.total[titanic.total$Embarked==''|is.na(titanic.total$Embarked),'Embarked']


titanic.total[titanic.total$Embarked=='','Embarked'] <-'S'

age.median <- median(titanic.total$Age,na.rm = TRUE)
titanic.total[is.na(titanic.total$Age),'Age'] <- age.median

fare.median <- median(titanic.total$Fare,na.rm = TRUE)
titanic.total[is.na(titanic.total$Fare),'Fare'] <- fare.median

#Categorical casting
titanic.total$Embarked <- as.factor(titanic.total$Embarked)
titanic.total$Sex <- as.factor(titanic.total$Sex)
titanic.total$Pclass <- as.factor(titanic.total$Pclass)
titanic.total$Survived <- as.factor(titanic.total$Survived)

#Data Split
titanic.test<- titanic.total[titanic.total$IsTrainSet==FALSE,]
titanic.train<- titanic.total[titanic.total$IsTrainSet==TRUE,]


#Regression model for Survival Rate using randomForest
survived.formula <- as.formula("Survived ~ Pclass + Sex + Age + Fare + SibSp + Parch + Embarked")
titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 5,nodesize = 0.01* nrow(titanic.train))

survived.feature <- "Pclass + Sex + Age + Fare + SibSp + Parch + Embarked"
titanic.prediction <- predict(titanic.model, newdata = titanic.test)

#Creating the output dataframe
PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- titanic.prediction 

write.csv(output.df, file = "kaggle_titanic.csv", row.names = FALSE)
