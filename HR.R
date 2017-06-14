#published: https://www.kaggle.com/gratipine/d/ludobenistant/hr-analytics/employee-retention-prediction-using-random-forest/

#reading in the data
data<-read.csv("HR.csv")

library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(randomForest)
library(corrplot)

set.seed(123)
testIndeces<-sample(nrow(data), floor(nrow(data)*0.33))

train<-data[-testIndeces,]
test<-data[testIndeces,]
rm("data")

#no missing values, so nothing to do there

#Random forest
#chart coordinate table
rf_model<-randomForest(as.factor(left)~satisfaction_level+last_evaluation+
                       number_project + average_montly_hours +
                       time_spend_company + Work_accident + promotion_last_5years + salary,
                       data=train)

plot(rf_model)
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

#Get Importance
importance<-importance(rf_model)
varImportance<-data.frame(Variables=row.names(importance),
                          Importance = round(importance[,"MeanDecreaseGini"],2))


#Create a rank variable based on importance
rankImportance <- varImportance %>%mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip()+
  theme_few()

#equal to test - column for left or no
testForPrediction<-test[,-7]

prediction<-predict(rf_model, testForPrediction)
prediction<-as.numeric(prediction)-1
success<-prediction==test$left
successRate<-length(which(success))/nrow(test)
#compare with the actual train model

rf_model<-randomForest(as.factor(left)~last_evaluation+
                         number_project + average_montly_hours +
                         time_spend_company + Work_accident + promotion_last_5years + salary,
                       data=train)

plot(rf_model)
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

#Get Importance
importance<-importance(rf_model)
varImportance<-data.frame(Variables=row.names(importance),
                          Importance = round(importance[,"MeanDecreaseGini"],2))


#Create a rank variable based on importance
rankImportance <- varImportance %>%mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip()+
  theme_few()


prediction<-predict(rf_model, testForPrediction)
prediction<-as.numeric(prediction)-1
success<-prediction==test$left
successRate<-length(which(success))/nrow(test)

#Correlations

numberOfProjects<-cor(train$number_project, train$left)
train$salary<-as.numeric(train$salary)
num.cols <- sapply(correlData,is.numeric)
correlations<-cor(correlData[,num.cols])
corrplot(correlations, methos="pie")