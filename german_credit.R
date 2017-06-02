set.seed(1555)
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library('caret') #ml algorithm

#Import
data(GermanCredit)
cs<-GermanCredit
str(cs)

#Changing the labels for the factor Class
cs<- cs %>% mutate(Class=factor(Class, labels=c(0,1)))


#Creating training and test sets
index<-sample(nrow(cs), floor(0.7*nrow(cs)))


train_cs<-cs[index,-1]
test_cs<-cs[-index,-1]


#Checking for N/As

if(sum(is.na(cs))!=0){
  na_count <-sapply(cs, function(y) sum(length(which(is.na(y)))))
  nac<-as.data.frame(na_count)
  nac
}else{
  cat("BYE!")
}


# Build the model

#Random Forest Model 
rf_model <- train(factor(Class) ~ .,
                  data = train_cs, method="cforest", trControl=myControl)

#Neural network Model 

nn_model<-train(factor(Class) ~ .,data=train_cs,
                method='nnet', trControl=myControl)

#XGboost Model
 
xgbmodel <-  train(factor(Class) ~ .,
                   data=train_cs, method="xgbTree" )
#Gradient boosting Model
gbm_model <- train(factor(Class) ~ .,
                   data = train_cs, method="gbm", trControl=myControl)


# Predict on the train set

prediction_RF_train<-predict(rf_model, train_cs, type='prob')
pred_rf_train<-as.data.frame(prediction_RF_train)

prediction_glm_train<-predict(glm_model, train_cs, type='prob')
pred_glm_train<-as.data.frame(prediction_glm_train)

prediction_nn_train<-predict(nn_model, train_cs, type='prob')
pred_nn_train<-as.data.frame(prediction_nn_train)

prediction_xgb_train<-predict(xgbmodel, train_cs, type='prob')
pred_xgb_train<-as.data.frame(prediction_xgb_train)

prediction_gbm_train<-predict(gbm_model, train_cs, type='prob')
pred_gbm_train<-as.data.frame(prediction_gbm_train)

# Predict on the test set

prediction_RF_test<-predict(rf_model, test_cs, type='prob')
pred_rf_test<-as.data.frame(prediction_RF_test)

prediction_glm_test<-predict(glm_model, test_cs, type='prob')
pred_glm_test<-as.data.frame(prediction_glm_test)

prediction_nn_test<-predict(nn_model, test_cs, type='prob')
pred_nn_test<-as.data.frame(prediction_nn_test)

prediction_xgb_test<-predict(xgbmodel, test_cs, type='prob')
pred_xgb_test<-as.data.frame(prediction_xgb_test)

prediction_gbm_test<-predict(gbm_model, test_cs, type='prob')
pred_gbm_test<-as.data.frame(prediction_gbm_test)

#Build Ensemble Model

train_ensemble<-cbind(train_cs, rf=pred_rf_train$`1`, 
                      
                      nn=pred_nn_train$`1`, xgb=pred_xgb_train$`1`)

rf_ensemble <- randomForest(factor(Class) ~ rf  + nn + xgb,
                            data = train_ensemble, ntree=10000, forest=TRUE)

# Get importance
imp<-function(x){
  importance    <- importance(x)
  varImportance <- data.frame(Variables = row.names(importance), 
                              Importance = round(importance[ ,'MeanDecreaseGini'],2))
  
  # Create a rank variable based on importance
  rankImportance <- varImportance %>%
    mutate(Rank = paste0('#',dense_rank(desc(Importance))))
  
  # Use ggplot2 to visualize the relative importance of variables
  ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                             y = Importance, fill = Importance)) +
    geom_bar(stat='identity') + 
    geom_text(aes(x = Variables, y = 0.5, label = Rank),
              hjust=0, vjust=0.55, size = 4, colour = 'red') +
    labs(x = 'Variables') +
    coord_flip() + 
    theme_few()
}

#Building datasets for ensemble predictions

test_ensemble<-cbind(test_cs, rf = pred_rf_test$`1`, nn = pred_nn_test$`1`, xgb = pred_xgb_test$`1`)

train_ensemble<-cbind(train_cs, rf = pred_rf_train$`1`, nn = pred_nn_train$`1`, xgb = pred_xgb_train$`1`)


#Predict the ensemble model on training and test sets

pred_ensemble<-predict(rf_ensemble, test_ensemble, type='prob')

pred_ensemble2<-predict(rf_ensemble, train_ensemble, type='prob')

pred_fin<-as.data.frame(pred_ensemble)

pred_fin2<-as.data.frame(pred_ensemble2)

#Final test set with preictions

final<-cbind(test_ensemble, pred=pred_fin$`1`)

#Final train set with preictions

finagle<-cbind(train_ensemble, pred=pred_fin2$`1`)

#Writing the test set results
write.csv(final,"new_cs_ml_german.csv")

final2<-rbind(final, finagle)

#Writing results for the entire dataset
write.csv(final2, "new_cs_ml_german_2.csv")

