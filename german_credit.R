set.seed(1555)
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library('caret') #ml algorithm


data(GermanCredit)
cs<-GermanCredit
str(cs)
nrow(cs)
index<-sample(nrow(cs), floor(0.7*nrow(cs)))

cs<- cs %>% mutate(Class=factor(Class, labels=c(0,1)))

train_cs<-cs[index,-1]
test_cs<-cs[-index,-1]

if(sum(is.na(cs))!=0){
  na_count <-sapply(cs, function(y) sum(length(which(is.na(y)))))
  nac<-as.data.frame(na_count)
  nac
}else{
  cat("BYE!")
}

str(train_cs)


View(cs)
# Build the model (note: not all possible variables are used)
rf_model <- train(factor(Class) ~ .,
                  data = train_cs, method="cforest", trControl=myControl)



nn_model<-train(factor(Class) ~ .,data=train_cs,
                method='nnet', trControl=myControl)


xgbmodel <-  train(factor(Class) ~ .,
                   data=train_cs, method="xgbTree" )

gbm_model <- train(factor(Class) ~ .,
                   data = train_cs, method="gbm", trControl=myControl)


# Predict using the train set
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

# Predict using the test set
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
 importance(rf_ensemble)
test_ensemble<-cbind(test_cs, rf = pred_rf_test$`1`, nn = pred_nn_test$`1`, xgb = pred_xgb_test$`1`
                     )

train_ensemble<-cbind(train_cs, rf = pred_rf_train$`1`, nn = pred_nn_train$`1`, xgb = pred_xgb_train$`1`
                      )

pred_ensemble<-predict(rf_ensemble, test_ensemble, type='prob')

pred_ensemble2<-predict(rf_ensemble, train_ensemble, type='prob')

pred_fin<-as.data.frame(pred_ensemble)

pred_fin2<-as.data.frame(pred_ensemble2)

final<-cbind(test_ensemble, pred=pred_fin$`1`)

pred_binary<-vector("numeric", nrow(final))
View(cbind(final, pred_binary, flag))


for(i in 1:nrow(final)){
  if(final$pred[i]>=0.4){
    pred_binary[i] <- 1
  }else{
    pred_binary[i] <- 0
  }
}
  
  
  flag<-vector("numeric", nrow(final))
  
for(i in 1:nrow(final)){
  if(final$Class[i]==pred_binary[i]){
      flag[i] <- 1
  }else{
      flag[i] <- 0
  }
}

auc(roc(test_cs$Class, test_cs$Class))

finagle<-cbind(train_ensemble, pred=pred_fin2$`1`)

write.csv(final,"new_cs_ml_german.csv")

final2<-rbind(final, finagle)

write.csv(final2, "new_cs_ml_german_2.csv")

