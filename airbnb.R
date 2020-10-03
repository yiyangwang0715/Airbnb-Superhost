setwd("C:\\Users\\eyon0\\OneDrive\\桌面\\BUMK746")
train<-read.csv('train0429.csv', header = T)
test<-read.csv('test0429.csv', header = T)
train=train[ ,-1]

ls(train)

library(dplyr)
numeric <- c("accommodates","availability_365","calculated_host_listings_count","extra_people"                  
         ,"guests_included"  ,              "host_id"                       
        ,"host_listings_count"      ,      "host_response_rate"            
        , "id"            ,         "number_of_reviews"             
        ,"policy"  ,                       "price"                         
         ,                   "review_scores_accuracy"        
         ,"review_scores_checkin"  ,        "review_scores_cleanliness"     
        , "review_scores_communication",    "review_scores_location"        
         ,"review_scores_rating"       ,    "review_scores_value"           
        , "reviews_per_month"             )

factor <- c("host_identity_verified"  ,       "host_is_superhost", "instant_bookable"              
        , "is_location_exact" ,"res.rate.na")
train[,factor] <- lapply(train[,factor], factor)
train[,numeric] <- lapply(train[,numeric], as.numeric)

test[,factor] <- lapply(test[,factor], factor)
test[,numeric] <- lapply(test[,numeric], as.numeric)

#Random forest, accuracy on test:0.8, auc:0.79
lapply(train, class)
lapply(test, class)


library(randomForest)


RF<-randomForest(host_is_superhost~ availability_365+      cancellation_policy+             
                host_identity_verified+     host_response_rate    
                            + instant_bookable    +is_location_exact     
                 +number_of_reviews+     review_scores_rating, data= train, importance=TRUE, na.action=na.omit,
                 ntree=10)
plot(RF)
print(RF)
varImpPlot(RF)
predrf<- predict(RF, test)
predrf
PRF<-cbind(test$host_is_superhost,predrf)

library(caret)
Predict_RF_Train = predict(RF,newdata = train, type = "class")
confusionMatrix(Predict_RF_Train, as.factor(train$host_is_superhost))

Predict_TEST = predict(RF, newdata = test)
confusionMatrix(Predict_TEST, as.factor(test$host_is_superhost))

library(ROCR)
library(gplots)
pre=predict(RF,newdata=test,type='response')
pred <- prediction(as.numeric(pre)-1,as.numeric(test$host_is_superhost))
performance(pred,'auc')@y.values
perf <- performance(pred,'tpr','fpr')
plot(perf)
abline(a=0,b=1)
perf_lift<- performance(pred,"lift","rpp")
plot(perf_lift, main="lift curve", colorize=T)

##logistic regression (predict error)
LR = glm(host_is_superhost~ availability_365+      cancellation_policy+             
            host_identity_verified+     host_response_rate    
          + instant_bookable    +is_location_exact     
          +number_of_reviews+     review_scores_rating, family=binomial (link='logit'), data=train)
LR

#test
PredictModel<-predict(LR, test, type="response",na.action=)
probability<- cbind(test$host_is_superhost,PredictModel)
list<-probability[order(probability[,2],decreasing=TRUE),]

#Gradient Boosting Model, auc=0.833 
library(gbm)
train$superhost<-ifelse(train$host_is_superhost=="t", 1, 0)
test$superhost<-ifelse(test$host_is_superhost=="t", 1, 0)

 GM= gbm(superhost~ availability_365+      cancellation_policy+             
          host_identity_verified+     host_response_rate    
        + instant_bookable    +is_location_exact     
        +number_of_reviews+     review_scores_rating, distribution="bernoulli", data=train, n.trees=200)
GMPredict=predict.gbm( object= GM,newdata=test, n.trees=200, type="response")
GMPredict
#testdata
PGBM<-cbind(test$superhost,GMPredict)
library(ROCR)
library(gplots)
pred <- prediction(as.numeric(GMPredict)-1,as.numeric(test$superhost))
performance(pred,'auc')@y.values
perf <- performance(pred,'tpr','fpr')
plot(perf)
abline(a=0,b=1)


