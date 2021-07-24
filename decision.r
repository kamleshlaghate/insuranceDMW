
library(e1071)
#rpart is used for decision tree making
library(rpart)
library(caTools)

mydata1<-read.csv(file = "/home/hp/Desktop/insuranceDMW/insurance2.csv")
#pick up only column 1,4,6,9 from dataset
subset_mydata<-mydata1[,c(1,3,4,5,8)]

View(subset_mydata)

#split the dataset

temp_field<-sample.split(subset_mydata,SplitRatio = 0.9)
train<-subset(subset_mydata,temp_field==TRUE)
test<-subset(subset_mydata,temp_field==FALSE)

summary(train)
summary(test)

head(train)
head(test)

fit<-rpart(train$insuranceclaim~.,data = train,method = "class")
plot(fit)
text(fit)

#test data excluding last column
preds<-predict(fit,newdata=test[,-8],type=("class"))
mean(preds==test$insuranceclaim)

#display output of test data

output1<-cbind(test,preds)
View(output1)
output1
confusionMatrix(table(preds,test$insuranceclaim,dnn=c("predicted","actual")))

#cp for post pruning


printcp(fit)

#gate the value of cp with min xerror

opt<-which.min(fit$cptable[,"xerror"])
cp<-fit$cptable[opt,"CP"]
cp

#prune tree
pruned_model<-prune(fit,cp)
plot(fit)
text(fit)
rpart_pruned_predict<-predict(pruned_model,test[,-8],type="class")


mean(rpart_pruned_predict==test$insuranceclaim)

output1<-cbind(test,rpart_pruned_predict)

View(output1)



#Random Forest
model_rf <- train(insuranceclaim~.,train,method="ranger",metric="ROC",preProcess = c('center', 'scale'),trControl=fitControl)
pred_rf <- predict(model_rf, test_data)
print(pred_rf)
cm_rf <- confusionMatrix(pred_rf, test_data$diagnosis, positive = "M")
print(cm_rf)
pred1<-predict(model_rf,to_pred)
print(pred1)


##################################################### neural network ###################################################################
#max = apply(data, 2 , max)
#min = apply(data, 2 , min)
#dataset = as.data.frame(scale(data, center = min, scale = max - min))
#set.seed(123)
#split = sample.split(dataset$insuranceclaim, SplitRatio = 0.8)
#training_set = subset(dataset, split == TRUE)
#test_set = subset(dataset, split == FALSE)
#install.packages("neuralnet")
#library(neuralnet)
#set.seed(2)
#Neural_Net = neuralnet(formula = insuranceclaim ~ age + smoker + children + region + bmi + sex + charges, data =  training_set, hidden = c(6,6) , linear.output = F)
#plot(Neural_Net)
#test_prediction = compute(Neural_Net, test_set[,-8])
#test_prediction = (test_prediction[net.result] * (max(dataset$insuranceclaim) - min(dataset$insuranceclaim))) + min(dataset$insuranceclaim)
#confusionMatrix(table(test_prediction ,test_set$insuranceclaim,dnn=c("predicted","actual")))
#results <- data.frame(actual = test_set$insuranceclaim, prediction = test_prediction$net.result)
#print(results)




# MAX-MIN NORMALIZATION
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
maxmindf <- as.data.frame(lapply(data, normalize))

# TRAINING AND TEST DATA
trainset <- maxmindf[1:1000, ]
testset <- maxmindf[1001:1334, ]
#4. NEURAL NETWORK

nn <- neuralnet(insuranceclaim ~ age + smoker + children + region + bmi + charges,data=trainset, hidden=c(7,7), linear.output=F, threshold=0.01)
nn$result.matrix
results <- data.frame(actual = results$actual, prediction = results$prediction)
results

predicted=results$prediction * abs(diff(range(dataset$insuranceclaim))) + min(dataset$insuranceclaim)
actual=results$actual * abs(diff(range(dataset$insuranceclaim))) + min(dataset$insuranceclaim)
comparison=data.frame(predicted,actual)
deviation=((actual-predicted)/actual)
comparison=data.frame(predicted,actual,deviation)
accuracy=1-abs(mean(deviation))
accuracy

