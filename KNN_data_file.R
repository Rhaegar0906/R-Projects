suppressWarnings(RNGversion('3.5.3'))
library(readxl)       
library(caret)               
library(gains)
library(pROC)
library(ggplot2)

# Load data
myData <- read_excel("/Users/rhaeagrxing/Downloads/Data_File_2.xlsx")
myScoreData <- read_excel("/Users/rhaeagrxing/Downloads/Score_File2.xlsx")
View(myData)

# Explore data
summary(myData)
str(myData)
pairs(myData)

# Normalize data
myData1 <- scale(myData[,2:4])           
myData1 <- data.frame(myData1, myData$Spam)
colnames(myData1)[4] <- 'Spam'
myData1$Spam <- as.factor(myData1$Spam)
View(myData1)

# Split data
set.seed(1)                         
myIndex <- createDataPartition(myData1$Spam, p=0.6, list=FALSE)
trainSet <- myData1[myIndex,]
validationSet <- myData1[-myIndex,]

# Cross-validation setup
myCtrl <- trainControl(method='cv', number=10)           
myGrid <- expand.grid(.k=c(1:10))

# Train KNN model
set.seed(1)                          
KNN_fit <- train(Spam~., data=trainSet, method='knn', trControl=myCtrl, tuneGrid=myGrid)
print(KNN_fit)

# Evaluate model
KNN_Class <- predict(KNN_fit, newdata=validationSet)
confusionMatrix(KNN_Class, validationSet$Spam, positive='1')

# Probability predictions
KNN_Class_prob <- predict(KNN_fit, newdata=validationSet, type='prob')
KNN_Class1 <- as.factor(ifelse(KNN_Class_prob[,2]>0.75, '1', '0'))
confusionMatrix(KNN_Class1, validationSet$Spam, positive='1')

# Gain and lift analysis
validationSet$Spam <- as.numeric(as.character(validationSet$Spam)) 
gains_table <- gains(validationSet$Spam, KNN_Class_prob[,2])
plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$Spam))~c(0, gains_table$cume.obs), xlab="# of cases", ylab="Cumulative", main="Cumulative Lift Chart", type="l")
lines(c(0, sum(validationSet$Spam))~c(0, dim(validationSet)[1]), col="red", lty=2)

# ROC and AUC
roc_object <- roc(validationSet$Spam, KNN_Class_prob[,2])
plot.roc(roc_object)
auc(roc_object)

# Score using trained model
trainscale <- preProcess(myData[,2:4], method=c('center', 'scale'))    
myScoreData1 <- predict(trainscale, myScoreData)	              
KNN_Score <- predict(KNN_fit, newdata=myScoreData1)	              
myScoreData <- data.frame(myScoreData, KNN_Score)
View(myScoreData)

# Additional EDA
cor(myData1[,1:3])
boxplot(myData1$Spam~myData1$V1, main="Boxplot for V1")
hist(myData1$V1, main="Histogram of V1", xlab="V1", breaks=30)

# Scatter plot
ggplot(myData1, aes(x=V1, y=V2, color=Spam)) + geom_point() + theme_minimal() + ggtitle("Scatterplot of V1 vs V2")

# Pair plot
pairs(myData1[,1:3], main="Pair Plot of Variables")

# Time series plot
timeSeriesData <- ts(myData$V1, frequency=12, start=c(2021,1))
plot(timeSeriesData, main="Time Series of V1")

write.xlsx(myData1, file="/Users/rhaeagrxing/Downloads/Updated_Data_File_2.xlsx")
