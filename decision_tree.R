suppressWarnings(RNGversion("3.5.3"))
library(readxl)
library(caret)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(forecast)
library(randomForest)
library(pROC)
library(gains)

# Data Loading - the start of every great data story
myData <- read_excel("/Users/rhaeagrxing/Downloads/Data_File_1.xlsx")
myScoreData <- read_excel("/Users/rhaeagrxing/Downloads/Score_File.xlsx")
View(myData)  # Exciting to see the raw data!

# Let's get to know our data
summary(myData)  # Quick summary to sniff out any oddities
str(myData)  # Structure - what are we really working with?
pairs(myData)  # Pair plots - a picture is worth a thousand numbers

# Prepping for the magic show (a.k.a modeling)
myData$HELOC <- as.factor(myData$HELOC)  # Factors are friends
set.seed(123)  # Consistency is key
myIndex <- createDataPartition(myData$HELOC, p=0.7, list=FALSE)
trainSet <- myData[myIndex,]
validationSet <- myData[-myIndex,]

# Decision tree - our first act
set.seed(456)  # Good old seed setting
tree_model <- rpart(HELOC ~ ., data=trainSet, method="class")
prp(tree_model, type=1, extra=1, under=TRUE)  # A beautiful tree

# Full Tree vs. Pruned Tree - Battle of the branches
full_tree <- rpart(HELOC ~ ., data=trainSet, method="class", control=rpart.control(minsplit=20))
pruned_tree <- prune(full_tree, cp=0.01)
prp(pruned_tree, type=1, extra=1, under=TRUE)  # Less is often more

# Model evaluation - moment of truth
predicted_class <- predict(pruned_tree, newdata=validationSet, type="class")
confusionMatrix(predicted_class, validationSet$HELOC, positive="1")  # Fingers crossed

# ROC - because we love curves
predicted_prob <- predict(pruned_tree, newdata=validationSet, type="prob")
roc_obj <- roc(validationSet$HELOC ~ predicted_prob[,2])
plot.roc(roc_obj)  # Pretty curves
auc(roc_obj)  # The magical AUC number

# ggplot - making data look good
ggplot(trainSet, aes(x=Variable1, y=Variable2, color=HELOC)) + geom_point() + theme_minimal()

# Random Forest - when one tree isn't enough
rf_model <- randomForest(HELOC ~ ., data=trainSet, ntree=100, importance=TRUE)
varImpPlot(rf_model)  # The importance dance

# Scoring time - let's see how we did
predicted_score <- predict(rf_model, newdata=myScoreData)
myScoreData$Predicted_HELOC <- predicted_score
View(myScoreData)  # Drum roll...

# A touch of Time Series - because why not?
ts_data <- ts(myData$Variable1, frequency=12, start=c(2020,1))
forecast_model <- auto.arima(ts_data)
forecast_plot <- forecast(forecast_model)
plot(forecast_plot)  # Gazing into the crystal ball

# Saving our masterpiece (a.k.a the model)
saveRDS(rf_model, file="rf_model.rds")

write.xlsx(myData1, file="/Users/rhaeagrxing/Downloads/Updated_Data_File_1.xlsx")
