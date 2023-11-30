library(readxl)
library(dplyr)
library(ggplot2)

# Load and view data for Regression Analysis
myData <- read_excel("Data/Regression_Data_File.xlsx")
View(myData)

# Linear Regression Model
lmodel <- lm(y ~ x1 + x2, data=myData)
summary(lmodel)

# Predictions with Linear Model
pred1_lmodel <- predict(lmodel, data.frame(x1=10, x2=20))
pred2_lmodel <- predict(lmodel, data.frame(x1=30, x2=60))

# Logistic Regression Model
logitmodel <- glm(Complication ~ Weight + Age + Diabetes, family=binomial, data=myData)
summary(logitmodel)

# Predictions with Logistic Model
pLog <- predict(logitmodel, data.frame(Weight=180, Age=60, Diabetes=0), type="response")
oLog <- pLog / (1 - pLog)
cf <- coef(logitmodel)
percentChange <- (exp(cf[2]) - 1) * 100

# Load and view second dataset
myData2 <- read_excel("Data/Regression_Data_File2.xlsx")
View(myData2)

# Linear Probability and Logistic Models
lpm <- lm(Spam ~ Recipients + Hyperlinks + Characters, data=myData2)
logitm <- glm(Spam ~ Recipients + Hyperlinks + Characters, family=binomial, data=myData2)

# Prediction Accuracy Comparison
lpm_pred <- ifelse(predict(lpm) >= 0.5, 1, 0)
logitm_pred <- ifelse(predict(logitm, type='response') >= 0.5, 1, 0)
lpm_accuracy <- mean(myData2$Spam == lpm_pred)
logitm_accuracy <- mean(myData2$Spam == logitm_pred)

# Load and view data for Depression Analysis
myData3 <- read_excel("Data/Regression_Data_File_Depression.xlsx")
View(myData3)
train <- myData3[1:225, ]
valid <- myData3[226:300, ]

# Build and evaluate model
depression_model <- glm(Depression ~ GPA + Attendance + Female, family=binomial, data=train)
pred1_depression <- ifelse(predict(depression_model, newdata=valid, type='response') >= 0.5, 1, 0)
pred2_depression <- ifelse(predict(depression_model, newdata=valid, type='response') >= 0.07, 1, 0)
accuracy1 <- mean(valid$Depression == pred1_depression)
accuracy2 <- mean(valid$Depression == pred2_depression)

# Load data for Spam Analysis
myData4 <- read_excel("Data/Regression_Data_File_Spam.xlsx")
View(myData4)
train <- myData4[1:375, ]
hold <- myData4[376:500, ]

# Fit and evaluate models
model1_spam <- glm(Spam ~ Recipients + Hyperlinks + Characters, family=binomial, data=train)
model2_spam <- glm(Spam ~ Recipients + Hyperlinks, family=binomial, data=train)
pred1_spam <- ifelse(predict(model1_spam, hold, type='response') >= 0.5, 1, 0)
pred2_spam <- ifelse(predict(model2_spam, hold, type='response') >= 0.5, 1, 0)
accuracy1_spam <- mean(hold$Spam == pred1_spam)
accuracy2_spam <- mean(hold$Spam == pred2_spam)

# Combined model for entire dataset
model3_spam <- glm(Spam ~ Recipients + Hyperlinks + Characters, family=binomial, data=myData4)
new_prediction <- predict(model3_spam, data.frame(Recipients=20, Hyperlinks=5, Characters=60), type='response')

# Print Summary and Predictions
print(summary(model3_spam))
print(new_prediction)

# Visualizations
ggplot(myData, aes(x=x1, y=y)) + geom_point() + geom_smooth(method='lm') + ggtitle('Linear Regression Analysis')
ggplot(myData2, aes(x=Recipients, y=Spam)) + geom_point() + geom_smooth(method='lm', color='blue') + ggtitle('Spam Analysis - Linear Model')
ggplot(myData3, aes(x=GPA, y=Depression, color=factor(Female))) + geom_point() + ggtitle('Depression Analysis')

