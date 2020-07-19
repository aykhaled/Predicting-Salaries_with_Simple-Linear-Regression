
## Predicting salaries
######################

# https://www.linkedin.com/in/aykhaled/

rm(list = ls())

# load libraries

library(tidyverse)
library(caTools)


# import dataset

dataset <- read.csv("Predicting_Salaries.csv")
glimpse(dataset)


# split dataset

set.seed(234)
split <- sample.split(dataset$AnnualSalary, SplitRatio = 0.75)
train <- subset(dataset, split == T)
test <- subset(dataset, split == F)


# Fit Linear Regression

LinReg <- lm(AnnualSalary ~ . , dataset)
summary(LinReg)

# apply the model on train set

train$yhat_AnnualSalary <- predict(LinReg, newdata = train)
  
# predict on test set

test$Predict_AnnualSalary <- predict(LinReg, newdata = test)


# visulizing train set

ggplot(train) +
  geom_point(aes(x = YearsOfExperience, y = AnnualSalary),
             color = 'darkgreen', stroke = 1.5) +
  geom_line(aes(x = YearsOfExperience, y = yhat_AnnualSalary),
            color = 'red', lwd = 1.5) +
  ggtitle ('Annual Salary vs. Experience (Training Set)') +
  xlab ('Years of Experience') +
  ylab ('Annual Salary') +
  scale_x_continuous(limits = c(0, 12)) + 
  scale_y_continuous(limits = c(0, 150000)) +
  theme_bw()

  
# visulizing test set

ggplot(test) +
  geom_point(aes(x = YearsOfExperience, y = AnnualSalary),
             color = 'darkgreen', stroke = 1.5) +
  geom_line(aes(x = YearsOfExperience, y = Predict_AnnualSalary),
            color = 'red', lwd = 1.5) +
  ggtitle ('Annual Salary vs. Experience (Test Set)') +
  xlab ('Years of Experience') +
  ylab ('Annual Salary') +
  scale_x_continuous(limits = c(0, 12)) + 
  scale_y_continuous(limits = c(0, 150000)) +
  theme_bw()







