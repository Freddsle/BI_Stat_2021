library(tidyverse)

temp <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00464/superconduct.zip", temp)

file_one <- read.csv(unz(temp, 'train.csv'))
file_two <- read.csv(unz(temp, 'unique_m.csv'))

unlink(temp)
file_two <- file_two %>% select(-material) %>% relocate(critical_temp)
file_one <- file_one %>% relocate(critical_temp)
new_table <- cbind(file_two, file_one[, c(2:82)])

# В этой задаче мы хотим научиться предсказывать критическую температуру 
# сверхпроводника (колонка critical_temp) по различным характеристикам вещества 
# и по его составу (все остальные колонки).

## Разделите данные на обучающую и тестовую выборки, 
# на первой вы будете оценивать коэффициенты, на второй вычислять метрику. 

require(caTools)
set.seed(42) 
sample = sample.split(new_table$critical_temp, SplitRatio = .75)
train = subset(new_table, sample == TRUE)
test  = subset(new_table, sample == FALSE)

# Стандартизируйте ваши данные. 

trainMean <- train %>% select(-critical_temp) %>% apply(., 2, mean)
trainSd <- train %>% select(-critical_temp) %>% apply(., 2, sd)

norm.train <- sweep(train[ ,c(2:168)], 2L, trainMean)
norm.train <- cbind(train[1], norm.train)
norm.test <- sweep(sweep(test[,c(2:168)], 2L, trainMean), 2, trainSd, "/")
norm.test <- cbind(test[1], norm.test)

# Постройте линейную модель, 
# которая по всем имеющимся у вас признакам предсказывает критическую температуру, 
# посмотрите на adjusted R-squared.  

model <- lm(critical_temp ~ ., data = norm.train)
model_summary <- summary(model)
model_summary$adj.r.squared    # 0.7668411
model_summary$fstatistic
model_summary$residuals

## Хорошая ли получилась модель?
So-so
norm.test$new_critical_temp = predict(model, norm.test)
norm.test$critical_temp - 




