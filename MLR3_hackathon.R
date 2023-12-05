
#install packages - uncomment

#install.packages("mlr3verse",dependencies=TRUE)

#install.packages("diptest")

#install.packages("mlr3")
library(ggplot2)
library(tidyverse)
library(data.table)
library(janitor)
library(mlr3)
library(mlr3data)
library(DT)
library(skimr)
library(precrec)
library(ranger)
library(mlr3verse)
library(data.table)

setwd("\\\\fp5hq/parentviewanalysis$/Data Science Graduate Programme/Year2/Module_3_Hackathon")

ames <- read.csv("ames.csv") %>%
  clean_names()

#multiple linear regression using tidyverse
#Using tidyverse
model <- lm(sale_price ~ lot_area + lot_config + neighborhood + bldg_type + overall_qual + overall_cond + year_built, data = ames)
summary(model)$coefficient

#Overall quality is the most significant factor with lot area as number 2
#Rerun without Neighborhood and condition
refined <- lm(sale_price ~ lot_area + lot_config + overall_qual + year_built, data = ames)
summary(refined)

#choose subset of variables - lot area, lot cofig, neighborhood, building type, overall quality, overall condition, year built & outcome of sale price
ames2 <- ames %>%
  select(lot_area,lot_config,neighborhood,bldg_type,overall_qual,overall_cond,year_built,sale_price)%>%
  mutate(row_ids=row_number())

#turn into data table
setDT(ames2)

#look at missing values
#(there are none cuz we checked LOL)

#create the task 
task = as_task_regr(ames2,target='sale_price',id='row_ids')

#train and test set split 
set.seed(7)
#use two thirds for training
train_set = sample(ames2$row_ids, 0.67 * nrow(ames2))

#remaining third for test
test_set = setdiff(ames2$row_ids, train_set)

##########################################
####### linear regression learner ########
##########################################
learner = lrn("regr.lm")

#train
learner$train(task, row_ids = train_set)

#predict
pred_train = learner$predict(task, row_ids=train_set)
pred_test = learner$predict(task, row_ids=test_set)

#assess
pred_train

#plot truth and response
ggplot(pred_train,aes(x=truth, y=response)) +
  geom_point() +
  geom_smooth(method='lm')

#use r sq to compare the models
measures = msrs(c('regr.rsq'))

pred_train$score(measures)

pred_test

pred_test$score(measures)

####################################################################
###########regression tree - this is not working for some reason, 
# something somewhere is a character and it doesn't like it!
####################################################################
learner = lrn("regr.rpart")

#train
learner$train(task, row_ids = train_set)
learner$model

#Predict
pred_train = learner$predict(task, row_ids=train_set)
pred_test = learner$predict(task, row_ids=test_set)

#Assess
pred_train

#graph
ggplot(pred_train,aes(x=truth, y=response)) +
  geom_point() +
  geom_smooth(method='lm')

######################################
########### random forest ############
######################################
learner = lrn("regr.ranger")

#Train
learner$train(task, row_ids = train_set)
learner$model

#Predict
pred_train = learner$predict(task, row_ids=train_set)
pred_test = learner$predict(task, row_ids=test_set)

#Assess
pred_train

#plot
ggplot(pred_train,aes(x=truth, y=response)) +
  geom_point() +
  geom_smooth(method='lm')

#measures - how well is training doing compared to test?
measures = msrs(c('regr.rsq'))
pred_train$score(measures)

pred_test

pred_test$score(measures)

### test not as good as train
### should add more variables/ split differently?
### 

