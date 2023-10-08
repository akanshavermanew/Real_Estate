#DTrees and random forest of linear regression
# Building Real Estate project using DTrees and Random forest model for linear regression 
#Random forest gave good result than dTees and simple linear model

#This file csv is generated and submitted for project

#csv created using random forest predictions 
#can refer another code from here https://rpubs.com/more11neha/RealEstateProject

#rmse for random forest came 0.67

#here we have not find cutoff as it was not asked to submit hard classes(cutoff)

library(tidymodels)
library(visdat)
library(tidyr)
library(car)
library(pROC)
library(ggplot2)
library(vip)
library(rpart.plot)
library(DALEXtra)
setwd("F:/R study materials/Projects/RealEstateProject")

hr_train=read.csv("housing_train.csv",stringsAsFactors = F)

hr_test= read.csv("housing_test.csv",stringsAsFactors = F)

vis_dat(hr_train)         #graphical represesntation of data

unique(table(hr_train$SellerG))
table(hr_train$Suburb)
glimpse(hr_train)

#hr_train$Price=as.numeric(hr_train$Price)

dp_pipe=recipe(Price ~ .,data=hr_train) %>% 
  update_role(Address,new_role = "drop_vars") %>% 
  update_role(Type,Suburb,Method,SellerG,CouncilArea,new_role="to_dummies") %>% 
  
  step_rm(has_role("drop_vars")) %>% 
  #step_novel(Type,Method,SellerG,CouncilArea) %>% 
  step_unknown(has_role("to_dummies"),new_level="__missing__") %>% 
  step_other(has_role("to_dummies"),threshold =0.02,other="__other__") %>% 
  step_dummy(has_role("to_dummies")) %>% 
  step_impute_median(all_numeric(),-all_outcomes())

dp_pipe=prep(dp_pipe)

summary(dp_pipe)

train=bake(dp_pipe,new_data = NULL)
test=bake(dp_pipe,new_data=hr_test)

table(train$Suburb)
glimpse(train)

vis_dat(train)

#Decision tree
tree_model=decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("regression")

#Cross validation
folds = vfold_cv(train, v = 5)

tree_grid = grid_regular(cost_complexity(), tree_depth(),    #if you want you can pass some range of values here
                         min_n(), levels = 3)

#Tuning
my_res=tune_grid(
  tree_model,
  Price~.,
  resamples = folds,
  grid = tree_grid,
  metrics = metric_set(rmse),     #for classification we use roc_auc and for regression we use rmse or mae 
  control = control_grid(verbose = TRUE)
  
)

autoplot(my_res)+theme_light()


fold_metrics=collect_metrics(my_res)    #grid search it will show table with parameters like mtry,tree_depth,etc (parameters of whatever model you are using
fold_metrics

my_res %>% show_best()            #it will give best roc_auc value for our model from grid serch with all parameters of whatever model you are using like mtry,tree_depth,etc

final_tree_fit=tree_model %>% 
  finalize_model(select_best(my_res)) %>% 
  fit(Price~.,data=train)

## feature importance 

library(vip)

final_tree_fit %>%
  vip(geom = "col", aesthetics = list(fill = "midnightblue", alpha = 0.8)) +
  scale_y_continuous(expand = c(0, 0))

#Plot the tree
library(rpart.plot)

rpart.plot(final_tree_fit$fit)

#Predictions
train_pred=predict(final_tree_fit,new_data = train)
test_pred=predict(final_tree_fit,new_data = test)


#Random forest
rf_model = rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("ranger")


#Cross validation
folds = vfold_cv(train, v = 5)

rf_grid = grid_regular(mtry(c(5,25)), trees(c(100,500)),     #for random forest passing some values is compulsory otherwise it will give error in decision tree the case was not like this
                       min_n(c(2,10)),levels = 3)

# c(5,25)  means start with 5 and go till 25
# mtry values should be <= features in your table

#tuning
my_res=tune_grid(
  rf_model,
  Price~.,
  resamples = folds,
  grid = rf_grid,
  metrics = metric_set(rmse,mae),
  control = control_grid(verbose = TRUE)
)

autoplot(my_res)+theme_light()

fold_metrics=collect_metrics(my_res)

my_res %>% show_best()

final_rf_fit=rf_model %>% 
  set_engine("ranger",importance='permutation') %>% 
  finalize_model(select_best(my_res,"rmse")) %>% 
  fit(Price~.,data=train)

# variable importance 

final_rf_fit %>%
  vip(geom = "col", aesthetics = list(fill = "midnightblue", alpha = 0.8)) +
  scale_y_continuous(expand = c(0, 0))

#predictions
train_pred=predict(final_rf_fit,new_data = train)
test_pred=predict(final_rf_fit,new_data = test)

test$Price=test_pred

#write csv
write.csv(test_pred,"Akansha_Verma_P1_part2.csv",row.names = F)
write.table(test_pred,"Akansha_Verma_P1_part2.csv",row.names=F,col.names = "Price")

getwd()

## partial dependence plots

model_explainer =explain_tidymodels(
  final_rf_fit,
  data = dplyr::select(train, -Price),
  y = as.integer(train$Price),
  verbose = FALSE
)

pdp = model_profile(
  model_explainer,
  variables = "Rooms",
  N = 2000,
  groups='YearBuilt'
)

plot(pdp)





#Quiz
----------------------------------------------------------------------------------
#q1)Find the variance of the target variable 'Price'.
  
#Soln 
var(hr_train$Price)

#Ans- 432958829215
-----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
#q2)Find out how many observations have missing values for variable 'YearBuilt'?
  
#soln  
sum(is.na(hr_train$YearBuilt))

#Ans - 3717
--------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
  
#q3)What is the difference in average price between house type h and t?
  
#Soln
# Example house data
house_data <- data.frame(
  House_Type = hr_train$Type,
 Price = hr_train$Price
 )

# Calculate the average price for house type h
avg_price_h <- mean(house_data$Price[house_data$House_Type == "h"])

# Calculate the average price for house type t
avg_price_t <- mean(house_data$Price[house_data$House_Type == "t"])

# Calculate the difference in average price
price_difference <- avg_price_h - avg_price_t

# Output the result
price_difference

#Ans - 392384.2
---------------------------------------------------------------------------------
--------------------------------------------------------------------------------
#q4)How many unique values variable postcode takes?
  
#Soln
  
length(unique(hr_train$Postcode))

#Ans - 94
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
#q5)Which seller has maximum value transactions? ( Sum of Price)
#Note: you need to write name of just that seller .
  
#Soln
# Example transaction data
transaction_data <- data.frame(
  Seller = hr_train$SellerG,
   Price = hr_train$Price
 )

# Calculate the sum of prices for each seller
seller_totals <- tapply(transaction_data$Price, transaction_data$Seller, sum)

# Find the seller with the maximum value transactions
max_seller <- names(seller_totals)[which.max(seller_totals)]

# Output the result
max_seller

#Ans- Jellis
-------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
#q6)Which CouncilArea has maximum average price?
#Note: you need to write name of just that CouncilArea .
  
#Soln
# Example house data
house_data <- data.frame(
  CouncilArea = hr_train$CouncilArea,
  Price = hr_train$Price
 )

# Calculate the average price for each CouncilArea
average_prices <- tapply(house_data$Price, house_data$CouncilArea, mean)

# Find the CouncilArea with the maximum average price
max_average_price_council <- names(average_prices)[which.max(average_prices)]

# Output the result
max_average_price_council

#Ans - Bayside
------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
#q7)which CouncilArea has maximum variance in the price?
#Note: you need to write name of just that CouncilArea .
  
#soln
# Example house data
house_data <- data.frame(
  CouncilArea = hr_train$CouncilArea,
  Price = hr_train$Price
 )

# Calculate the variance in price for each CouncilArea
variances <- tapply(house_data$Price, house_data$CouncilArea, var)

# Find the CouncilArea with the maximum variance in price
max_variance_council <- names(variances)[which.max(variances)]

# Output the result
max_variance_council

#Ans - Stonnington
