# Building Real Estate project using simple linear regression model
#rmse came as 0.51

library(tidymodels)
library(visdat)
library(tidyr)
library(car)

setwd("F:/R study materials/Projects/RealEstateProject")

hr_train=read.csv("housing_train.csv",stringsAsFactors = F)

hr_test= read.csv("housing_test.csv",stringsAsFactors = F)

vis_dat(hr_train)         #graphical represesntation of data

unique(table(hr_train$SellerG))
table(hr_train$Suburb)
glimpse(hr_train)

#Step1 : Data preparation
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

#Step2:separating into half in test data and half in train data
set.seed(2)
s=sample(1:nrow(train),0.8*nrow(train))
t1=train[s,]
t2=train[-s,]

#Step3:Building linear model
fit=lm(Price~.,data=t1)
summary(fit)

#Step4:Minimizing based on vif values
#minimized based on vif values
sort(vif(fit),decreasing = T)
fit=lm(Price~.-Type_X__other__ -SellerG_X__other__,data=t1)

#Step5:appliying step function which will minimize columns based on AIC score
fit=stats::step(fit)

formula(fit)

#Step6:Minmimizing columns based on p values
#Minmizing based on p values after taking actual formula it is reduced to this much after removing greater p value columns
fit=lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
         Landsize + BuildingArea + YearBuilt + Suburb_Reservoir + 
         Suburb_Richmond + Type_t + Type_u + Method_S + Method_SP + 
         Method_VB + Method_X__other__ + SellerG_Biggin + SellerG_hockingstuart + 
         SellerG_Jellis + SellerG_Marshall + SellerG_Nelson + SellerG_Ray + 
         CouncilArea_Banyule + CouncilArea_Bayside  + 
         CouncilArea_Brimbank + CouncilArea_Darebin + CouncilArea_Glen.Eira + 
         CouncilArea_Hobsons.Bay + CouncilArea_Manningham + CouncilArea_Maribyrnong + 
         CouncilArea_Melbourne + CouncilArea_Moonee.Valley + CouncilArea_Moreland + 
         CouncilArea_Port.Phillip + CouncilArea_Stonnington + CouncilArea_Yarra + 
         CouncilArea_X__other__   ,data=t1)


summary(fit)

#Step7:Predicting rmse values 
#predicting rmse values
t2.pred=predict(fit,newdata=t2)
errors=t2$Price-t2.pred

rmse=errors**2 %>% mean() %>% sqrt()
mae=mean(abs(errors))

Score=212467/rmse

#Step 8:Building model on entire data i.e. inside train and test is entire data and t1 and t2 is just data divided into halves to build our model on that first and if it works properly then we do finalising our model
#and doing same steps as done in t1 and t2
#bUILDING MODEL ON ENTIRE DATA with same step as above
fit.final=lm(Price ~ .,data=train)
sort(vif(fit.final),decreasing = T)
summary(fit.final)

#minimized based on vif values
fit.final=lm(Price~.-Type_X__other__ -SellerG_X__other__,data=train)

fit.final=stats::step(fit.final)
formula(fit.final)

#minimized based on p values
fit.final=lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
               Landsize + BuildingArea + YearBuilt + Suburb_Reservoir + 
               Suburb_Richmond + Type_t + Type_u + Method_S   
              + SellerG_Biggin  + 
               SellerG_hockingstuart + SellerG_Jellis + SellerG_Marshall  
                + SellerG_Ray  + CouncilArea_Banyule + 
               CouncilArea_Bayside  + CouncilArea_Brimbank + 
               CouncilArea_Darebin + CouncilArea_Glen.Eira + CouncilArea_Hobsons.Bay + 
               CouncilArea_Manningham + CouncilArea_Maribyrnong + CouncilArea_Melbourne + 
               CouncilArea_Moonee.Valley + CouncilArea_Moreland + CouncilArea_Port.Phillip + 
               CouncilArea_Stonnington + CouncilArea_Yarra + CouncilArea_X__other__  ,data=train)
summary(fit.final)

#to append price column in test data
test.pred=predict(fit.final,newdata=test)
test$Price=test.pred

#Step9:Writing in csv file
#write csv(can d with 115 or 116 any one)
write.csv(test.pred,"submision1.csv",row.names = F)
write.table(test.pred,"Aswini_RealEstate_P1_part2.csv",row.names=F,col.names = "Price")

getwd()




