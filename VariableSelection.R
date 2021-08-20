my_data<-read.csv(file.choose())
names(my_data) <- c("Year","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","Y")
olsreg<-lm(Y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15,my_data)
summary(olsreg)


##detection of multicollinearity
library(car)
vif(olsreg)

##Multicollinearity is present in the models , because the VIFs are all greater than 5 for all the variables
library(mctest)

##Approach with variance Decomposition
eigprop(olsreg)
##Step1---In row 16, x11 and x12 have higher proportion 0.5 also in Row 15 same is the case for x4,x5,x9
##We drop x4 and x10 and again fit the model

olsreg_1<-lm(Y~x1+x2+x3+x5+x6+x7+x8+x9+x11+x12+x13+x14+x15,my_data)
summary(olsreg_1)
eigprop(olsreg_1)

##step2----In row 12, x1 and x14 have  proportion higher than 0.50, In row 13 x5 and x6 have  proportion higher than 0.50 and in row 14, x9 and x11 
##have higher proporion
##we remove x14,x5 and x9 and again fit the model

olsreg_2<-lm(Y~x1+x2+x3+x6+x7+x8+x11+x12+x13+x15,my_data)
summary(olsreg_2)
eigprop(olsreg_2)

##Step3---x12 in row 7,x1 in row 8, x6 in row 10 and x7 and x11 in row 11 have  proportion higher than 0.50
## We remove x1,x12,x6,x11 and again fit the model

olsreg_3<-lm(Y~x2+x3+x7+x8+x13+x15,my_data)
summary(olsreg_3)
eigprop(olsreg_3)

##Step4---x2 and x3 in row 7 and x8 in row 4 have proportion higher than 0.50
##We remove x3 and x8 again fit the model

olsreg_4<-lm(Y~x2+x7+x13+x15,my_data)
summary(olsreg_4)
eigprop(olsreg_4)

##Step5---x7,x13 and x15 in row 4 have proportion higher than 0.50 and x2 in row 5 has proportion higher than 0.50
##We remove x2 and x13 and again fit the model

olsreg_5<-lm(Y~x7+x15,my_data)
summary(olsreg_5)
eigprop(olsreg_5)

##Thus in the step 5 there is still indiaction of the presence of multicollinearity.
##Now we first determine the important variables using AIC criteria


##Variable Selecion
AIC<-stepAIC(olsreg,direction="both")
AIC
summary(AIC)

##The important variables selected by this method to regress Y are x2 , x3 , x4 , x6 , x7 ,x8 , x9 , x10 , x11 , x14 , x15

## Using the subset model we use Ridge Regression Technique to get rid off the Multicollinearity problem
df <- data.frame(my_data[,3:5],my_data[,7:12],my_data[,15:16])
head(df)
#ridge regression
library(lmridge)
ridge_mod = lmridge(my_data$Y~.,df, K = seq(0, 0.1, 0.001),scaling="sc")
ridge_mod
## Ridge trace
plot(ridge_mod, type = "ridge")
##The estimated values of the parameters are stabilize somewhere between 0.02 and 0.04(value of the ridge parameter).
##We take the parameter as 0.035

model1=lmridge(my_data$Y~.,df, K=0.035)
## ridge related statistics from summary function
summary(model1)
summary(model1)$summaries[[1]]$stats
#Adj R-sq=98.83%
vif(model1)

##Here the values of the vifs are all less than 5 , so we can claim that now our model is free from Multicollinearity problem





