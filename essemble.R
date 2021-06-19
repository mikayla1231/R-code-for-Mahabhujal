#Loading the required libraries
library('caret')

df<-read_xlsx("C:\\Users\\Mikayla\\OneDrive\\Desktop\\runoff\\GWFR.xlsx")
summary(df)
head(df)
str(df)

#Does the data contain missing values
sum(is.na(df))

sample<-sample.split(df,SplitRatio=0.70)
train<-subset(df,sample=TRUE)
test<-subset(df,sample=FALSE)

fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final',
  classProbs = T)

predictors<-c("Elevation", "Depth", "Rainmm","Runoff")
outcomeName<-'Usagelit'


#Create mode
linearMod <- lm(Usagelit ~ Runoff, data=test)  # build linear regression model on full data
print(linearMod)
summary(linearMod)

#Prediction
predict(linearMod,test)
plot(test$Usagelit,lty=1.8,col="blue")

#Accuracy
modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)
AIC(linearMod)  
BIC(linearMod)  

lmMod <- lm(Usagelit ~ Runoff, data=train)  # build the model
distPred <- predict(lmMod, test)  # predict distance

actuals_preds <- data.frame(cbind(actuals=test$Usagelit, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  

df.timeseries <- ts(df,start = c(2010,1),frequency = 12)

# Print the timeseries data.
print(df.timeseries)


# Plot a graph of the time series.
plot(df.timeseries)
library(gbm)

df.boost=gbm(Usagelit ~ . ,data = train,distribution = "gaussian",n.trees = 10000,
             shrinkage = 0.01, interaction.depth = 4)
df.boost

summary(df.boost) #Summary gives a table of Variable Importance and a plot of Variable Importance

#Plot of Response variable with lstat variable
plot(df.boost,i="Runoff") 
#Inverse relation with lstat variable

plot(df.boost,i="Rainmm") 
#as the average number of rooms increases the the price increases

cor(df$Runoff,df$Usagelit)#negetive correlation coeff-r

cor(df$Rainmm,df$Usagelit)#positive correlation coeff-r

n.trees = seq(from=100 ,to=10000, by=100) #no of trees-a vector of 100 values 

library(xgboost)
table(test$Rainmm)[2]/nrow(test)
gbm.fit <- gbm(
  formula = Rainmm~ .,
  distribution = "gaussian",
  data = train,
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# print results
print(gbm.fit)

sqrt(min(gbm.fit$cv.error))

# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit, method = "cv")

gbm.fit2 <- gbm(
  formula = Rainmm~ .,
  distribution = "gaussian",
  data = train,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.1,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# find index for n trees with minimum CV error
min_MSE <- which.min(gbm.fit2$cv.error)

# get MSE and compute RMSE
sqrt(gbm.fit2$cv.error[min_MSE])


# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit2, method = "cv")

hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_grid)
gbm.fit.final <- gbm(
  formula = Rainmm ~ .,
  distribution = "gaussian",
  data = train,
  n.trees = 483,
  interaction.depth = 5,
  shrinkage = 0.1,
  n.minobsinnode = 5,
  bag.fraction = .65, 
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  


