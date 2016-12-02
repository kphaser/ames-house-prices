##### PREDICT 413 Final Project Code #####

### Load the data ###

library(ggplot2)
library(dplyr)
library(data.table)
library(caret)
library(moments)
library(corrplot)
library(VIM)
library(mice)
library(rpart)

train <- read.csv("data/train.csv",stringsAsFactors=FALSE)
test <- read.csv("data/test.csv",stringsAsFactors=FALSE)


train <- read.csv("data/train.csv",stringsAsFactors=TRUE)
test <- read.csv("data/test.csv",stringsAsFactors=TRUE)

# csv files are in the data subdirectory of the project
# ProjectTemplate automates the process
library(ProjectTemplate)
load.project()

# verify data exists
head(train)
head(test)

# create a time series class of data by ordering data by date and month

# combine train and test sets?



### EDA ###
dim(train)
summary(train)
str(train)

# check for duplicate records
nrow(train)
nrow(unique(train))
nrow(train) - nrow(unique(train))
# check for missing values
colSums(sapply(train, is.na))
# check to make sure there aren't constant features
sapply(train,function(x) length(unique(x)))
# check features with near zero variance
nearZeroVar(train,saveMetrics = TRUE)

# change some int var to factors and separate categorical and numerical variables to individual data frames
change_var <- c("OverallQual","OverallCond","FullBath","TotRmsAbvGrd","Fireplaces","GarageCars","MoSold")
train[,(change_var):=lapply(.SD,as.factor),.SDcols=change_var]
cat_var <- names(train)[which(sapply(train, is.factor))]
num_var <- names(train)[which(sapply(train, is.numeric))]
train[,(cat_var) := lapply(.SD, as.factor), .SDcols = cat_var]
train_cat <- train[,.SD,.SDcols = cat_var]
train_num <- train[,.SD,.SDcols = num_var]

# apply new factor levels for variables that have meaningful NA levels so we don't impute/delete them
ok_na <- c("Alley", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "GarageType", "GarageFinish", "GarageQual", "GarageCond", "PoolQC", "Fence", "MiscFeature")

addNoAnswer <- function(x){
    if(is.factor(x)) return(factor(x, levels=c(levels(x), "None")))
    return(x)
}

train[,(ok_na):=lapply(.SD,addNoAnswer),.SDcols=ok_na]

changeNA <- function (x) {
    x[is.na(x)] <- "None"
    return(x)
}

train[,(ok_na):=lapply(.SD,changeNA),.SDcols=ok_na]

head(train$Alley)
head(train$BsmtQual)
class(train$Alley)



# initial plots
plot(train$SalePrice)
hist(train$SalePrice)
qqnorm(train$SalePrice)
qqline(train$SalePrice)
hist(log(train$SalePrice+1)) # normalize response variable


train %>% select(LandSlope, Neighborhood, SalePrice) %>% filter(LandSlope == c('Sev', 'Mod')) %>% arrange(Neighborhood) %>% group_by(Neighborhood, LandSlope) %>% summarize(Count = n()) %>% ggplot(aes(Neighborhood, Count)) + geom_bar(aes(fill = LandSlope), position = 'dodge', stat = 'identity') + theme_light() +theme(axis.text.x = element_text(angle = 90, hjust =1))

# plot of SalePrice by Neighborhood
train %>% select(Neighborhood, SalePrice) %>% ggplot(aes(factor(Neighborhood), SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Neighborhoods')
# plot of SalePrice by MSZoning
train %>% select(MSZoning, SalePrice) %>% ggplot(aes(factor(MSZoning), SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('MSZoning')
# plot of SalePrice by FullBath
train %>% select(FullBath, SalePrice) %>% ggplot(aes(factor(FullBath), SalePrice)) + geom_boxplot() + xlab('FullBath')
# plot of SalePrice by GarageFinish
train %>% select(GarageFinish, SalePrice) %>% ggplot(aes(factor(GarageFinish), SalePrice)) + geom_boxplot() + xlab('GarageFinish')
# plot of SalePrice by KitchenQual
train %>% select(KitchenQual, SalePrice) %>% ggplot(aes(factor(KitchenQual), SalePrice)) + geom_boxplot() + xlab('KitchenQual')
# plot of SalePrice by CentralAir
train %>% select(CentralAir, SalePrice) %>% ggplot(aes(factor(CentralAir), SalePrice)) + geom_boxplot() + xlab('CentralAir')
# plot of YearBuilt by OverallQual
train %>% select(OverallCond, YearBuilt) %>% ggplot(aes(factor(OverallCond),YearBuilt)) + geom_boxplot() + xlab('Overall Condition')



# plot correlations
numericdf <- total[,numeric_vars]
ames_cor <- cor(numericdf)
col <- colorRampPalette(c("#BB4444","#EE9988","#FFFFFF","#77AADD","#4477AA"))
corrplot(ames_cor,method="shade",shade.col=NA,tl.col="black",tl.srt=45,col=col(200),addCoef.col="black",addcolorlabel="no",order="AOE")




### Data Prep ###


# impute missing values using mean/median, random forest, or mice package?

md.pattern(train_num)
aggr_plot <- aggr(train_num, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

imp.train <- mice(train, method='cart', maxit=3, seed=123)
train_complete <- complete(imp.train)


# split training data into training and validation set
n <- dim(train)[1]
set.seed(123)
valid.sample <- sample(n,round(n/4))
valid <- train[valid.sample,]
train <- train[-valid.sample,]

train_control <- trainControl(method="cv", number=5)


### Build Models ###

# Multiple linear regression
mylm=lm(y~., data=totaltrain)
mystep1=stepAIC(mylm, direction="both")
mystep1$anova
mypred1=predict(mystep1,totalimp[1461:2919,])
mypred2=exp(mypred1)
submit <- data.frame(Id=totalimp$Id[1461:2919],SalePrice=mypred2)
write.csv(submit,"lm1.csv",row.names=FALSE)

# Ridge regression
set.seed(123)  # for reproducibility
ridge1 <- train(x=x_train,y=y,
                    method="glmnet",
                    metric="RMSE",
                    maximize=FALSE,
                    trControl=fitControl,
                    tuneGrid=expand.grid(alpha=c(seq(1,0,-0.01)), 
                                         lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                  0.00075,0.0005,0.0001)))
ridge1

# Lasso regression
fitControl <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=5,
                                 verboseIter=FALSE)


set.seed(123)  # for reproducibility
lasso_mod <- train(x=x_train,y=y,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=fitControl,
                     tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                          lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                   0.00075,0.0005,0.0001)))
lasso_mod


# Lasso v2 (new features)
set.seed(123)  # for reproducibility
lasso_mod2 <- train(x=x_train,y=y,
                   method="glmnet",
                   metric="RMSE",
                   maximize=FALSE,
                   trControl=fitControl,
                   tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                        lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                 0.00075,0.0005,0.0001)))
lasso_mod2

# Lasso v3 w/o YearMo
set.seed(123)  # for reproducibility
lasso_mod3 <- train(x=x_train,y=y,
                    method="glmnet",
                    metric="RMSE",
                    maximize=FALSE,
                    trControl=fitControl,
                    tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                         lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                  0.00075,0.0005,0.0001)))
lasso_mod3

# Lasso v4 no new features and impute with median -- same score as v1
set.seed(123)  # for reproducibility
lasso_mod4 <- train(x=x_train,y=y,
                    method="glmnet",
                    metric="RMSE",
                    maximize=FALSE,
                    trControl=fitControl,
                    tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                         lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                  0.00075,0.0005,0.0001)))
lasso_mod4


# Lasso v5 no new features, skew fix and dummy var added, and impute with mean and remove zero variance variables
set.seed(123)  # for reproducibility
lasso_mod5 <- train(x=x_train,y=y,
                    method="glmnet",
                    metric="RMSE",
                    maximize=FALSE,
                    trControl=fitControl,
                    tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                         lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                  0.00075,0.0005,0.0001)))
lasso_mod5


# Random forest
rfmod <- randomForest(SalePrice~.-Id,data=train)
pred <- predict(rfmod,newdata=valid)
rmse(log(valid$SalePrice),log(pred))


set.seed(123)
rfmodel <- train(SalePrice~.-Id, data=train, trControl=fitControl, method="rf")
rfmodel
rfpred <- predict(rfmodel,newdata=valid)
rmse(log(valid$SalePrice),log(rfpred)) # 0.1503

# rf2
set.seed(123)
rf2 <- train(x=x_train,y=y, trControl=fitControl, method="rf")
rf2

# Support vector regression
library(e1071)
svmfit <- svm(SalePrice~.-Id,train)
svmpred <- predict(svmfit,valid)
# perform a grid search
tuneResult <- tune(svm, SalePrice ~ .-Id,  data = train_complete,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
print(tuneResult)
# Draw the tuning graph
plot(tuneResult)
tunedModel <- tuneResult$best.model
tunedSVM <- predict(tunedModel, valid) 

sqrt(mean((tunedSVM-r.test$PRCP)^2))
rmse(log(valid$SalePrice),log(svmpred))
rmse(log(valid$SalePrice),log(tunedSVM))

# svm 2
set.seed(123)
svm2 <- train(x=x_train,y=y, 
                 method = "svmLinear2", 
                 trControl = fitControl,
                 verbose = FALSE)
svm2



# GBM
fitControl <- trainControl(method = "cv", number = 5)

set.seed(123)
gbmFit1 <- train(SalePrice ~ . -Id, data = train, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)
gbmFit1
pred <- predict(gbmFit1,newdata=valid)
rmse(log(valid$SalePrice),log(pred))

# new data prep
set.seed(123)
gbmFit2 <- train(SalePrice ~ . -Id, data = train, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)
gbmFit2
gbmpred <- predict(gbmFit2,newdata=valid)
rmse(log(valid$SalePrice),log(gbmpred))

# prep with dummy vars and log transform of num vars and impute with mean
set.seed(123)
gbmFit3 <- train(x=x_train,y=y, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)
gbmFit3
gbmpred <- predict(gbmFit3,newdata=valid)
rmse(log(valid$SalePrice),log(gbmpred))


# XGBoost
library(xgboost)
fitControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
tuneGrid <- expand.grid(nrounds=c(500,100),eta=c(0.01,0.1,0.2,0.3,0.4),max_depth=c(2,4,6,8,10),gamma=c(0,1),
                        colsample_bytree=0.6,min_child_weight=1,subsample=1)

set.seed(123)
xgb1 <- train(SalePrice ~ . -Id, data = train, 
              method = "xgbTree", 
              trControl = fitControl,
              #tuneGrid=tuneGrid,
              verbose = FALSE)
xgb1
xgbpred <- predict(xgb1,newdata=valid)
rmse(log(valid$SalePrice),log(xgbpred))




# Artificial neural network - not working?
fitControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
set.seed(123)
nn1 <- train(x=x_train,y=y, 
              method = "neuralnet", 
              trControl = fitControl,
              #tuneGrid=tuneGrid,
              verbose = FALSE)
nn1
nnpred <- predict(nn1,newdata=valid)
rmse(log(valid$SalePrice),log(nnpred))



### Model averaging ###
fitControl <- trainControl(method="repeatedcv",
                           number=5,
                           repeats=5,
                           verboseIter=FALSE)

# Multiple linear regression with stepwise selection
library(MASS)
lmfit=lm(y~., data=x_train)
lmstep=stepAIC(lmfit, direction="both")
lmstep$anova
lmpreds <- exp(predict(lmstep,newdata=x_test)) - 1


# Lasso
set.seed(123)
lasso <- train(x=x_train,y=y,
                   method="glmnet",
                   metric="RMSE",
                   maximize=FALSE,
                   trControl=fitControl,
                   tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                        lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                 0.00075,0.0005,0.0001)))
lasso
lassopreds <- exp(predict(lasso,newdata=x_test)) - 1


# GLM
set.seed(123)
glmfit <- train(x=x_train,y=y, 
                method = "glmnet", 
                trControl = fitControl,
                verbose = FALSE)
glmfit
glmpreds <- exp(predict(glmfit,newdata=x_test)) - 1


# Random forest
set.seed(123)
rforest <- train(x=x_train,y=y, trControl=fitControl, method="rf")
rforest
rfpreds <- exp(predict(rforest,newdata=x_test)) - 1


# CForest
set.seed(123)
cforest <- train(x=x_train,y=y, trControl=fitControl, method="cforest")
cforest
cfpreds <- exp(predict(cforest,newdata=x_test)) - 1


# SVM 1
set.seed(123)
svmfit <- train(x=x_train,y=y, 
                method = "svmLinear", 
                trControl = fitControl,
                verbose = FALSE)
svmfit
svmpreds <- exp(predict(svmfit,newdata=x_test)) - 1

# SVM 2 tuned
library(e1071)
svmfit2 <- tune(svm, x_train,y,ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
print(svmfit2)
plot(svmfit2)
tunedModel <- svmfit2$best.model
svmpred2 <- exp(predict(tunedModel,newdata=x_test)) - 1 


# GBM
set.seed(123)
gbmfit <- train(x=x_train,y=y,
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)
gbmfit
gbmpreds <- exp(predict(gbmfit,newdata=x_test)) - 1


# Xgboost
set.seed(123)
xgbfit <- train(x=x_train,y=y,
              method = "xgbTree", 
              trControl = fitControl,
              #tuneGrid=tuneGrid,
              verbose = FALSE)
xgbfit
xgbpreds <- exp(predict(xgbfit,newdata=x_test)) - 1


# Neural network
set.seed(123)
nnfit <- train(x=x_train,y=y,
                method = "nnet", 
                trControl = fitControl,
                #tuneGrid=tuneGrid,
                linout=TRUE,
                verbose = FALSE)
nnfit
nnpreds <- exp(predict(nnfit,newdata=x_test)) - 1



# model averaging
avg_results <- (lassopreds+gbmpreds)/2
solution <- data.frame(Id=as.integer(rownames(x_test)),SalePrice=avg_results)
write.csv(solution,"modelavg1.csv",row.names=FALSE)

avg_results2 <- (lassopreds+rfpreds+cfpreds+svmpreds+svmpred2+gbmpreds+xgbpreds+nnpreds)/8
solution <- data.frame(Id=as.integer(rownames(x_test)),SalePrice=avg_results2)
write.csv(solution,"modelavg2.csv",row.names=FALSE)

avg_results3 <- (lassopreds+rfpreds+svmpred2+gbmpreds+xgbpreds)/5
solution <- data.frame(Id=as.integer(rownames(x_test)),SalePrice=avg_results3)
write.csv(solution,"modelavg3.csv",row.names=FALSE)


### Evaluate and Validate Models ###

# check residuals
# compare validation metric




### Model Selection ###




### Submission File ###

# random forest no parameter adjustments
rfsubmit <- randomForest(SalePrice~.-Id,data=train_complete)
pred <- predict(rfsubmit,newdata=test)
results <- data.frame(Id=test$Id,SalePrice=pred)
write.csv(results,'randomforest1.csv',row.names=FALSE)

# gbm w/ n.trees=150,interaction.depth=3,shrinkage=0.1,n.minobsinnode=10
gbmsubmit <- gbm(SalePrice~.-Id,data=train_complete,n.trees=150,interaction.depth=3,shrinkage=0.1,n.minobsinnode=10,verbose=FALSE)
pred <- predict(gbmsubmit,newdata=test,n.trees=150,type="response")
results <- data.frame(Id=test$Id,SalePrice=pred)
write.csv(results,'gbm1.csv',row.names=FALSE)

# gbm2 with same parameters but different data prep and newly created variable
gbmsubmit2 <- gbm(log(SalePrice)~.-Id, data=train_complete,n.trees=150,interaction.depth=3,shrinkage=0.1,n.minobsinnode=10,verbose=FALSE)
pred <- predict(gbmsubmit2,newdata=test,n.trees=150,type="response")
gbmpred <- exp(pred)
results <- data.frame(Id=test$Id,SalePrice=gbmpred)
write.csv(results,'gbm3.csv',row.names=FALSE)

# svm no tuning
svmsubmit <- svm(SalePrice~.-Id,train_complete)
svmpred <- predict(svmfit,test)
results <- data.frame(Id=test$Id,SalePrice=svmpred)
write.csv(results,'svm1.csv',row.names=FALSE)

# svm with tuning
tuneResult <- tune(svm, SalePrice ~ .-Id,  data = train_complete,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
tunedModel <- tuneResult$best.model
tunedSVM <- predict(tunedModel, test) 
results <- data.frame(Id=test$Id,SalePrice=tunedSVM)
write.csv(results,'svm2.csv',row.names=FALSE)

# xgboost with standard tuning
set.seed(123)
xgbSubmit <- train(SalePrice ~ . -Id, data = train_complete, 
              method = "xgbTree", 
              trControl = fitControl,
              #tuneGrid=tuneGrid,
              verbose = FALSE)
xgbSubmit
xgbpred <- predict(xgbSubmit,newdata=test)
results <- data.frame(Id=test$Id,SalePrice=xgbpred)
write.csv(results,'xgb1.csv',row.names=FALSE)


### Second data processing with skew transform and one hot encoding of categorical variables and imputation with mean ###
preds <- exp(predict(lasso_mod,newdata=x_test)) - 1
solution <- data.frame(Id=as.integer(rownames(x_test)),SalePrice=preds)
write.csv(solution,"lasso1.csv",row.names=FALSE)

preds <- exp(predict(gbmFit3,newdata=x_test)) - 1
solution <- data.frame(Id=as.integer(rownames(x_test)),SalePrice=preds)
write.csv(solution,"gbm3.csv",row.names=FALSE)

preds <- exp(predict(lasso_mod2,newdata=x_test)) - 1
solution <- data.frame(Id=as.integer(rownames(x_test)),SalePrice=preds)
write.csv(solution,"lasso2.csv",row.names=FALSE)

preds <- exp(predict(lasso_mod3,newdata=x_test)) - 1
solution <- data.frame(Id=as.integer(rownames(x_test)),SalePrice=preds)
write.csv(solution,"lasso3.csv",row.names=FALSE)

preds <- exp(predict(lasso_mod4,newdata=x_test)) - 1
solution <- data.frame(Id=as.integer(rownames(x_test)),SalePrice=preds)
write.csv(solution,"lasso4.csv",row.names=FALSE)

preds <- exp(predict(rf2,newdata=x_test)) - 1
solution <- data.frame(Id=as.integer(rownames(x_test)),SalePrice=preds)
write.csv(solution,"rf2.csv",row.names=FALSE)


solution <- data.frame(Id=as.integer(rownames(x_test)),SalePrice=lmpreds)
write.csv(solution,"lm2.csv",row.names=FALSE)

solution <- data.frame(Id=as.integer(rownames(x_test)),SalePrice=nnpreds)
write.csv(solution,"nn1.csv",row.names=FALSE)
