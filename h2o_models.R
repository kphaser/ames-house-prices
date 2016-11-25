### H2O Model Building ###
library(h2o)
library(h2oEnsemble)
h2o.init(nthreads = -1)

data <- h2o.importFile("data/train.csv")
test <- h2o.importFile("data/test.csv")

data <- cbind(y,x_train)
colnames(data)[1] <- "SalePrice"
data <- as.h2o(data)

test_h2o <- as.h2o(x_test)

# Partition the data
splits <- h2o.splitFrame(data = data, 
                         ratios = 0.75,  #partition data into 75%, 25%
                         seed = 1)  #setting a seed will guarantee reproducibility
train_h2o <- splits[[1]]
valid_h2o <- splits[[2]]

# Identify response and predictors
y <- "SalePrice"
x <- setdiff(names(data),y)
print(x)

## Models
# GLM
glm_fit1 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = train_h2o,
                    model_id = "glm_fit1",
                    family = "gaussian")
glm_fit2 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = train_h2o,
                    model_id = "glm_fit2",
                    validation_frame = valid_h2o,
                    family = "gaussian",
                    lambda_search = TRUE)
glm_fit3 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = train_h2o,
                    model_id = "glm_fit3",
                    validation_frame = valid_h2o,
                    family = "gaussian",
                    alpha = 1, # lasso penalty
                    lambda_search = TRUE)
glm_fit4 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = train_h2o,
                    model_id = "glm_fit4",
                    validation_frame = valid_h2o,
                    family = "gaussian",
                    alpha = 0, # ridge penalty
                    lambda_search = TRUE)


glm_perf1 <- h2o.performance(model = glm_fit1,
                             newdata = valid_h2o)
glm_perf2 <- h2o.performance(model = glm_fit2,
                             newdata = valid_h2o)
glm_perf3 <- h2o.performance(model = glm_fit3,
                             newdata = valid_h2o)
glm_perf4 <- h2o.performance(model = glm_fit4,
                             newdata = valid_h2o)


glm_perf1
glm_perf2
glm_perf3
glm_perf4

h2o.rmse(glm_perf1)
h2o.rmse(glm_perf2)
h2o.rmse(glm_perf3)
h2o.rmse(glm_perf4)

# Random Forest
rf_fit1 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train_h2o,
                            model_id = "rf_fit1",
                            seed = 1)

rf_fit2 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train_h2o,
                            model_id = "rf_fit2",
                            #validation_frame = valid_h2o,  #only used if stopping_rounds > 0
                            ntrees = 1000,
                            seed = 1)

rf_fit3 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train_h2o,
                            model_id = "rf_fit2",
                            validation_frame = valid_h2o,
                            stopping_rounds = 1,
                            stopping_metric = "MSE",
                            ntrees = 1000,
                            seed = 1)

rf_perf1 <- h2o.performance(model = rf_fit1,
                            newdata = valid_h2o)
rf_perf2 <- h2o.performance(model = rf_fit2,
                            newdata = valid_h2o)
rf_perf3 <- h2o.performance(model = rf_fit3,
                            newdata = valid_h2o)

rf_perf1
rf_perf2
rf_perf3

h2o.rmse(rf_perf1)
h2o.rmse(rf_perf2)
h2o.rmse(rf_perf3)


# GBM
gbm_fit1 <- h2o.gbm(x = x,
                    y = y,
                    training_frame = train_h2o,
                    model_id = "gbm_fit1",
                    seed = 1)

gbm_fit2 <- h2o.gbm(x = x,
                    y = y,
                    training_frame = train_h2o,
                    model_id = "gbm_fit2",
                    #validation_frame = valid,  #only used if stopping_rounds > 0
                    ntrees = 500,
                    seed = 1)

gbm_fit3 <- h2o.gbm(x = x,
                    y = y,
                    training_frame = train_h2o,
                    model_id = "gbm_fit3",
                    validation_frame = valid_h2o,  #only used if stopping_rounds > 0
                    ntrees = 500,
                    score_tree_interval = 5,      #used for early stopping
                    stopping_rounds = 3,          #used for early stopping
                    stopping_metric = "AUTO",      #used for early stopping
                    stopping_tolerance = 0.0005,  #used for early stopping
                    seed = 1)


gbm_perf1 <- h2o.performance(model = gbm_fit1,
                             newdata = valid_h2o)
gbm_perf2 <- h2o.performance(model = gbm_fit2,
                             newdata = valid_h2o)
gbm_perf3 <- h2o.performance(model = gbm_fit3,
                             newdata = valid_h2o)


gbm_perf1
gbm_perf2
gbm_perf3

h2o.scoreHistory(gbm_fit2)
h2o.scoreHistory(gbm_fit3)

plot(gbm_fit3, 
     timestep = "number_of_trees", 
     metric = "rmse")


# Deep Learning
dl_fit1 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train_h2o,
                            model_id = "dl_fit1",
                            seed = 1)

dl_fit2 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train_h2o,
                            model_id = "dl_fit2",
                            #validation_frame = valid,  #only used if stopping_rounds > 0
                            epochs = 20,
                            hidden= c(10,10),
                            stopping_rounds = 0,  # disable early stopping
                            seed = 1)

dl_fit3 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train_h2o,
                            model_id = "dl_fit3",
                            validation_frame = valid_h2o,  #in DL, early stopping is on by default
                            epochs = 20,
                            hidden = c(10,10),
                            score_interval = 1,           #used for early stopping
                            stopping_rounds = 3,          #used for early stopping
                            stopping_metric = "AUTO",     #used for early stopping
                            stopping_tolerance = 0.0005,   #used for early stopping
                            seed = 1)

dl_perf1 <- h2o.performance(model = dl_fit1,
                            newdata = valid_h2o)
dl_perf2 <- h2o.performance(model = dl_fit2,
                            newdata = valid_h2o)
dl_perf3 <- h2o.performance(model = dl_fit3,
                            newdata = valid_h2o)

dl_perf1
dl_perf2
dl_perf3

h2o.scoreHistory(dl_fit3)

plot(dl_fit3, 
     timestep = "epochs", 
     metric = "rmse")


# Ensemble Stacking with default wrappers
learner <- c("glm_fit3", "rf_fit2", 
             "gbm_fit3", "dl_fit1")
metalearner <- "h2o.glm.wrapper"


fit <- h2o.ensemble(x = x, y = y, 
                    training_frame = train_h2o, 
                    family = "gaussian", 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5))


pred <- predict(fit, valid_h2o)
predictions <- as.data.frame(pred$pred)[,1]
labels <- as.data.frame(valid_h2o[,y])[,1]

rmse(predictions,labels)


# Ensemble Stacking with default wrappers and neural network meta
learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper", 
             "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")
metalearner <- "h2o.deeplearning.wrapper"


fit2 <- h2o.ensemble(x = x, y = y, 
                    training_frame = train_h2o, 
                    family = "gaussian", 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5))


pred <- predict(fit, valid_h2o)
predictions <- as.data.frame(pred$pred)[,1]
labels <- as.data.frame(valid_h2o[,y])[,1]

rmse(predictions,labels)





# Model submissions
# Ensemble Stacking with default wrappers
learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper", 
             "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")
metalearner <- "h2o.glm.wrapper"


fit <- h2o.ensemble(x = x, y = y, 
                    training_frame = data, 
                    family = "gaussian", 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5))

pred <- predict(fit, test_h2o)
predictions <- as.data.frame(pred$pred)[,1]
results <- exp(predictions) - 1

solution <- data.frame(Id=as.integer(rownames(x_test)),SalePrice=results)
write.csv(solution,"ensemble1.csv",row.names=FALSE)


# GLM
fit <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = data,
                    model_id = "fit",
                    validation_frame = valid_h2o,
                    family = "gaussian",
                    alpha = 1, # lasso penalty
                    lambda_search = TRUE)


# Average model
fit1 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = data,
                    model_id = "glm_fit2",
                    validation_frame = valid_h2o,
                    family = "gaussian",
                    lambda_search = TRUE)
fit2 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = data,
                    model_id = "glm_fit3",
                    validation_frame = valid_h2o,
                    family = "gaussian",
                    alpha = 1, # lasso penalty
                    lambda_search = TRUE)
fit3 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = data,
                    model_id = "glm_fit4",
                    validation_frame = valid_h2o,
                    family = "gaussian",
                    alpha = 0, # ridge penalty
                    lambda_search = TRUE)
fit4 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = data,
                            model_id = "rf_fit1",
                            seed = 1)
fit5 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = data,
                            model_id = "rf_fit2",
                            #validation_frame = valid_h2o,  #only used if stopping_rounds > 0
                            ntrees = 1000,
                            seed = 1)
fit6 <- h2o.gbm(x = x,
                    y = y,
                    training_frame = data,
                    model_id = "gbm_fit1",
                    seed = 1)
fit7 <- h2o.gbm(x = x,
                    y = y,
                    training_frame = data,
                    model_id = "gbm_fit2",
                    #validation_frame = valid,  #only used if stopping_rounds > 0
                    ntrees = 500,
                    seed = 1)
fit8 <- h2o.gbm(x = x,
                    y = y,
                    training_frame = data,
                    model_id = "gbm_fit3",
                    validation_frame = valid_h2o,  #only used if stopping_rounds > 0
                    ntrees = 500,
                    score_tree_interval = 5,      #used for early stopping
                    stopping_rounds = 3,          #used for early stopping
                    stopping_metric = "AUTO",      #used for early stopping
                    stopping_tolerance = 0.0005,  #used for early stopping
                    seed = 1)
fit9 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = data,
                            model_id = "dl_fit1",
                            seed = 1)
fit10 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = data,
                            model_id = "dl_fit2",
                            #validation_frame = valid,  #only used if stopping_rounds > 0
                            epochs = 20,
                            hidden= c(10,10),
                            stopping_rounds = 0,  # disable early stopping
                            seed = 1)
fit11 <- h2o.ensemble(x = x, y = y, 
                    training_frame = data, 
                    family = "gaussian", 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5))

pred1 <- h2o.predict(fit1, test_h2o)
results1 <- exp(pred1) - 1
pred2 <- h2o.predict(fit2, test_h2o)
results2 <- exp(pred2) - 1
pred3 <- h2o.predict(fit3, test_h2o)
results3 <- exp(pred3) - 1
pred4 <- h2o.predict(fit4, test_h2o)
results4 <- exp(pred4) - 1
pred5 <- h2o.predict(fit5, test_h2o)
results5 <- exp(pred5) - 1
pred6 <- h2o.predict(fit6, test_h2o)
results6 <- exp(pred6) - 1
pred7 <- h2o.predict(fit7, test_h2o)
results7 <- exp(pred7) - 1
pred8 <- h2o.predict(fit8, test_h2o)
results8 <- exp(pred8) - 1
pred9 <- h2o.predict(fit9, test_h2o)
results9 <- exp(pred9) - 1
pred10 <- h2o.predict(fit10, test_h2o)
results10 <- exp(pred10) - 1
pred11 <- predict(fit11, test_h2o)
predictions11 <- as.data.frame(pred11$pred)[,1]
results11 <- exp(predictions11) - 1
results11 <- as.h2o(results11)
avgresults <- (results1+results2+results3+results4+results5+results6+results7+results8+results9+results10+results11)/11
avgresults <- as.data.frame(avgresults)
solution <- data.frame(Id=as.integer(rownames(x_test)),SalePrice=avgresults)
colnames(solution)[2] <- "SalePrice"
solution[2] <- round(solution[2],2)
write.csv(solution,"h2oavgmodel.csv",row.names=FALSE)

# Submission file
pred <- predict(fit, test_h2o)
predictions <- as.data.frame(pred$pred)[,1]
results <- exp(predictions) - 1

solution <- data.frame(Id=as.integer(rownames(x_test)),SalePrice=results)
write.csv(solution,"h2oglm.csv",row.names=FALSE)


# Shutdown H2O    
h2o.shutdown(prompt=FALSE)
