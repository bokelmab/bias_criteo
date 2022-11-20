##required libraries
library(data.table)
library(dplyr)
library(magrittr)
library(ranger)

## set seed
set.seed(181122)

## choose target
target <- 'conversion'

## read Criteo data 
## please download the data from http://go.criteo.net/criteo-research-uplift-v2.1.csv.gz
data <- fread('data/criteo-uplift-v2.1.csv')

## delete menth closing campaign and encode treatment
names(data)[which(names(data)== 'treatment')] <- 'W'
y_data <- data[, target, with = F]
data$Y <- data[, target, with = F]
data <- data[,-c('conversion', 'visit', 'exposure'), with = F]
data$Y <- y_data

### prove of sampling bias ############################################################
diff_means <- c()
sd_means <- c()
t_test <- c()
for(i_samp in 1:5){
  
  ## split in training and test set
  idx_train <- sample(1:nrow(data), round(0.1*nrow(data)), replace = F)
  dt_train <- data[idx_train,]
  dt_test <- data[-idx_train,]
  
  ## test for difference in expected value of random forest predictions
  rf0 <- ranger(Y ~ ., data = dt_train[W==0,-c('W'), with = F], min.node.size = 1000, num.trees = 500)  
  pred_rf0 <- predict(rf0, dt_test)$predictions
  
  options(scipen=999) ## disable scientific notation
  diff_means <- c(diff_means, mean(pred_rf0[dt_test$W==1])-mean(pred_rf0[dt_test$W==0]))
  sd_means <- c(sd_means, sqrt(var(pred_rf0[dt_test$W==1])/nrow(dt_test[W==1,])+var(pred_rf0[dt_test$W==0])/nrow(dt_test[W==0,])))
  t_test <- c(t_test, t.test(x = pred_rf0[dt_test$W==1], y = pred_rf0[dt_test$W==0])$p.value)
  
  ## print results
  print(diff_means)
  print(sd_means)
  print(t_test)
}

## save results
saveRDS(diff_means, 'results/diff_means.RDS')
saveRDS(sd_means, 'results/sd_means.RDS')
saveRDS(t_test, 'results/t_test.RDS')

### doubly-robust estimation of treatment effects ####################################################
set.seed(181122)
idx_train <- sample(1:nrow(data), round(0.1*nrow(data)), replace = F)
dt_train <- data[idx_train,]
dt_test <- data[-idx_train,]
rf0 <- ranger(Y ~ ., data = dt_train[W==0,-c('W'), with = F], min.node.size = 1000, num.trees = 500)
rf1 <- ranger(Y ~ ., data = dt_train[W==1,-c('W'), with = F], min.node.size = 1000, num.trees = 500)
rfW <- ranger(W ~ ., data = dt_train[,-c('Y'), with = F], min.node.size = 1000, num.trees = 500)

## predict by spliting data to overcome run-time problems
pred_rf0 <- c()
pred_rf1 <- c()
pred_rfw <- c()
for(i_split in 1:12){
  pred_rf0 <- c(pred_rf0, predict(rf0, dt_test[(1:1000000)+(i_split-1)*1000000,])$predictions)
  pred_rf1 <- c(pred_rf1, predict(rf1, dt_test[(1:1000000)+(i_split-1)*1000000,])$predictions)
  pred_rfw <- c(pred_rfw, predict(rfW, dt_test[(1:1000000)+(i_split-1)*1000000,])$predictions)
}
pred_rf0 <- c(pred_rf0, predict(rf0, dt_test[12000001:nrow(dt_test),])$predictions)
pred_rf1 <- c(pred_rf1, predict(rf1, dt_test[12000001:nrow(dt_test),])$predictions)
pred_rfw <- c(pred_rfw, predict(rfW, dt_test[12000001:nrow(dt_test),])$predictions)

## save results
saveRDS(pred_rf0, 'results/pred_rf0.RDS')
saveRDS(pred_rf1, 'results/pred_rf1.RDS')
saveRDS(pred_rfw, 'results/pred_rfw.RDS')

## estimation of treatment effects
tau_dr <- (dt_test$Y-pred_rf1)*dt_test$W/pred_rfw - (dt_test$Y-pred_rf0)*(1-dt_test$W)/(1-pred_rfw) + (pred_rf1-pred_rf0)
mean(tau_dr) ## doubly-robust estimation
mean(dt_test[W==1,]$Y-pred_rf0[dt_test$W==1])-mean(dt_test[W==0,]$Y-pred_rf0[dt_test$W==0])
mean(dt_test[W==1,]$Y)-mean(dt_test[W==0,]$Y) ## estimation by sample means

### Calculation of Qini curve by the original and adjusted outcomes #############################
source('code/Qini_calculation_and_plot.R') ## helper functions

pred_tlearner <- pred_rf1-pred_rf0 ## predictions of t-learner
dt_test$Y_mu <- dt_test$Y-pred_rf0
plot_qini_curve(p_preds1 = pred_tlearner, p_preds2 = pred_rf1,p_dt_test = dt_test)

