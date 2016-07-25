library(xgboost)
library(dplyr)

feats1 = read.csv('data/working/features/accel_feat.csv')
feats2 = read.csv('data/working/features/AP_feats.csv')
feats3 = read.csv('data/working/features/video_feat.csv')
targets = read.csv('data/working/features/targets.csv')
feats = cbind.data.frame(feats1, feats2) %>% cbind.data.frame(feats3)
rm(feats1, feats2, feats3)
gc()

# class weights
cweights = c(1.35298455691,1.38684574053,1.59587388404,1.35318713948,0.347783666015,0.661081706198,1.04723628621,0.398865222651,
             0.207586320237,1.50578335208,0.110181365961,1.07803284435,1.36560417316,1.17024113802,1.1933637414,1.1803704493,
             1.34414875433,1.11683830693,1.08083910312,0.503152249073)


# parameters
params = list(booster = 'gbtree',
              objective = 'reg:logistic',
              eta = 0.1, # [500] 0.2059921+0.00275667  0.2357373+0.02353194
              subsample=0.6,
              colsample_bytree=0.9,
              max_depth=5, # 2 [100] 0.20522+0.003013858  0.2360499+0.02452221, 3 [100] 0.1816438+0.00311104  0.2355763, 4 [100] 0.1564062+0.002854183  0.2367746+0.02612863+0.02516619
              verbose=1,
              silent=1,
              nthread=15)


# data as 20 dimensional dmatrix
data <- lapply(1:20, function(c) xgb.DMatrix(data=data.matrix(feats), label=as.numeric(targets[,c]), missing=NA))

# folds
folds = list(1:1753, 1754:3395, 3396:4911, 4912:6448, 6449:7955, 7956:9347, 9348:10852, 10853:12520, 12521:14345, 14346:16124)

# slice data into folds and create boosters = 10 folds X 20 boosters/fold X {train,test}/booster
booster_folds <- lapply(1:length(folds), function(k){
  test = lapply(1:20, function(c) xgboost:::slice(data[[c]], folds[[k]]))
  train = lapply(1:20, function(c) xgboost:::slice(data[[c]], unlist(folds[-k])))
  booster = lapply(1:20, function(c) xgboost:::xgb.Booster(params = params, cachelist = list(train[[c]], test[[c]])))
  res = list(train=train, test=test, booster=booster)
  return(res)
})

# Brier score eval func
BS <- function(preds, train){
  labels = lapply(1:20, function(c) getinfo(train[[c]], 'label'))
  err = lapply(1:20, function(c) sum(cweights[c]*(preds[[c]] - labels[[c]])^2)/length(labels[[c]]))
  err = sum(unlist(err))
  return(c('BS'=err))
}

#BestScore <- Inf

nrounds= 100

gc()

for(i in 1:nrounds){ # iterate over rounds
  
  #print(i)
  
  res = list() # list of 10 {train-BS,test-BS} pairs
  
  for( j in 1:length(booster_folds)){ # iterate over 10 folds
    
    fold = booster_folds[[j]]
    
    # update 20 dimentional dmatrix
    succ = lapply(1:20, function(c) xgboost:::xgb.iter.update(booster=fold$booster[[c]], dtrain=fold$train[[c]],iter= i - 1, obj=NULL))
    
    gc()
    
    # eval 20 dimentional dmatrix
    preds_train = lapply(1:20, function(c) predict(fold$booster[[c]], fold$train[[c]]))
    preds_test = lapply(1:20, function(c) predict(fold$booster[[c]], fold$test[[c]]))
    
    norm_train = colSums(do.call('rbind', preds_train))
    norm_test = colSums(do.call('rbind', preds_test))
    
    preds_train = lapply(1:20, function(c) preds_train[[c]]/norm_train)
    preds_test = lapply(1:20, function(c) preds_test[[c]]/norm_test)
    
    eval_train = BS(preds_train, fold$train); names(eval_train) = 'train-BS'
    eval_test = BS(preds_test, fold$test); names(eval_test) = 'test-BS'
    
    gc()
    rm( preds_train, preds_test, norm_train, norm_test)
    
    
    res[[j]] = c(eval_train, eval_test)
    gc()
    
  }
  
  res = do.call('rbind', res) %>% as.data.frame
  res = c(mean(res[,1]), sd(res[,1]), mean(res[,2]), sd(res[,2]))
  cat('[',i,'] ',res[1],'+',res[2],'  ',res[3],'+',res[4],'\n', sep='')
  
  #hist[[i]] = res

  
}

#invisible(hist)






