library(xgboost)
library(dplyr)

# load train features
train = lapply(1:10, function(i){
  read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/columns.csv'))
}) %>% do.call(rbind, .) %>% data.frame

# load train target
traintarget = lapply(1:10, function(i){
  read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/targets.csv'))
}) %>% do.call(rbind, .) %>% data.frame

train = train[complete.cases(traintarget),]
traintarget = traintarget[complete.cases(traintarget),-(1:2)]

# load test features
sub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv') %>% data.table
test_nrows = sub[,by='record_id', max(end)]$V1 # 5-29 sec

test = lapply(11:882, function(i){
  df = read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/columns.csv'))
  df = df[1:test_nrows[i-10], ]
  return(df)
}) %>% do.call(rbind, .) %>% data.frame

# remove duplicate columns
remDupcols <- function(data){
  rem = which(!(names(data) %in% colnames(unique(as.matrix(data), MARGIN=2))))
  return(rem)
}
rem = remDupcols(train)
train = train[,-rem]
test = test[,-rem]

# remove highly correlated features from data
require(caret)
remHighcor <- function(data, cutoff, ...){
  data_cor <- cor(sapply(data, as.numeric), ...)
  data_cor[is.na(data_cor)] <- 0
  rem <- findCorrelation(data_cor, cutoff=cutoff, verbose=T)
  return(rem)
}
rem = remHighcor(train, cutoff = 0.99, use='pairwise.complete.obs')
train = train[,-rem]
test = test[,-rem]

rm(rem, remHighcor, remDupcols, test_nrows, sub)
gc()

# xgboost CV
cweights = c(1.35298455691,1.38684574053,1.59587388404,1.35318713948,0.347783666015,0.661081706198,1.04723628621,0.398865222651,
             0.207586320237,1.50578335208,0.110181365961,1.07803284435,1.36560417316,1.17024113802,1.1933637414,1.1803704493,
             1.34414875433,1.11683830693,1.08083910312,0.503152249073)
params = list(booster = 'gbtree',
              objective = 'reg:logistic',
              eta = 0.1, # 
              subsample=0.6,
              colsample_bytree=0.9,
              max_depth=4, 
              verbose=1,
              silent=1,
              nthread=3)
data <- lapply(1:20, function(c) xgb.DMatrix(data=data.matrix(train), label=as.numeric(traintarget[,c]), missing=NA))
folds = list(1:1753, 1754:3395, 3396:4911, 4912:6448, 6449:7955, 7956:9347, 9348:10852, 10853:12520, 12521:14345, 14346:16124)
booster_folds <- lapply(1:length(folds), function(k){
  test = lapply(1:20, function(c) xgboost:::slice(data[[c]], folds[[k]]))
  train = lapply(1:20, function(c) xgboost:::slice(data[[c]], unlist(folds[-k])))
  booster = lapply(1:20, function(c) xgboost:::xgb.Booster(params = params, cachelist = list(train[[c]], test[[c]])))
  res = list(train=train, test=test, booster=booster)
  return(res)
})
BS <- function(preds, train){
  labels = lapply(1:20, function(c) getinfo(train[[c]], 'label'))
  err = lapply(1:20, function(c) sum(cweights[c]*(preds[[c]] - labels[[c]])^2)/length(labels[[c]]))
  err = sum(unlist(err))
  return(c('BS'=err))
}

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
# [70] 0.125181+0.001987844  0.1912817+0.02403459


# xgboost
gc()
dtrain <- lapply(1:20, function(c) xgb.DMatrix(data=data.matrix(train), label=as.numeric(traintarget[,c]), missing=NA))
booster = lapply(1:20, function(c) xgboost:::xgb.Booster(params = params, cachelist = list(dtrain[[c]])))
nrounds= 70
gc()
for(i in 1:nrounds){ # iterate over rounds
  print(i)
  # update 20 dimentional dmatrix
  succ = lapply(1:20, function(c) xgboost:::xgb.iter.update(booster=booster[[c]], dtrain=dtrain[[c]],iter= i - 1, obj=NULL))
  gc()
}

# eval 20 dimentional dmatrix
dtest <- xgb.DMatrix(data=data.matrix(test), missing=NA)
preds_test = lapply(1:20, function(c) predict(booster[[c]], dtest))
norm_test = colSums(do.call('rbind', preds_test))
preds_test = lapply(1:20, function(c) preds_test[[c]]/norm_test)

# create submission
sub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv') %>% data.table
for(i in 1:20){
  sub[,i+3] = preds_test[[i]]
}

write.table(sub, 'data/working/subs/xgboost_default.csv', 
            col.names = T, row.names = F, quote=F, sep=',')

# LB = 0.1760


