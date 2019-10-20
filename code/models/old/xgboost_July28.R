library(xgboost)
library(dplyr)
library(caret)
library(data.table)

result_all <- NULL
prj_name <- 'xgboost_july28'

set.seed(1)

for(t in 1:20){
  
  cat('t = ', t, '\n',sep='')
  
  # load train, traintarget
  fnames = list.files(path = 'data/working/features/', pattern = 'train_[a-zA-Z0-9_]+.csv$') %>% paste0('data/working/features/',.)
  train  = lapply(fnames, fread) %>% do.call(cbind.data.frame, .) %>% data.frame
  traintarget = read.csv('data/working/features/train.target')[,-(1:4)]
  train = train[complete.cases(traintarget),]
  traintarget = traintarget[complete.cases(traintarget),]
  
  # load test
  fnames = list.files(path = 'data/working/features/', pattern = 'test_[a-zA-Z0-9_]+.csv$') %>% paste0('data/working/features/',.)
  test  = lapply(fnames, fread) %>% do.call(cbind.data.frame, .) %>% data.frame
  
  # remove cor columns
  #dup = remDupcols(data = train)
  dup = c(203,204,205,206,207,208,209,210,211,212,213,960,961,962,963,964,965,966,967,968,969,970,971,972,973,974,975,976,977,978,979,980,981,982,983,984,985)
  #res = remHighcorPar(data = train[,-dup], nblocks = 6, ncore = 4, cutoff = 0.98)$rem
  res = c(174,175,176,177,194,195,196,197,154,155,156,157,159,160,161,162,164,165,166,167,169,170,171,172,182,183,185,186,188,189,191,192)
  train = train[,-dup][,-res] # 16124 X 916
  test = test[,-dup][,-res] # 16600 X 916
  
  # cv folds
  folds = list(1:1753, 1754:3395, 3396:4911, 4912:6448, 6449:7955, 
               7956:9347, 9348:10852, 10853:12520, 12521:14345, 14346:16124)
  
  # params
  params = list(booster='gbtree',
                objective='reg:logistic',
                eta=runif(1, 0.05, 0.1),
                subsample=runif(1, 0.7, 0.99),
                colsample_bytree=runif(1, 0.5, 0.99),
                max_depth=sample(3:8, 1),
                max_delta_step = sample(0:3, 1),
                verbose=1,
                silent=1,
                nthread=10)
  
  # cv
  set.seed(1+t)
  cv = xgb1.cv(params = params, traindata = train, traintarget = traintarget,
               folds = folds, nrounds= 1000, prediction=T, early_stopping_rounds = 2)
  
  result_new <- data.frame(ID = t,
                           myparam_colsample_bytree = params$colsample_bytree,
                           myparam_subsample = params$subsample,
                           myparam_eta = params$eta,
                           myparam_max_depth = params$max_depth,
                           myparam_max_delta_step = params$max_delta_step,
                           best_nrounds = cv$best_nrounds,
                           best_score = paste0(cv$hist[[cv$best_nrounds]], collapse=' '),
                           best_cvtestmean = cv$hist[[cv$best_nrounds]][3])
  
  result_all <- rbind(result_all, result_new)
  
  write.table(result_all, paste0('data/working/',prj_name,"/result_all.csv"), sep = ",", row.names = FALSE, quote=F, col.names = T)
  write.table(cv$pred, paste0('data/working/',prj_name,'/cv_preds/cv_pred_',t,'.csv'), sep = ",", row.names = FALSE, quote=F, col.names = T)
  
  # train
  set.seed(1+t)
  res = xgb1.train(params = params, train = train, traintarget = traintarget, test = test, nrounds = cv$best_nrounds)
  write.table(res$pred, paste0('data/working/',prj_name,'/test_preds/test_pred_',t,'.csv'), sep = ",", row.names = FALSE, quote=F, col.names = T)
  
  # featimp
  imp = lapply(1:20, function(c) {
    imp = xgb.importance(feature_names = names(test), model = res$model[[c]])
    names(imp)[-1] = paste0(names(imp)[-1],'_',c)
    return(imp)
  }) %>% Reduce(function(...) merge(..., all=T, by='Feature'), .)
  write.table(imp, paste0('data/working/',prj_name,'/feat_imps/feat_imp_',t,'.csv'), sep = ",", row.names = FALSE, quote=F, col.names = T)
  
  gc()
  rm(train, traintarget, test, fnames, cv, res, imp, params, folds)
  gc()
  gc()
}












