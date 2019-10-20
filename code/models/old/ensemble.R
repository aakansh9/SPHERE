for(w1 in seq(0,1,0.01)){
  print(w1)
  a = w1*train[[2]] + (1-w1)*train[[6]]
  a = t(apply(a, MARGIN = 1, function(x) x/sum(x)))
  print(apply((traintarget-a)^2, MARGIN = 1, function(x) sum(x*cweights)) %>% mean)
}

fnames =  list.files(path = 'data/working/models/', pattern = 'test_[a-zA-Z0-9_]+.csv$') %>% paste0('data/working/models/',.)
test = lapply(fnames, function(x) {
  fread(x) %>% data.frame
}) 
a = test[[2]]*0.77 + (1-0.77)*test[[6]]
a = t(apply(a, MARGIN = 1, function(x) x/sum(x)))
samplesub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv')
samplesub[,4:23] = a
write.table(samplesub, 'data/working/subs/en_1.csv', quote=F, sep=',', row.names = F, col.names = T)



library(xgboost)
library(dplyr)
library(caret)
library(data.table)

result_all <- NULL
prj_name <- 'xgboost_ensemble_lag_2_4'

set.seed(1)

for(t in 1:20){
  
  cat('t = ', t, '\n',sep='')
  
  # load train, traintarget
  fnames =  list.files(path = 'data/working/models/', pattern = 'cv_[a-zA-Z0-9_]+.csv$') %>% paste0('data/working/models/',.)
  #train  = lapply(fnames[c(2,5,6)], function(x){
  #  t = fread(x) %>% data.frame
  #  names(t) = paste0(names(t),'_',x)
  #  t
  #}) %>% do.call(cbind.data.frame, .) %>% data.frame
  
  tmp = fread(fnames[2]) %>% data.frame
  #tmp1 = fread(fnames[1]) %>% data.frame
  #a = (tmp + tmp1)/2
  train = cbind.data.frame(tmp, apply(tmp, MARGIN = 2, lag, n=2))
  train = cbind.data.frame(train, apply(tmp, MARGIN = 2, lag, n=4))
  #train = cbind.data.frame(train, apply(tmp, MARGIN = 2, lag, n=3))
  
  #train = apply(train, MARGIN = 1, function(x) as.integer(x>=max(x))) %>% t %>% data.frame
  #target = apply(traintarget, MARGIN = 1, function(x) as.integer(x>=max(x))) %>% t %>% data.frame
  


  

  traintarget = read.csv('data/working/features/train.target')[,-(1:4)]
  traintarget = traintarget[complete.cases(traintarget),]
  
  
  # load test
  #fnames = list.files(path = 'data/working/features/', pattern = 'test_[a-zA-Z0-9_]+.csv$') %>% paste0('data/working/features/',.)
  #test  = lapply(fnames, fread) %>% do.call(cbind.data.frame, .) %>% data.frame
  

  fnames =  list.files(path = 'data/working/models/', pattern = 'test_[a-zA-Z0-9_]+.csv$') %>% paste0('data/working/models/',.)
  tmp = fread(fnames[2]) %>% data.frame
  test = cbind.data.frame(tmp, apply(tmp, MARGIN = 2, lag, n=2))
  test = cbind.data.frame(test, apply(tmp, MARGIN = 2, lag, n=4))
  #test = cbind.data.frame(test, apply(tmp, MARGIN = 2, lag, n=3))
  samplesub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv')
  test[which(samplesub$end %in% c(1:4)), 21:60] = NA 

  
  # cv folds
  folds = list(1:1753, 1754:3395, 3396:4911, 4912:6448, 6449:7955, 
               7956:9347, 9348:10852, 10853:12520, 12521:14345, 14346:16124)
  
  # params
  params = list(booster='gbtree',
                objective='reg:logistic',
                eta=runif(1, 0.05, 0.1),# 0.07978238
                subsample=runif(1, 0.7, 0.99), #0.7489838
                colsample_bytree=runif(1, 0.7, 0.99), # 0.758718
                max_depth=sample(3:8, 1), #  4
                max_delta_step = sample(0:3, 1), # 2
                verbose=1,
                silent=1,
                nthread=10)
  
  # cv
  set.seed(2)
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
  set.seed(2)
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






samplesub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv')
ens = read.csv('data/working/xgboost_ensemble/test_preds/test_pred_1.csv')
xgb = read.csv('data/working/models/test_pred_1_xgboost_july28_FEATPRUNE2_01823.csv')
samplesub[,4:23] = ens
samplesub[which(samplesub$end %in% c(1,2)),4:23] = xgb[which(samplesub$end %in% c(1,2)), ]

write.table(samplesub, 'data/working/subs/ens_2.csv', quote=F, sep=',', row.names = F, col.names = T)
# 0.1545





