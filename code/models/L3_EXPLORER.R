L3_EXPLORER <- function(m=c(1,2), nlags=0:2, seed=0, proj_name='ET1_LAGGER1'){
  
  source('~/SPHERE-Challenge/code/models/xgboost_functions.R')
  
  # folds
  folds = list(1:1753, 1754:3395, 3396:4911, 4912:6448, 6449:7955, 
               7956:9347, 9348:10852, 10853:12520, 12521:14345, 14346:16124)
  
  
  print('Loading train')
  # train
  fnames = list.files(path = 'data/working/LAGGER_MODELS/', pattern = '[a-zA-Z0-9_]+TR.csv$', recursive = T) %>% paste0('data/working/LAGGER_MODELS/',.)
  train = lapply(fnames[m], fread) %>% do.call(cbind, .) %>% data.frame
  #nms = lapply(m, function(n) paste0('V',1:20,'_MODEL',n)) %>% unlist
  #colnames(train)=nms
  train = lapply(nlags, function(n) apply(train, MARGIN = 2, lag, n=n)) %>% Reduce('cbind',.)
  #colnames(train) = lapply(nlags, function(n) paste0(nms,'_LAG',n)) %>% unlist
  
  #for(n in nlags[nlags > 0]){
  #  train[unlist(lapply(1:n, function(k) unlist(lapply(folds, '[', 1)) + k-1)), paste0(nms,'_LAG',n)]=NA
  #}
  
  print('Loading test')
  # test
  fnames = list.files(path = 'data/working/LAGGER_MODELS/', pattern = '[a-zA-Z0-9_]+TT.csv$', recursive = T) %>% paste0('data/working/LAGGER_MODELS/',.)
  test = lapply(fnames[m], fread) %>% do.call(cbind, .) %>% data.frame
  test = lapply(nlags, function(n) apply(test, MARGIN = 2, lag, n=n)) %>% Reduce('cbind',.)
  colnames(test) = colnames(train)
  
  print('Loading target')
  # target
  traintarget = read.csv('data/working/features/train.target')[,-(1:4)]
  traintarget = traintarget[complete.cases(traintarget),]
  
  # params
  set.seed(seed)
  params = list(booster='gbtree', objective='reg:logistic', eta=runif(1, 0.05, 0.1),
                subsample=runif(1, 0.7, 0.99), colsample_bytree=runif(1, 0.3, 0.99),
                max_depth=sample(3:8, 1), max_delta_step = sample(0:3, 1),
                verbose=1,silent=1,nthread=10)
  
  print('CVing')
  # cv
  set.seed(seed+1)
  cv = xgb1.cv(params = params, traindata = train, traintarget = traintarget,
               folds = folds, nrounds= 1000, prediction=T, early_stopping_rounds = 2)
  
  
  print('Final Modeling')
  # predict
  set.seed(seed+2)
  res = xgb1.train(params = params, train = train, traintarget = traintarget, test = test, nrounds = cv$best_nrounds)
  
  # correct pred
  samplesub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv')
  res$pred[which(samplesub$end %in% c(1:max(nlags))), ] = NA
  
  # feat imp
  imp = lapply(1:20, function(c) {
    imp = xgb.importance(feature_names = colnames(test), model = res$model[[c]])
    names(imp)[-1] = paste0(names(imp)[-1],'_',c)
    return(imp)
  }) %>% Reduce(function(...) merge(..., all=T, by='Feature'), .)
  
  print('Saving data')
  # save
  paras = data.frame(myparam_colsample_bytree = params$colsample_bytree,
                     myparam_subsample = params$subsample,
                     myparam_eta = params$eta,
                     myparam_max_depth = params$max_depth,
                     myparam_max_delta_step = params$max_delta_step,
                     best_nrounds = cv$best_nrounds,
                     best_score = paste0(cv$hist[[cv$best_nrounds]], collapse=' '),
                     best_cvtestmean = cv$hist[[cv$best_nrounds]][3])
  dir.create(paste0('data/working/L3_MODELS/',proj_name), recursive = T)
  write.table(paras, paste0('data/working/L3_MODELS/',proj_name,'/paras.csv'), sep = ",", row.names = FALSE, quote=F, col.names = T)
  write.table(cv$pred, paste0('data/working/L3_MODELS/',proj_name,'/pred_TR.csv'), sep = ",", row.names = FALSE, quote=F, col.names = T)
  write.table(res$pred, paste0('data/working/L3_MODELS/',proj_name,'/pred_TT.csv'), sep = ",", row.names = FALSE, quote=F, col.names = T)
  write.table(imp, paste0('data/working/L3_MODELS/',proj_name,'/imp.csv'), sep = ",", row.names = FALSE, quote=F, col.names = T)
  
}

L3_EXPLORER(m = c(46,31), nlags = 0:4, seed = 1, proj_name = 'RF1_RF2_L3_XGBTREE')
gc()


