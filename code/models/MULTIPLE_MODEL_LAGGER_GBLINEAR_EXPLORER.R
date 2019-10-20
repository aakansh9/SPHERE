MULTIPLE_MODEL_LAGGER_EXPLORER <- function(m=c(1,2), nlags=0:2, seed=0, proj_name='ET1_LAGGER1'){
  
  source('~/SPHERE-Challenge/code/models/xgboost_functions_GBLINEAR.R')
  
  # folds
  folds = list(1:1753, 1754:3395, 3396:4911, 4912:6448, 6449:7955, 
               7956:9347, 9348:10852, 10853:12520, 12521:14345, 14346:16124)
  
  
  print('Loading train')
  # train
  fnames = list.files(path = 'data/working/models/', pattern = '[a-zA-Z0-9_]+TR.csv$') %>% paste0('data/working/models/',.)
  train = lapply(fnames[m], fread) %>% do.call(cbind, .) %>% data.frame
  nms = lapply(m, function(n) paste0('V',1:20,'_MODEL',n)) %>% unlist
  colnames(train)=nms
  train = lapply(nlags, function(n) apply(train, MARGIN = 2, lag, n=n)) %>% Reduce('cbind',.)
  colnames(train) = lapply(nlags, function(n) paste0(nms,'_LAG',n)) %>% unlist
  
  for(n in nlags[nlags > 0]){
    train[unlist(lapply(1:n, function(k) unlist(lapply(folds, '[', 1)) + k-1)), paste0(nms,'_LAG',n)]=NA
  }
  
  print('Loading test')
  # test
  fnames = list.files(path = 'data/working/models/', pattern = '[a-zA-Z0-9_]+TT.csv$') %>% paste0('data/working/models/',.)
  test = lapply(fnames[m], fread) %>% do.call(cbind, .) %>% data.frame
  test = lapply(nlags, function(n) apply(test, MARGIN = 2, lag, n=n)) %>% Reduce('cbind',.)
  colnames(test) = colnames(train)
  
  print('Loading target')
  # target
  traintarget = read.csv('data/working/features/train.target')[,-(1:4)]
  traintarget = traintarget[complete.cases(traintarget),]
  
  # params
  set.seed(seed)
  params = list(booster='gblinear', objective='reg:logistic',
                eta = 0.2,alpha = sample(c(0,0.01,0.001,0.00001),1),
                lambda =sample(c(1,2),1),verbose=1,silent=1,nthread=10) #sample(c(1,2),1)
  
  
  print('CVing')
  # cv
  set.seed(seed+1)
  cv = xgb1.cv(params = params, traindata = train, traintarget = traintarget,
               folds = folds, nrounds= 200, prediction=T, early_stopping_rounds = 2)
  
  
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
  paras <- data.frame(myparam_eta = params$eta,
                      myparam_alpha = params$alpha,
                      myparam_lambda = params$lambda,
                      best_nrounds = cv$best_nrounds,
                      best_score = paste0(cv$hist[[cv$best_nrounds]], collapse=' '),
                      best_cvtestmean = cv$hist[[cv$best_nrounds]][3])
  dir.create(paste0('data/working/LAGGER_MODELS/',proj_name), recursive = T)
  write.table(paras, paste0('data/working/LAGGER_MODELS/',proj_name,'/paras.csv'), sep = ",", row.names = FALSE, quote=F, col.names = T)
  write.table(cv$pred, paste0('data/working/LAGGER_MODELS/',proj_name,'/pred_TR.csv'), sep = ",", row.names = FALSE, quote=F, col.names = T)
  write.table(res$pred, paste0('data/working/LAGGER_MODELS/',proj_name,'/pred_TT.csv'), sep = ",", row.names = FALSE, quote=F, col.names = T)
  write.table(imp, paste0('data/working/LAGGER_MODELS/',proj_name,'/imp.csv'), sep = ",", row.names = FALSE, quote=F, col.names = T)
  
}

MULTIPLE_MODEL_LAGGER_EXPLORER(m = c(3,7), nlags = 0:4, seed = 4, proj_name = 'GBLINEAR1_GBTREE3_LAGGER2_XGBLINEAR')
gc()

MULTIPLE_MODEL_LAGGER_EXPLORER(m = c(3,7,9), nlags = 0:3, seed = 8, proj_name = 'GBLINEAR1_GBTREE3_RF1_LAGGER2_XGBLINEAR')
gc()

MULTIPLE_MODEL_LAGGER_EXPLORER(m = c(3,7,9), nlags = 0:2, seed = 7, proj_name = 'GBLINEAR1_GBTREE3_RF1_LAGGER1_XGBLINEAR')
gc()

MULTIPLE_MODEL_LAGGER_EXPLORER(m = c(1,7), nlags = 0:3, seed = 6, proj_name = 'ET1_GBTREE3_LAGGER1_XGBLINEAR')
gc()

MULTIPLE_MODEL_LAGGER_EXPLORER(m = c(9,7), nlags = 0:4, seed = 5, proj_name = 'RF1_GBTREE3_LAGGER1_XGBLINEAR')
gc()

MULTIPLE_MODEL_LAGGER_EXPLORER(m = c(1,9), nlags = 0:4, seed = 2, proj_name = 'ET1_RF1_LAGGER1_XGBLINEAR')
gc()

