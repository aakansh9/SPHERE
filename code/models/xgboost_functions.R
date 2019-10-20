library(xgboost) # 0.4-3
library(dplyr) # 0.5.0

xgb1.cv <- function(params = list(), traindata, traintarget, folds = list(), nrounds= 10, prediction=F, early_stopping_rounds=NULL){
  gc()
  
  ### eval metric
  cweights = c(1.35298455691,1.38684574053,1.59587388404,1.35318713948,0.347783666015,
               0.661081706198,1.04723628621,0.398865222651,0.207586320237,1.50578335208,
               0.110181365961,1.07803284435,1.36560417316,1.17024113802,1.1933637414,
               1.1803704493,1.34414875433,1.11683830693,1.08083910312,0.503152249073)
  BS <- function(preds, train){
    labels = lapply(1:20, function(c) getinfo(train[[c]], 'label'))
    err = lapply(1:20, function(c) sum(cweights[c]*(preds[[c]] - labels[[c]])^2)/length(labels[[c]]))
    err = sum(unlist(err))
    return(c('BS'=err))
  }
  
  ### create 20-D Dmatrix
  data <- lapply(1:20, function(c) xgb.DMatrix(data=data.matrix(traindata), label=as.numeric(traintarget[,c]), missing=NA))
  
  ### Initiate boosters
  booster_folds <- lapply(1:length(folds), function(k){
    test = lapply(1:20, function(c) xgboost:::slice(data[[c]], folds[[k]]))
    train = lapply(1:20, function(c) xgboost:::slice(data[[c]], unlist(folds[-k])))
    booster = lapply(1:20, function(c) xgboost:::xgb.Booster(params = params, cachelist = list(train[[c]], test[[c]])))
    res = list(train=train, test=test, booster=booster)
    return(res)
  })
  gc()
  
  ### start nrounds training
  
  hist = list()
  best_score = Inf
  best_iter = 0
  for(i in 1:nrounds){
    
    # single CV iteration
    res = lapply(1:length(booster_folds), function(j){
      fold = booster_folds[[j]]
      # iterate 20-D Dmatrix by 1
      succ = lapply(1:20, function(c) xgboost:::xgb.iter.update(booster=fold$booster[[c]], dtrain=fold$train[[c]],iter= i - 1, obj=NULL))
      # evaluate score
      gc()
      preds_train = lapply(1:20, function(c) predict(fold$booster[[c]], fold$train[[c]]))
      preds_test = lapply(1:20, function(c) predict(fold$booster[[c]], fold$test[[c]]))
      
      norm_train = colSums(do.call('rbind', preds_train))
      norm_test = colSums(do.call('rbind', preds_test))
      preds_train = lapply(1:20, function(c) preds_train[[c]]/norm_train)
      preds_test = lapply(1:20, function(c) preds_test[[c]]/norm_test)
      eval_train = BS(preds_train, fold$train); names(eval_train) = 'train-BS'
      eval_test = BS(preds_test, fold$test); names(eval_test) = 'test-BS'
      rm(preds_train, preds_test, norm_train, norm_test)
      gc()
      return(c(eval_train, eval_test))
    })
    
    # calc CV score
    res = do.call('rbind', res) %>% as.data.frame
    res = c('train-mean' = mean(res[,1]), 'train-sd' = sd(res[,1]), 'test-mean' =mean(res[,2]), 'test-sd'=sd(res[,2]))
    cat('[',i,'] ',res[1],'+',res[2],'  ',res[3],'+',res[4],'\n', sep='')
    
    # update best_score
    if (res[3] < best_score){
      best_score = res[3]
      best_iter = i
    }
    #cat('best_score = ', best_score, ', best_iter = ',best_iter,', current_round = ',i,'\n',sep='')
    
    if(!is.null(early_stopping_rounds))
      if(i-best_iter >= early_stopping_rounds)
        break

    hist[[i]] = res
    gc()
  }
  
  ### return
  if(prediction == T){
    pred = lapply(1:length(booster_folds), function(j){
      fold = booster_folds[[j]]
      preds_test = lapply(1:20, function(c) predict(fold$booster[[c]], fold$test[[c]], ntreelimit = best_iter))
      norm_test = colSums(do.call('rbind', preds_test))
      preds_test = lapply(1:20, function(c) preds_test[[c]]/norm_test) %>% do.call(cbind, .) %>% data.frame
      gc()
      return(preds_test)
    }) %>% do.call(rbind, .) %>% data.frame
  } else {
    pred = NULL
  }
  gc()
  return(list('pred'=pred, 'hist'=hist, 'best_nrounds' = best_iter))
}

###########################################################################

xgb1.train <- function(params = list(), train, traintarget, test, nrounds){
  
  # create 20-D Dmatrix
  gc()
  dtrain <- lapply(1:20, function(c) xgb.DMatrix(data=data.matrix(train), label=as.numeric(traintarget[,c]), missing=NA))
  
  # initiate boosters
  booster = lapply(1:20, function(c) xgboost:::xgb.Booster(params = params, cachelist = list(dtrain[[c]])))
  
  # start training
  for(i in 1:nrounds){ # iterate over rounds
    cat('[',i,']\n',sep='')
    # update 20-D Dmatrix by 1
    succ = lapply(1:20, function(c) xgboost:::xgb.iter.update(booster=booster[[c]], dtrain=dtrain[[c]],iter= i - 1, obj=NULL))
    gc()
  }
  
  # predict
  dtest <- xgb.DMatrix(data=data.matrix(test), missing=NA)
  preds_test = lapply(1:20, function(c) predict(booster[[c]], dtest))
  norm_test = colSums(do.call('rbind', preds_test))
  preds_test = lapply(1:20, function(c) preds_test[[c]]/norm_test) %>% do.call(cbind, .) %>% data.frame
  gc()
  
  # final 20-D model
  model = lapply(1:20, function(c) {
    bst <- xgboost:::xgb.handleToBooster(booster[[c]])
    bst <- xgboost:::xgb.Booster.check(bst, saveraw = TRUE)
    bst$niter = nrounds
    bst$params <- params
    return(bst)
  })
  gc()
  return(list('pred' = preds_test, 'model' = model))
}

###########################################################################

