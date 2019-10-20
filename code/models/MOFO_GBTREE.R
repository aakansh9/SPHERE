
seed=0
proj_name='MOFO_GBTREE1'

source('~/SPHERE-Challenge/code/models/xgboost_functions.R')

# folds
folds = list(1:1753, 1754:3395, 3396:4911, 4912:6448, 6449:7955, 
             7956:9347, 9348:10852, 10853:12520, 12521:14345, 14346:16124)


print('Loading train')
# train
fnames = list.files(path = 'data/working/L2_features/', pattern = '[a-zA-Z0-9_]+TR.csv$') %>% paste0('data/working/L2_features/',.)
train = lapply(fnames, fread) %>% do.call(cbind, .) %>% data.frame
#rem = remDupcols(train)
#rem = c(1203,1204,1205,1206,1207,1208,1209,1210,1211,1212,1213,1960,1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,2188,2189,2190,2191,2192,2193,2194,2195,2196,2197,2198,2945,2946,2947,2948,2949,2950,2951,2952,2953,2954,2955,2956,2957,2958,2959,2960,2961,2962,2963,2964,2965,2966,2967,2968,2969,2970,3173,3174,3175,3176,3177,3178,3179,3180,3181,3182,3183,3930,3931,3932,3933,3934,3935,3936,3937,3938,3939,3940,3941,3942,3943,3944,3945,3946,3947,3948,3949,3950,3951,3952,3953,3954,3955,4158,4159,4160,4161,4162,4163,4164,4165,4166,4167,4168,4915,4916,4917,4918,4919,4920,4921,4922,4923,4924,4925,4926,4927,4928,4929,4930,4931,4932,4933,4934,4935,4936,4937,4938,4939,4940,5143,5144,5145,5146,5147,5148,5149,5150,5151,5152,5153,5900,5901,5902,5903,5904,5905,5906,5907,5908,5909,5910,5911,5912,5913,5914,5915,5916,5917,5918,5919,5920,5921,5922,5923,5924,5925,5926,6001,6002,6011,6087,6096,6172,6181,6256,6257,6266,6342,6351,6427,6436,6511,6512,6521,6597,6606,6682,6691,6766,6767,6776,6852,6861,6937,6946,7021,7022,7031,7107,7116,7192)
#train = train[,-rem]

print('Loading test')
# test
fnames = list.files(path = 'data/working/L2_features/', pattern = '[a-zA-Z0-9_]+TT.csv$') %>% paste0('data/working/L2_features/',.)
test = lapply(fnames, fread) %>% do.call(cbind, .) %>% data.frame
#test = test[,-rem]

print('Loading target')
# target
traintarget = read.csv('data/working/features/train.target')[,-(1:4)]
traintarget = traintarget[complete.cases(traintarget),]

# params
set.seed(seed)
params = list(booster='gbtree', objective='reg:logistic', eta=runif(1, 0.05, 0.1),
              subsample=runif(1, 0.7, 0.99), colsample_bytree=runif(1, 0.1, 0.5),
              max_depth=sample(8:15, 1), max_delta_step = sample(0:5, 1),
              verbose=1,silent=1,nthread=40)

#set.seed(seed)
#s = sample(1:ncol(train), 900)

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
nlags=0:4
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
dir.create(paste0('data/working/LAGGER_MODELS/',proj_name), recursive = T)
write.table(paras, paste0('data/working/LAGGER_MODELS/',proj_name,'/paras.csv'), sep = ",", row.names = FALSE, quote=F, col.names = T)
write.table(cv$pred, paste0('data/working/LAGGER_MODELS/',proj_name,'/pred_TR.csv'), sep = ",", row.names = FALSE, quote=F, col.names = T)
write.table(res$pred, paste0('data/working/LAGGER_MODELS/',proj_name,'/pred_TT.csv'), sep = ",", row.names = FALSE, quote=F, col.names = T)
write.table(imp, paste0('data/working/LAGGER_MODELS/',proj_name,'/imp.csv'), sep = ",", row.names = FALSE, quote=F, col.names = T)


