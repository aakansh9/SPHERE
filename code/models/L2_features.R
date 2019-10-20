###################################################

###### lagged predictions of L1 models

dir.create('data/working/L2_features')

for (m in 1:10){
  
  # TR
  folds = list(1:1753, 1754:3395, 3396:4911, 4912:6448, 6449:7955, 
               7956:9347, 9348:10852, 10853:12520, 12521:14345, 14346:16124)
  nlags=0:4
  fnames = list.files(path = 'data/working/models/', pattern = '[a-zA-Z0-9_]+TR.csv$') %>% paste0('data/working/models/',.)
  train = fread(fnames[m]) %>% data.frame
  train = lapply(nlags, function(n) apply(train, MARGIN = 2, lag, n=n)) %>% Reduce('cbind',.)
  colnames(train) = lapply(nlags, function(n) paste0('V',1:20,'_LAG',n)) %>% unlist
  for(n in nlags[nlags > 0]){
    train[unlist(lapply(1:n, function(k) unlist(lapply(folds, '[', 1)) + k-1)), paste0('V',1:20,'_LAG',n)]
  }
  mnames = unlist(strsplit(unlist(lapply(strsplit(fnames, 's/'), '[',2)), '.csv'))
  train = data.frame(train)
  write.table(train, paste0('data/working/L2_features/LAGGED_PRED_',mnames[m],'.csv'), row.names = F, col.names = T, quote = F, sep=',')
  
  # test
  fnames = list.files(path = 'data/working/models/', pattern = '[a-zA-Z0-9_]+TT.csv$') %>% paste0('data/working/models/',.)
  test = fread(fnames[m]) %>% data.frame
  test = lapply(nlags, function(n) apply(test, MARGIN = 2, lag, n=n)) %>% Reduce('cbind',.)
  colnames(test) = colnames(train)
  test = data.frame(test)
  mnames = unlist(strsplit(unlist(lapply(strsplit(fnames, 's/'), '[',2)), '.csv'))
  write.table(test, paste0('data/working/L2_features/LAGGED_PRED_',mnames[m],'.csv'), row.names = F, col.names = T, quote = F, sep=',')
  
  
}

###################################################

###### Lagged raw AKA features

# train
nlags=0:4
fnames = list.files(path = 'data/working/features/', pattern = 'train_[a-zA-Z0-9_]+.csv$') %>% paste0('data/working/features/',.)
train  = lapply(fnames, fread) %>% do.call(cbind.data.frame, .) %>% data.frame
traintarget = read.csv('data/working/features/train.target')[,-(1:4)]
train = train[complete.cases(traintarget),]
nms = colnames(train)
train = lapply(nlags, function(n) apply(train, MARGIN = 2, lag, n=n)) %>% Reduce('cbind',.)
colnames(train) = lapply(nlags, function(n) paste0(nms,'_LAG',n)) %>% unlist
train = data.frame(train)
write.table(train, paste0('data/working/L2_features/LAGGED_RAW_AKA_FEAT_TR.csv'), row.names = F, col.names = T, quote = F, sep=',')

# test
fnames = list.files(path = 'data/working/features/', pattern = 'test_[a-zA-Z0-9_]+.csv$') %>% paste0('data/working/features/',.)
test  = lapply(fnames, fread) %>% do.call(cbind.data.frame, .) %>% data.frame
test = lapply(nlags, function(n) apply(test, MARGIN = 2, lag, n=n)) %>% Reduce('cbind',.)
colnames(test) = colnames(train)
test = data.frame(test)
write.table(test, paste0('data/working/L2_features/LAGGED_RAW_AKA_FEAT_TT.csv'), row.names = F, col.names = T, quote = F, sep=',')

###################################################

###### Lagged raw GUP features

# train
nlags=0:4
fnames = list.files(path = 'data/working/guppsyfeatures/', pattern = 'train_[a-zA-Z0-9_]+.csv$') %>% paste0('data/working/guppsyfeatures/',.)
train  = lapply(fnames, fread) %>% do.call(cbind.data.frame, .) %>% data.frame
traintarget = read.csv('data/working/features/train.target')[,-(1:4)]
train = train[complete.cases(traintarget),]
nms = colnames(train)
train = lapply(nlags, function(n) apply(train, MARGIN = 2, lag, n=n)) %>% Reduce('cbind',.)
colnames(train) = lapply(nlags, function(n) paste0(nms,'_LAG',n)) %>% unlist
train = data.frame(train)
write.table(train, paste0('data/working/L2_features/LAGGED_RAW_GUP_FEAT_TR.csv'), row.names = F, col.names = T, quote = F, sep=',')

# test
fnames = list.files(path = 'data/working/guppsyfeatures/', pattern = 'test_[a-zA-Z0-9_]+.csv$') %>% paste0('data/working/guppsyfeatures/',.)
test  = lapply(fnames, fread) %>% do.call(cbind.data.frame, .) %>% data.frame
test = lapply(nlags, function(n) apply(test, MARGIN = 2, lag, n=n)) %>% Reduce('cbind',.)
colnames(test) = colnames(train)
test = data.frame(test)
write.table(test, paste0('data/working/L2_features/LAGGED_RAW_GUP_FEAT_TT.csv'), row.names = F, col.names = T, quote = F, sep=',')

###################################################


