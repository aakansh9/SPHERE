# train index
train_index <- lapply(1:10, function(i){
  target <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/targets.csv'))
  target <- cbind.data.frame('folder' = rep(i, nrow(target)), target[,1:2])
}) %>% do.call(rbind, .) %>% data.frame
train_index <- cbind.data.frame('index' = 1:nrow(train_index), train_index)
write.table(train_index, 'data/working/features/train.index', col.names = T, row.names = F, quote=F, sep=',')

# test index
sub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv')
test_index <- cbind.data.frame('index' = 1:nrow(sub), 'folder' = sub$record_id, sub[,2:3])
write.table(test_index, 'data/working/features/test.index', col.names = T, row.names = F, quote=F, sep=',')

# train target
traintarget = lapply(1:10, function(i){
  read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/targets.csv'))
}) %>% do.call(rbind, .) %>% data.frame
traintarget <- cbind.data.frame('index' = 1:nrow(traintarget), 'folder' = train_index$folder, traintarget)
write.table(traintarget, 'data/working/features/train.target', col.names = T, row.names = F, quote=F, sep=',')

# CV folds
train_index = train_index[complete.cases(traintarget),]
folds = list(1:1753, 1754:3395, 3396:4911, 4912:6448, 6449:7955, 
             7956:9347, 9348:10852, 10853:12520, 12521:14345, 14346:16124)
#folds = lapply(1:length(folds), function(x) c(min(train_index$index[folds[[x]]]), max(train_index$index[folds[[x]]]))) %>% do.call(rbind, .) %>% data.frame
folds = lapply(1:length(folds), function(x) c(min(folds[[x]])-1, max(folds[[x]])-1)) %>% do.call(rbind, .) %>% data.frame

write.table(folds, 'data/working/features/train.folds', col.names = F, row.names = F, quote=F, sep=',')







