require(dplyr)
require(data.table)


active_sensors <- function(t_start, t_end, pir_data){
  rem = which((pir_data$end <= t_start) | (pir_data$start > t_end))
  if ( length(rem) >0 )
    res = pir_data[-rem,'index'] %>% unique
  else
    res = pir_data[,'index'] %>% unique
  return(res)
}

# train
train_PIR_feat1 <- lapply(1:10, function(i){
  print(i)
  pir <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/pir.csv'))
  target <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/targets.csv'))
  feat = matrix(0, nrow = nrow(target), ncol = 9, dimnames = list(NULL, paste0('room_',c(0:8)))) %>% data.frame
  for(e in 1:nrow(feat)){
    w1 = active_sensors(t_start = e-1, t_end = e, pir_data=pir)
    feat[e,w1+1]=1
  }
  feat$nrooms = rowSums(feat)
  return(feat)
}) %>% do.call(rbind, .) %>% data.frame


# test
sub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv') %>% data.table
t_ends = sub[,by='record_id', max(end)]$V1 # 5-29 sec

test_PIR_feat1 <- lapply(11:882, function(i){
  print(i)
  pir <- read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/pir.csv'))
  feat = matrix(0, nrow = t_ends[i-10], ncol = 9, dimnames = list(NULL, paste0('room_',c(0:8)))) %>% data.frame
  for(e in 1:nrow(feat)){
    w1 = active_sensors(t_start = e-1, t_end = e, pir_data=pir)
    feat[e,w1+1]=1
  }
  feat$nrooms = rowSums(feat)
  return(feat)
}) %>% do.call(rbind, .) %>% data.frame


# save
write.table(train_PIR_feat1, 'data/working/features/train_PIR_feat1.csv', col.names = T, row.names = F, quote=F, sep=',')
write.table(test_PIR_feat1, 'data/working/features/test_PIR_feat1.csv', col.names = T, row.names = F, quote=F, sep=',')


# hall, stairs can be predicted by transitions
# stairs = ascend, descend, 
# hall = walk, loadwalk, ...
# some pir sensors ex hall, study are not reliable



# PIR (passive infrared) sensors are located in all 0-8 (9 rooms).
# Sensors may have false negative and false positive acitvations.
# We produce best possible predictions of location in every 1 sec. window just based on PIR data.
# A person spends most of time in living, kitchen, bed2
# Minor times are spent on stairs, hall, study



