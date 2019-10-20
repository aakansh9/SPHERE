require(dplyr)

############# window features function #############

window_features <- function(t_start, t_end, quantile_probs=seq(0,1,0.25), df, cols, colnames_prefix= NULL, FUN = NULL, n, ...){
  
  window_data = df[which(df$t >= t_start & df$t < t_end), cols, drop=F]
  if(!is.null(FUN))
    if(nrow(window_data) >= n)
      window_data = lapply(window_data, FUN=FUN, n=n, ...) %>% do.call(cbind, .) %>% data.frame
    
    # quantiles
    res = unlist(lapply(window_data, quantile, probs = quantile_probs, names=F, na.rm=T))
    # means
    res = c(res, unlist(lapply(window_data, mean, na.rm=T)))
    # sds
    res = c(res, unlist(lapply(window_data, sd, na.rm=T)))
    # names
    names(res) = c(unlist(lapply(1:length(cols), function(i){paste0(colnames_prefix,cols[i],'_W',t_end-t_start,'_',quantile_probs*100, 'Q')})),
                   paste0(colnames_prefix,cols, '_W', t_end-t_start, '_mean'),
                   paste0(colnames_prefix,cols, '_W', t_end-t_start, '_sd'))
    return(res)
}

############# statistical features of raw in {1}-sec window #############

# train
train_AP_feat1 <- lapply(1:10, function(i){
  print(i)
  accel <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/acceleration.csv'))
  target <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/targets.csv'))
  feat <- lapply(target$end, function(e){
    w1 = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.25), df=accel, cols=c('Kitchen_AP','Lounge_AP','Upstairs_AP','Study_AP'), colnames_prefix= 'raw_')
    return(c(w1))
  }) %>% do.call(rbind, .) %>% data.frame
}) %>% do.call(rbind, .) %>% data.frame

# test
sub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv') %>% data.table
t_ends = sub[,by='record_id', max(end)]$V1 # 5-29 sec

test_AP_feat1 <- lapply(11:882, function(i){
  print(i)
  accel <- read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/acceleration.csv'))
  feat <- lapply(1:t_ends[i-10], function(e){
    w1 = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.25), df=accel, cols=c('Kitchen_AP','Lounge_AP','Upstairs_AP','Study_AP'),  colnames_prefix= 'raw_')
    return(c(w1))
  }) %>% do.call(rbind, .) %>% data.frame
  # NaNs, NAs if accel is empty
  return(feat)
}) %>% do.call(rbind, .) %>% data.frame

# remove infinite values
train_AP_feat1[is.nan(train_AP_feat1)] = NA
test_AP_feat1[is.nan(test_AP_feat1)] = NA

train_AP_feat1[is.na(train_AP_feat1)] = 0
test_AP_feat1[is.na(test_AP_feat1)] = 0

# save
write.table(train_AP_feat1, 'data/working/features/train_AP_feat1.csv', col.names = T, row.names = F, quote=F, sep=',')
write.table(test_AP_feat1, 'data/working/features/test_AP_feat1.csv', col.names = T, row.names = F, quote=F, sep=',')


############# statistical features of moving mean in {1}-sec window #############

# train
train_AP_feat2 <- lapply(1:10, function(i){
  print(i)
  accel <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/acceleration.csv'))
  target <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/targets.csv'))
  feat <- lapply(target$end, function(e){
    w1 = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.5), df=accel, cols=c('Kitchen_AP','Lounge_AP','Upstairs_AP','Study_AP'), FUN = roll_mean, n=5,  colnames_prefix= 'roll_mean_5_')
    return(c(w1))
  }) %>% do.call(rbind, .) %>% data.frame
}) %>% do.call(rbind, .) %>% data.frame

# test
sub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv') %>% data.table
t_ends = sub[,by='record_id', max(end)]$V1 # 5-29 sec

test_AP_feat2 <- lapply(11:882, function(i){
  print(i)
  accel <- read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/acceleration.csv'))
  feat <- lapply(1:t_ends[i-10], function(e){
    w1 = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.5), df=accel, cols=c('Kitchen_AP','Lounge_AP','Upstairs_AP','Study_AP'), FUN = roll_mean, n=5,  colnames_prefix= 'roll_mean_5_')
    return(c(w1))
  }) %>% do.call(rbind, .) %>% data.frame
  # NaNs, NAs if accel is empty
  return(feat)
}) %>% do.call(rbind, .) %>% data.frame

# remove infinite values
train_AP_feat2[is.nan(train_AP_feat2)] = NA
test_AP_feat2[is.nan(test_AP_feat2)] = NA

train_AP_feat2[is.na(train_AP_feat2)] = 0
test_AP_feat2[is.na(test_AP_feat2)] = 0

# save
write.table(train_AP_feat2, 'data/working/features/train_AP_feat2.csv', col.names = T, row.names = F, quote=F, sep=',')
write.table(test_AP_feat2, 'data/working/features/test_AP_feat2.csv', col.names = T, row.names = F, quote=F, sep=',')

############# statistical features of binarized in {1}-sec window #############

# train
train_AP_feat3 <- lapply(1:10, function(i){
  print(i)
  accel <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/acceleration.csv'))
  accel[!is.na(accel)] = 1
  accel[is.na(accel)] = 0
  target <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/targets.csv'))
  feat <- lapply(target$end, function(e){
    w1 = window_features(t_start=e-1, t_end=e, quantile_probs=c(1), df=accel, cols=c('Kitchen_AP','Lounge_AP','Upstairs_AP','Study_AP'), colnames_prefix= 'binarized_')
    return(c(w1))
  }) %>% do.call(rbind, .) %>% data.frame
}) %>% do.call(rbind, .) %>% data.frame

# test
sub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv') %>% data.table
t_ends = sub[,by='record_id', max(end)]$V1 # 5-29 sec

test_AP_feat3 <- lapply(11:882, function(i){
  print(i)
  accel <- read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/acceleration.csv'))
  feat <- lapply(1:t_ends[i-10], function(e){
    w1 = window_features(t_start=e-1, t_end=e, quantile_probs=c(1), df=accel, cols=c('Kitchen_AP','Lounge_AP','Upstairs_AP','Study_AP'), colnames_prefix= 'binarized_')
    return(c(w1))
  }) %>% do.call(rbind, .) %>% data.frame
  # NaNs, NAs if accel is empty
  return(feat)
}) %>% do.call(rbind, .) %>% data.frame

# remove infinite values
train_AP_feat3[is.nan(train_AP_feat3)] = NA
test_AP_feat3[is.nan(test_AP_feat3)] = NA

train_AP_feat3[is.na(train_AP_feat3)] = 0
test_AP_feat3[is.na(test_AP_feat3)] = 0

# save
write.table(train_AP_feat3, 'data/working/features/train_AP_feat3.csv', col.names = T, row.names = F, quote=F, sep=',')
write.table(test_AP_feat3, 'data/working/features/test_AP_feat3.csv', col.names = T, row.names = F, quote=F, sep=',')






