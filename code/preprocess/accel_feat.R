require(dplyr)
library(data.table)
#library(zoo)
library(RcppRoll)

############# window features function #############

window_features <- function(t_start, t_end, quantile_probs=seq(0,1,0.25), df, cols, colnames_prefix= NULL, FUN = NULL, n, ...){
  
  window_data = df[which(df$t >= t_start & df$t < t_end), cols, drop=F]
  if(!is.null(FUN))
    if(nrow(window_data) >= n)
      window_data = lapply(window_data, FUN=FUN, n=n, ...) %>% do.call(cbind, .) %>% data.frame

  #print('a')
  # quantiles
  res = unlist(lapply(window_data, quantile, probs = quantile_probs, names=F, na.rm=T))
  #print('a')
  # means
  res = c(res, unlist(lapply(window_data, mean, na.rm=T)))
  #print('a')
  # sds
  res = c(res, unlist(lapply(window_data, sd, na.rm=T)))
  #print('a')
  # names
  names(res) = c(unlist(lapply(1:length(cols), function(i){paste0(colnames_prefix,cols[i],'_W',t_end-t_start,'_',quantile_probs*100, 'Q')})),
                 paste0(colnames_prefix,cols, '_W', t_end-t_start, '_mean'),
                 paste0(colnames_prefix,cols, '_W', t_end-t_start, '_sd'))
  return(res)
}


############# statistical features of raw x, y, z axis in {1}-sec window #############

# train
train_accel_feat1 <- lapply(1:10, function(i){
  print(i)
  accel <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/acceleration.csv'))
  target <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/targets.csv'))
  feat <- lapply(target$end, function(e){
    w1 = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.25), df=accel, cols=c('x','y','z'), colnames_prefix= 'raw_')
    return(c(w1))
  }) %>% do.call(rbind, .) %>% data.frame
}) %>% do.call(rbind, .) %>% data.frame

# test
sub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv') %>% data.table
t_ends = sub[,by='record_id', max(end)]$V1 # 5-29 sec

test_accel_feat1 <- lapply(11:882, function(i){
  print(i)
  accel <- read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/acceleration.csv'))
  feat <- lapply(1:t_ends[i-10], function(e){
    w1 = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.25), df=accel, cols=c('x','y','z'),  colnames_prefix= 'raw_')
    return(c(w1))
  }) %>% do.call(rbind, .) %>% data.frame
  # NaNs, NAs if accel is empty
  return(feat)
}) %>% do.call(rbind, .) %>% data.frame

# remove infinite values
train_accel_feat1[is.nan(train_accel_feat1)] = NA
test_accel_feat1[is.nan(test_accel_feat1)] = NA

# save
write.table(train_accel_feat1, 'data/working/features/train_accel_feat1.csv', col.names = T, row.names = F, quote=F, sep=',')
write.table(test_accel_feat1, 'data/working/features/test_accel_feat1.csv', col.names = T, row.names = F, quote=F, sep=',')

############# statistical features of MA-median x, y, z axis in {1}-sec window #############

# train
train_accel_feat2 <- lapply(1:10, function(i){
  print(i)
  accel <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/acceleration.csv'))
  target <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/targets.csv'))
  feat <- lapply(target$end, function(e){
    #print(e)
    w1 = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.5), df=accel, cols=c('x','y','z'), FUN = roll_median, n=5,  colnames_prefix= 'roll_median_5_')  
    return(c(w1))
  }) %>% do.call(rbind, .) %>% data.frame
}) %>% do.call(rbind, .) %>% data.frame

# test
sub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv') %>% data.table
t_ends = sub[,by='record_id', max(end)]$V1 # 5-29 sec

test_accel_feat2 <- lapply(11:882, function(i){
  print(i)
  accel <- read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/acceleration.csv'))
  feat <- lapply(1:t_ends[i-10], function(e){
    w1 = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.5), df=accel, cols=c('x','y','z'), FUN = roll_median, n=5, colnames_prefix= 'roll_median_5_')
    return(c(w1))
  }) %>% do.call(rbind, .) %>% data.frame
  # NaNs, NAs if accel is empty
  return(feat)
}) %>% do.call(rbind, .) %>% data.frame

# remove infinite values
train_accel_feat2[is.nan(train_accel_feat2)] = NA
test_accel_feat2[is.nan(test_accel_feat2)] = NA

# save
write.table(train_accel_feat2, 'data/working/features/train_accel_feat2.csv', col.names = T, row.names = F, quote=F, sep=',')
write.table(test_accel_feat2, 'data/working/features/test_accel_feat2.csv', col.names = T, row.names = F, quote=F, sep=',')

############# statistical features of MA-mean x, y, z axis in {1}-sec window #############

# train
train_accel_feat3 <- lapply(1:10, function(i){
  print(i)
  accel <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/acceleration.csv'))
  target <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/targets.csv'))
  feat <- lapply(target$end, function(e){
    #print(e)
    w1 = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.5), df=accel, cols=c('x','y','z'), FUN = roll_mean, n=5, colnames_prefix= 'roll_mean_5_')  
    return(c(w1))
  }) %>% do.call(rbind, .) %>% data.frame
}) %>% do.call(rbind, .) %>% data.frame

# test
sub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv') %>% data.table
t_ends = sub[,by='record_id', max(end)]$V1 # 5-29 sec

test_accel_feat3 <- lapply(11:882, function(i){
  print(i)
  accel <- read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/acceleration.csv'))
  feat <- lapply(1:t_ends[i-10], function(e){
    w1 = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.5), df=accel, cols=c('x','y','z'), FUN = roll_mean, n=5, colnames_prefix= 'roll_mean_5_')
    return(c(w1))
  }) %>% do.call(rbind, .) %>% data.frame
  # NaNs, NAs if accel is empty
  return(feat)
}) %>% do.call(rbind, .) %>% data.frame

# remove infinite values
train_accel_feat3[is.nan(train_accel_feat3)] = NA
test_accel_feat3[is.nan(test_accel_feat3)] = NA

# save
write.table(train_accel_feat3, 'data/working/features/train_accel_feat3.csv', col.names = T, row.names = F, quote=F, sep=',')
write.table(test_accel_feat3, 'data/working/features/test_accel_feat3.csv', col.names = T, row.names = F, quote=F, sep=',')

############# statistical features of raw x, y, z axis interactions in {1}-sec window #############

# train
train_accel_feat4 <- lapply(1:10, function(i){
  print(i)
  accel <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/acceleration.csv'))
  accel$xyzsum = accel$x + accel$y + accel$y
  accel$xyzabssum = abs(accel$x) + abs(accel$y) + abs(accel$z)
  accel$xyzvecmag = sqrt((accel$x)^2 + (accel$y)^2 + (accel$z)^2)
  accel$xyvecmag = sqrt((accel$x)^2 + (accel$y)^2)
  accel$yzvecmag = (accel$y)^2 + (accel$z)^2
  accel$xzvecmag = sqrt((accel$x)^2 + (accel$z)^2)
  
  target <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/targets.csv'))
  feat <- lapply(target$end, function(e){
    #print(e)
    w1 = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.25), df=accel, cols=c('xyzsum','xyzabssum','xyzvecmag','xyvecmag','yzvecmag','xzvecmag'),colnames_prefix= 'raw_')  
    return(c(w1))
  }) %>% do.call(rbind, .) %>% data.frame
}) %>% do.call(rbind, .) %>% data.frame

# test
sub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv') %>% data.table
t_ends = sub[,by='record_id', max(end)]$V1 # 5-29 sec

test_accel_feat4 <- lapply(11:882, function(i){
  print(i)
  accel <- read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/acceleration.csv'))
  accel$xyzsum = accel$x + accel$y + accel$y
  accel$xyzabssum = abs(accel$x) + abs(accel$y) + abs(accel$z)
  accel$xyzvecmag = sqrt((accel$x)^2 + (accel$y)^2 + (accel$z)^2)
  accel$xyvecmag = sqrt((accel$x)^2 + (accel$y)^2)
  accel$yzvecmag = (accel$y)^2 + (accel$z)^2
  accel$xzvecmag = sqrt((accel$x)^2 + (accel$z)^2)
  
  feat <- lapply(1:t_ends[i-10], function(e){
    w1 = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.25), df=accel, cols=c('xyzsum','xyzabssum','xyzvecmag','xyvecmag','yzvecmag','xzvecmag'), colnames_prefix= 'raw_')
    return(c(w1))
  }) %>% do.call(rbind, .) %>% data.frame
  # NaNs, NAs if accel is empty
  return(feat)
}) %>% do.call(rbind, .) %>% data.frame

# remove infinite values
train_accel_feat4[is.nan(train_accel_feat4)] = NA
test_accel_feat4[is.nan(test_accel_feat4)] = NA

# save
write.table(train_accel_feat4, 'data/working/features/train_accel_feat4.csv', col.names = T, row.names = F, quote=F, sep=',')
write.table(test_accel_feat4, 'data/working/features/test_accel_feat4.csv', col.names = T, row.names = F, quote=F, sep=',')

############# statistical features of Moving-median x, y, z axis interactions in {1}-sec window #############

# train
train_accel_feat5 <- lapply(1:10, function(i){
  print(i)
  accel <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/acceleration.csv'))
  accel$xyzsum = accel$x + accel$y + accel$y
  accel$xyzabssum = abs(accel$x) + abs(accel$y) + abs(accel$z)
  accel$xyzvecmag = sqrt((accel$x)^2 + (accel$y)^2 + (accel$z)^2)
  accel$xyvecmag = sqrt((accel$x)^2 + (accel$y)^2)
  accel$yzvecmag = (accel$y)^2 + (accel$z)^2
  accel$xzvecmag = sqrt((accel$x)^2 + (accel$z)^2)
  
  target <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/targets.csv'))
  feat <- lapply(target$end, function(e){
    #print(e)
    w1 = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.5), df=accel, cols=c('xyzsum','xyzabssum','xyzvecmag','xyvecmag','yzvecmag','xzvecmag'),FUN = roll_median, n=5, colnames_prefix= 'roll_median_5_')  
    return(c(w1))
  }) %>% do.call(rbind, .) %>% data.frame
}) %>% do.call(rbind, .) %>% data.frame

# test
sub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv') %>% data.table
t_ends = sub[,by='record_id', max(end)]$V1 # 5-29 sec

test_accel_feat5 <- lapply(11:882, function(i){
  print(i)
  accel <- read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/acceleration.csv'))
  accel$xyzsum = accel$x + accel$y + accel$y
  accel$xyzabssum = abs(accel$x) + abs(accel$y) + abs(accel$z)
  accel$xyzvecmag = sqrt((accel$x)^2 + (accel$y)^2 + (accel$z)^2)
  accel$xyvecmag = sqrt((accel$x)^2 + (accel$y)^2)
  accel$yzvecmag = (accel$y)^2 + (accel$z)^2
  accel$xzvecmag = sqrt((accel$x)^2 + (accel$z)^2)
  
  feat <- lapply(1:t_ends[i-10], function(e){
    w1 = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.5), df=accel, cols=c('xyzsum','xyzabssum','xyzvecmag','xyvecmag','yzvecmag','xzvecmag'), FUN = roll_median, n=5, colnames_prefix= 'roll_median_5_')
    return(c(w1))
  }) %>% do.call(rbind, .) %>% data.frame
  # NaNs, NAs if accel is empty
  return(feat)
}) %>% do.call(rbind, .) %>% data.frame

# remove infinite values
train_accel_feat5[is.nan(train_accel_feat5)] = NA
test_accel_feat5[is.nan(test_accel_feat5)] = NA

# save
write.table(train_accel_feat5, 'data/working/features/train_accel_feat5.csv', col.names = T, row.names = F, quote=F, sep=',')
write.table(test_accel_feat5, 'data/working/features/test_accel_feat5.csv', col.names = T, row.names = F, quote=F, sep=',')

############# statistical features of Moving-mean x, y, z axis interactions in {1}-sec window #############

# train
train_accel_feat6 <- lapply(1:10, function(i){
  print(i)
  accel <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/acceleration.csv'))
  accel$xyzsum = accel$x + accel$y + accel$y
  accel$xyzabssum = abs(accel$x) + abs(accel$y) + abs(accel$z)
  accel$xyzvecmag = sqrt((accel$x)^2 + (accel$y)^2 + (accel$z)^2)
  accel$xyvecmag = sqrt((accel$x)^2 + (accel$y)^2)
  accel$yzvecmag = (accel$y)^2 + (accel$z)^2
  accel$xzvecmag = sqrt((accel$x)^2 + (accel$z)^2)
  
  target <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/targets.csv'))
  feat <- lapply(target$end, function(e){
    #print(e)
    w1 = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.5), df=accel, cols=c('xyzsum','xyzabssum','xyzvecmag','xyvecmag','yzvecmag','xzvecmag'),FUN = roll_mean, n=5, colnames_prefix= 'roll_mean_5_')  
    return(c(w1))
  }) %>% do.call(rbind, .) %>% data.frame
}) %>% do.call(rbind, .) %>% data.frame

# test
sub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv') %>% data.table
t_ends = sub[,by='record_id', max(end)]$V1 # 5-29 sec

test_accel_feat6 <- lapply(11:882, function(i){
  print(i)
  accel <- read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/acceleration.csv'))
  accel$xyzsum = accel$x + accel$y + accel$y
  accel$xyzabssum = abs(accel$x) + abs(accel$y) + abs(accel$z)
  accel$xyzvecmag = sqrt((accel$x)^2 + (accel$y)^2 + (accel$z)^2)
  accel$xyvecmag = sqrt((accel$x)^2 + (accel$y)^2)
  accel$yzvecmag = (accel$y)^2 + (accel$z)^2
  accel$xzvecmag = sqrt((accel$x)^2 + (accel$z)^2)
  
  feat <- lapply(1:t_ends[i-10], function(e){
    w1 = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.5), df=accel, cols=c('xyzsum','xyzabssum','xyzvecmag','xyvecmag','yzvecmag','xzvecmag'), FUN = roll_mean, n=5, colnames_prefix= 'roll_mean_5_')
    return(c(w1))
  }) %>% do.call(rbind, .) %>% data.frame
  # NaNs, NAs if accel is empty
  return(feat)
}) %>% do.call(rbind, .) %>% data.frame

# remove infinite values
train_accel_feat6[is.nan(train_accel_feat6)] = NA
test_accel_feat6[is.nan(test_accel_feat6)] = NA

# save
write.table(train_accel_feat6, 'data/working/features/train_accel_feat6.csv', col.names = T, row.names = F, quote=F, sep=',')
write.table(test_accel_feat6, 'data/working/features/test_accel_feat6.csv', col.names = T, row.names = F, quote=F, sep=',')


