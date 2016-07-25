require(dplyr)

# window features function

window_features <- function(t_start, t_end, df, cols){
  
  window_data = df[which(df$t >= t_start & df$t < t_end), cols, drop=F]
  
  # quantiles
  res = unlist(lapply(window_data, quantile, names=F))
  
  # means
  res = c(res, unlist(lapply(window_data, mean, na.rm=T)))
  
  # sds
  res = c(res, unlist(lapply(window_data, sd, na.rm=T)))
  
  # names
  names(res) = c(unlist(lapply(1:length(cols), function(i){paste0(cols[i],'_W',t_end-t_start,'_',seq(0,100,25), 'Q')})),
                 paste0(cols, '_W', t_end-t_start, '_mean'),
                 paste0(cols, '_W', t_end-t_start, '_sd'))

  return(res)
}

feats = list()
targets = list()
for (i in 1:10){
  print(i)
  accel <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/acceleration.csv'))
  target <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/targets.csv'))
  
  feat = lapply(target$end, function(e){
    return(c(window_features(t_start = e - 1, t_end = e, df = accel, cols = c('x','y','z')),
             window_features(t_start = e - 2, t_end = e, df = accel, cols = c('x','y','z')),
             window_features(t_start = e - 3, t_end = e, df = accel, cols = c('x','y','z'))))
  }) %>% do.call(rbind, .) %>% as.data.frame
  feats[[i]] = feat[complete.cases(target),]
  targets[[i]] = target[complete.cases(target), -(1:2)]
}

feats = do.call(rbind, feats) %>% as.data.frame
targets = do.call(rbind, targets) %>% as.data.frame
write.table(feats, 'data/working/features/accel_feat.csv', col.names = T, row.names = F, quote=F, sep=',')
write.table(targets, 'data/working/features/targets.csv', col.names = T, row.names = F, quote=F, sep=',')






