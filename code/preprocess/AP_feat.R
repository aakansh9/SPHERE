require(dplyr)

# window features function

window_features <- function(t_start, t_end, df, cols){
  
  window_data = df[which(df$t >= t_start & df$t < t_end), cols, drop=F]
  
  # quantile's
  res = unlist(lapply(window_data, quantile, names=F, na.rm=T))
  
  # mean's
  res = c(res, unlist(lapply(window_data, mean, na.rm=T)))
  
  # sd's
  res = c(res, unlist(lapply(window_data, sd, na.rm=T)))
  
  # names
  names(res) = c(unlist(lapply(1:length(cols), function(i){paste0(cols[i],'_W',t_end-t_start,'_',seq(0,100,25), 'Q')})),
                 paste0(cols, '_W', t_end-t_start, '_mean'),
                 paste0(cols, '_W', t_end-t_start, '_sd'))
  
  return(res)
}

# ... AP window features ...

feats = list()
for (i in 1:10){
  print(i)
  target <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/targets.csv'))
  accel <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/acceleration.csv'))
  accel[is.na(accel)] = 0
  feat = lapply(target$end, function(e){
    return(c(window_features(t_start = e - 1, t_end = e, df = accel, cols = c('Kitchen_AP','Lounge_AP','Upstairs_AP', 'Study_AP'))))
  }) %>% do.call(rbind, .) %>% as.data.frame
  feats[[i]] = feat[complete.cases(target),]
}
feats = do.call(rbind, feats) %>% as.data.frame
write.table(feats, 'data/working/features/AP_feats.csv', col.names = T, row.names = F, quote=F, sep=',')






