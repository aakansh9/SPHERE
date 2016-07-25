require(dplyr)

# window features function

window_features <- function(t_start, t_end, df, cols){
  
  window_data = df[which(df$t >= t_start & df$t < t_end), cols, drop=F]
  
  # quantile's
  res = unlist(lapply(window_data, quantile, probs=seq(0,1,0.5), names=F, na.rm=T))
  
  # mean's
  res = c(res, unlist(lapply(window_data, mean, na.rm=T)))
  
  # sd's
  res = c(res, unlist(lapply(window_data, sd, na.rm=T)))
  
  # names
  names(res) = c(unlist(lapply(1:length(cols), function(i){paste0(cols[i],'_W',t_end-t_start,'_',seq(0,100,50), 'Q')})),
                 paste0(cols, '_W', t_end-t_start, '_mean'),
                 paste0(cols, '_W', t_end-t_start, '_sd'))
  
  return(res)
}

# ... video window-features ...

feats = list()
for (i in 1:10){
  print(i)
  target <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/targets.csv'))
  hallway <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/video_hallway.csv'))
  kitchen <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/video_kitchen.csv'))
  living <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/video_living_room.csv'))
  
  names(hallway)[-1] = paste0('hallway_', names(hallway)[-1])
  names(kitchen)[-1] = paste0('kitchen_', names(kitchen)[-1])
  names(living)[-1] = paste0('living_', names(living)[-1])
  
  feat = lapply(target$end, function(e){
    
    return(c(window_features(t_start = e - 1, t_end = e, df = hallway, cols = c('hallway_centre_2d_x','hallway_centre_2d_y')),  
           window_features(t_start = e - 1, t_end = e, df = kitchen, cols = c('kitchen_centre_2d_x','kitchen_centre_2d_y')),
           window_features(t_start = e - 1, t_end = e, df = living, cols = c('living_centre_2d_x','living_centre_2d_y'))))
    
  }) %>% do.call(rbind, .) %>% as.data.frame
  feats[[i]] = feat[complete.cases(target),]
}
feats = do.call(rbind, feats) %>% as.data.frame
feats[is.na(feats)]=0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

feats[is.nan(feats)] <- 0
write.table(feats, 'data/working/features/video_feat.csv', col.names = T, row.names = F, quote=F, sep=',')






