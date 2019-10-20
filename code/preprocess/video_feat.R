require(dplyr)

############# window features function #############

window_features <- function(t_start, t_end, quantile_probs=seq(0,1,0.25), df, cols, colnames_prefix= NULL, FUN = NULL, thres, ...){
  
  window_data = df[which(df$t >= t_start & df$t < t_end), cols, drop=F]
  if(!is.null(FUN))
    if(nrow(window_data) >= thres)
      window_data = lapply(window_data, FUN=FUN, ...) %>% do.call(cbind, .) %>% data.frame
    
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


displacements <- function(t_start, t_end, df, prefix= NULL){
  
  window_data = df[which(df$t >= t_start & df$t < t_end),]
  n = nrow(window_data)
  
  if( n > 0){
    disp_center_2d = sqrt((window_data[1, "centre_2d_x"] - window_data[n, "centre_2d_x"])^2 + (window_data[1, "centre_2d_y"] - window_data[n, "centre_2d_y"])^2)
    direc_center_2d = (window_data[n, "centre_2d_y"] - window_data[1, "centre_2d_y"])/(window_data[n, "centre_2d_x"] - window_data[1, "centre_2d_x"])
    disp_br_2d = sqrt((window_data[1, "bb_2d_br_x"] - window_data[n, "bb_2d_br_x"])^2 + (window_data[1, "bb_2d_br_y"] - window_data[n, "bb_2d_br_y"])^2)
    direc_br_2d = (window_data[n, "bb_2d_br_y"] - window_data[1, "bb_2d_br_y"])/(window_data[n, "bb_2d_br_x"] - window_data[1, "bb_2d_br_x"])
    disp_tl_2d = sqrt((window_data[1, "bb_2d_tl_x"] - window_data[n, "bb_2d_tl_x"])^2 + (window_data[1, "bb_2d_tl_y"] - window_data[n, "bb_2d_tl_y"])^2)
    direc_tl_2d = (window_data[n, "bb_2d_tl_y"] - window_data[1, "bb_2d_tl_y"])/(window_data[n, "bb_2d_tl_x"] - window_data[1, "bb_2d_tl_x"])
    disp_boxcenter_2d = sqrt((window_data[1, "boxcentre_2d_x"] - window_data[n, "boxcentre_2d_x"])^2 + (window_data[1, "boxcentre_2d_y"] - window_data[n, "boxcentre_2d_y"])^2)
    
    disp_center_3d = sqrt((window_data[1, "centre_3d_x"] - window_data[n, "centre_3d_x"])^2 + (window_data[1, "centre_3d_y"] - window_data[n, "centre_3d_y"])^2)
    direc_center_3d = (window_data[n, "centre_3d_y"] - window_data[1, "centre_3d_y"])/(window_data[n, "centre_3d_x"] - window_data[1, "centre_3d_x"])
    disp_brb_3d = sqrt((window_data[1, "bb_3d_brb_x"] - window_data[n, "bb_3d_brb_x"])^2 + (window_data[1, "bb_3d_brb_y"] - window_data[n, "bb_3d_brb_y"])^2)
    direc_brb_3d = (window_data[n, "bb_3d_brb_y"] - window_data[1, "bb_3d_brb_y"])/(window_data[n, "bb_3d_brb_x"] - window_data[1, "bb_3d_brb_x"])
    disp_flt_3d = sqrt((window_data[1, "bb_3d_flt_x"] - window_data[n, "bb_3d_flt_x"])^2 + (window_data[1, "bb_3d_flt_y"] - window_data[n, "bb_3d_flt_y"])^2)
    direc_flt_3d = (window_data[n, "bb_3d_flt_y"] - window_data[1, "bb_3d_flt_y"])/(window_data[n, "bb_3d_flt_x"] - window_data[1, "bb_3d_flt_x"])
    disp_boxcenter_3d = sqrt((window_data[1, "boxcentre_3d_x"] - window_data[n, "boxcentre_3d_x"])^2 + (window_data[1, "boxcentre_3d_y"] - window_data[n, "boxcentre_3d_y"])^2)
    
  } else{
    disp_center_2d = NA
    direc_center_2d = NA
    disp_br_2d = NA
    direc_br_2d = NA
    disp_tl_2d = NA
    direc_tl_2d = NA
    disp_boxcenter_2d = NA
    
    disp_center_3d = NA
    direc_center_3d = NA
    disp_brb_3d = NA
    direc_brb_3d = NA
    disp_flt_3d = NA
    direc_flt_3d = NA
    disp_boxcenter_3d = NA
  }
  res = c(direc_center_2d = direc_center_2d,disp_br_2d = disp_br_2d,direc_br_2d = direc_br_2d,disp_tl_2d = disp_tl_2d,
          direc_tl_2d = direc_tl_2d,disp_boxcenter_2d = disp_boxcenter_2d,disp_center_3d = disp_center_3d,
          direc_center_3d = direc_center_3d,disp_brb_3d = disp_brb_3d,direc_brb_3d = direc_brb_3d,disp_flt_3d = disp_flt_3d,
          direc_flt_3d = direc_flt_3d,disp_boxcenter_3d = disp_boxcenter_3d)
  
  names(res) = paste0(prefix, names(res), '_W',t_end-t_start)
  return(res)
}

############# statistical features of raw attributes in {1}-sec window #############

# train
train_video_feat1 <- lapply(1:10, function(i){
  
  print(i)
  
  hallway <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/video_hallway.csv'))
  kitchen <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/video_kitchen.csv'))
  living <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/video_living_room.csv'))
  
  hallway$boxcentre_2d_x = (hallway$bb_2d_br_x + hallway$bb_2d_tl_x)/2
  hallway$boxcentre_2d_y = (hallway$bb_2d_br_y + hallway$bb_2d_tl_y)/2
  hallway$boxcentre_3d_x = (hallway$bb_3d_brb_x + hallway$bb_3d_flt_x)/2
  hallway$boxcentre_3d_y = (hallway$bb_3d_brb_y + hallway$bb_3d_flt_y)/2
  hallway$boxcentre_3d_z = (hallway$bb_3d_brb_z + hallway$bb_3d_flt_z)/2
  
  kitchen$boxcentre_2d_x = (kitchen$bb_2d_br_x + kitchen$bb_2d_tl_x)/2
  kitchen$boxcentre_2d_y = (kitchen$bb_2d_br_y + kitchen$bb_2d_tl_y)/2
  kitchen$boxcentre_3d_x = (kitchen$bb_3d_brb_x + kitchen$bb_3d_flt_x)/2
  kitchen$boxcentre_3d_y = (kitchen$bb_3d_brb_y + kitchen$bb_3d_flt_y)/2
  kitchen$boxcentre_3d_z = (kitchen$bb_3d_brb_z + kitchen$bb_3d_flt_z)/2
  
  living$boxcentre_2d_x = (living$bb_2d_br_x + living$bb_2d_tl_x)/2
  living$boxcentre_2d_y = (living$bb_2d_br_y + living$bb_2d_tl_y)/2
  living$boxcentre_3d_x = (living$bb_3d_brb_x + living$bb_3d_flt_x)/2
  living$boxcentre_3d_y = (living$bb_3d_brb_y + living$bb_3d_flt_y)/2
  living$boxcentre_3d_z = (living$bb_3d_brb_z + living$bb_3d_flt_z)/2
  
  target <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/targets.csv'))
  
  feat <- lapply(target$end, function(e){
    w1_hallway = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.25), df=hallway, cols=names(hallway)[-1], colnames_prefix= 'raw_hallway_')
    w1_kitchen = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.25), df=kitchen, cols=names(kitchen)[-1], colnames_prefix= 'raw_kitchen_')
    w1_living = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.25), df=living, cols=names(living)[-1], colnames_prefix= 'raw_living_')
    return(c(w1_hallway, w1_kitchen, w1_living))
  }) %>% do.call(rbind, .) %>% data.frame
  
  return(feat)
  
}) %>% do.call(rbind, .) %>% data.frame

# test
sub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv') %>% data.table
t_ends = sub[,by='record_id', max(end)]$V1 # 5-29 sec

test_video_feat1 <- lapply(11:882, function(i){
  print(i)
  
  hallway <- read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/video_hallway.csv'))
  kitchen <- read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/video_kitchen.csv'))
  living <- read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/video_living_room.csv'))
  
  hallway$boxcentre_2d_x = (hallway$bb_2d_br_x + hallway$bb_2d_tl_x)/2
  hallway$boxcentre_2d_y = (hallway$bb_2d_br_y + hallway$bb_2d_tl_y)/2
  hallway$boxcentre_3d_x = (hallway$bb_3d_brb_x + hallway$bb_3d_flt_x)/2
  hallway$boxcentre_3d_y = (hallway$bb_3d_brb_y + hallway$bb_3d_flt_y)/2
  hallway$boxcentre_3d_z = (hallway$bb_3d_brb_z + hallway$bb_3d_flt_z)/2
  
  kitchen$boxcentre_2d_x = (kitchen$bb_2d_br_x + kitchen$bb_2d_tl_x)/2
  kitchen$boxcentre_2d_y = (kitchen$bb_2d_br_y + kitchen$bb_2d_tl_y)/2
  kitchen$boxcentre_3d_x = (kitchen$bb_3d_brb_x + kitchen$bb_3d_flt_x)/2
  kitchen$boxcentre_3d_y = (kitchen$bb_3d_brb_y + kitchen$bb_3d_flt_y)/2
  kitchen$boxcentre_3d_z = (kitchen$bb_3d_brb_z + kitchen$bb_3d_flt_z)/2
  
  living$boxcentre_2d_x = (living$bb_2d_br_x + living$bb_2d_tl_x)/2
  living$boxcentre_2d_y = (living$bb_2d_br_y + living$bb_2d_tl_y)/2
  living$boxcentre_3d_x = (living$bb_3d_brb_x + living$bb_3d_flt_x)/2
  living$boxcentre_3d_y = (living$bb_3d_brb_y + living$bb_3d_flt_y)/2
  living$boxcentre_3d_z = (living$bb_3d_brb_z + living$bb_3d_flt_z)/2
  
  feat <- lapply(1:t_ends[i-10], function(e){
    w1_hallway = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.25), df=hallway, cols=names(hallway)[-1], colnames_prefix= 'raw_hallway_')
    w1_kitchen = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.25), df=kitchen, cols=names(kitchen)[-1], colnames_prefix= 'raw_kitchen_')
    w1_living = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.25), df=living, cols=names(living)[-1], colnames_prefix= 'raw_living_')
    return(c(w1_hallway, w1_kitchen, w1_living))
  }) %>% do.call(rbind, .) %>% data.frame
  
  return(feat)
}) %>% do.call(rbind, .) %>% data.frame

# remove infinite values
train_video_feat1[is.nan(train_video_feat1)] = NA
test_video_feat1[is.nan(test_video_feat1)] = NA

# binary features
train_video_feat1$issignal_hallway = train_video_feat1[,"raw_hallway_centre_2d_x_W1_0Q"]; train_video_feat1$issignal_hallway[!is.na(train_video_feat1$issignal_hallway)]=1; train_video_feat1$issignal_hallway[is.na(train_video_feat1$issignal_hallway)]=0
train_video_feat1$issignal_kitchen = train_video_feat1[,"raw_kitchen_centre_2d_x_W1_0Q"]; train_video_feat1$issignal_kitchen[!is.na(train_video_feat1$issignal_kitchen)]=1; train_video_feat1$issignal_kitchen[is.na(train_video_feat1$issignal_kitchen)]=0
train_video_feat1$issignal_living = train_video_feat1[,"raw_living_centre_2d_x_W1_0Q"]; train_video_feat1$issignal_living[!is.na(train_video_feat1$issignal_living)]=1; train_video_feat1$issignal_living[is.na(train_video_feat1$issignal_living)]=0

test_video_feat1$issignal_hallway = test_video_feat1[,"raw_hallway_centre_2d_x_W1_0Q"]; test_video_feat1$issignal_hallway[!is.na(test_video_feat1$issignal_hallway)]=1; test_video_feat1$issignal_hallway[is.na(test_video_feat1$issignal_hallway)]=0
test_video_feat1$issignal_kitchen = test_video_feat1[,"raw_kitchen_centre_2d_x_W1_0Q"]; test_video_feat1$issignal_kitchen[!is.na(test_video_feat1$issignal_kitchen)]=1; test_video_feat1$issignal_kitchen[is.na(test_video_feat1$issignal_kitchen)]=0
test_video_feat1$issignal_living = test_video_feat1[,"raw_living_centre_2d_x_W1_0Q"]; test_video_feat1$issignal_living[!is.na(test_video_feat1$issignal_living)]=1; test_video_feat1$issignal_living[is.na(test_video_feat1$issignal_living)]=0

# save
write.table(train_video_feat1, 'data/working/features/train_video_feat1.csv', col.names = T, row.names = F, quote=F, sep=',')
write.table(test_video_feat1, 'data/working/features/test_video_feat1.csv', col.names = T, row.names = F, quote=F, sep=',')

############# statistical features of diff of attributes in {1}-sec window #############

# train
train_video_feat2 <- lapply(1:10, function(i){
  
  print(i)
  
  hallway <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/video_hallway.csv'))
  kitchen <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/video_kitchen.csv'))
  living <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/video_living_room.csv'))
  
  hallway$boxcentre_2d_x = (hallway$bb_2d_br_x + hallway$bb_2d_tl_x)/2
  hallway$boxcentre_2d_y = (hallway$bb_2d_br_y + hallway$bb_2d_tl_y)/2
  hallway$boxcentre_3d_x = (hallway$bb_3d_brb_x + hallway$bb_3d_flt_x)/2
  hallway$boxcentre_3d_y = (hallway$bb_3d_brb_y + hallway$bb_3d_flt_y)/2
  hallway$boxcentre_3d_z = (hallway$bb_3d_brb_z + hallway$bb_3d_flt_z)/2
  
  kitchen$boxcentre_2d_x = (kitchen$bb_2d_br_x + kitchen$bb_2d_tl_x)/2
  kitchen$boxcentre_2d_y = (kitchen$bb_2d_br_y + kitchen$bb_2d_tl_y)/2
  kitchen$boxcentre_3d_x = (kitchen$bb_3d_brb_x + kitchen$bb_3d_flt_x)/2
  kitchen$boxcentre_3d_y = (kitchen$bb_3d_brb_y + kitchen$bb_3d_flt_y)/2
  kitchen$boxcentre_3d_z = (kitchen$bb_3d_brb_z + kitchen$bb_3d_flt_z)/2
  
  living$boxcentre_2d_x = (living$bb_2d_br_x + living$bb_2d_tl_x)/2
  living$boxcentre_2d_y = (living$bb_2d_br_y + living$bb_2d_tl_y)/2
  living$boxcentre_3d_x = (living$bb_3d_brb_x + living$bb_3d_flt_x)/2
  living$boxcentre_3d_y = (living$bb_3d_brb_y + living$bb_3d_flt_y)/2
  living$boxcentre_3d_z = (living$bb_3d_brb_z + living$bb_3d_flt_z)/2
  
  target <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/targets.csv'))
  
  feat <- lapply(target$end, function(e){
    w1_hallway = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.5), df=hallway, cols=names(hallway)[-1], colnames_prefix= 'diff_hallway_', FUN=diff, thres=0)
    w1_kitchen = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.5), df=kitchen, cols=names(kitchen)[-1], colnames_prefix= 'diff_kitchen_', FUN=diff, thres=0)
    w1_living = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.5), df=living, cols=names(living)[-1], colnames_prefix= 'diff_living_', FUN=diff, thres=0)
    return(c(w1_hallway, w1_kitchen, w1_living))
  }) %>% do.call(rbind, .) %>% data.frame
  
  return(feat)
  
}) %>% do.call(rbind, .) %>% data.frame

# test
sub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv') %>% data.table
t_ends = sub[,by='record_id', max(end)]$V1 # 5-29 sec

test_video_feat2 <- lapply(11:882, function(i){
  print(i)
  
  hallway <- read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/video_hallway.csv'))
  kitchen <- read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/video_kitchen.csv'))
  living <- read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/video_living_room.csv'))
  
  hallway$boxcentre_2d_x = (hallway$bb_2d_br_x + hallway$bb_2d_tl_x)/2
  hallway$boxcentre_2d_y = (hallway$bb_2d_br_y + hallway$bb_2d_tl_y)/2
  hallway$boxcentre_3d_x = (hallway$bb_3d_brb_x + hallway$bb_3d_flt_x)/2
  hallway$boxcentre_3d_y = (hallway$bb_3d_brb_y + hallway$bb_3d_flt_y)/2
  hallway$boxcentre_3d_z = (hallway$bb_3d_brb_z + hallway$bb_3d_flt_z)/2
  
  kitchen$boxcentre_2d_x = (kitchen$bb_2d_br_x + kitchen$bb_2d_tl_x)/2
  kitchen$boxcentre_2d_y = (kitchen$bb_2d_br_y + kitchen$bb_2d_tl_y)/2
  kitchen$boxcentre_3d_x = (kitchen$bb_3d_brb_x + kitchen$bb_3d_flt_x)/2
  kitchen$boxcentre_3d_y = (kitchen$bb_3d_brb_y + kitchen$bb_3d_flt_y)/2
  kitchen$boxcentre_3d_z = (kitchen$bb_3d_brb_z + kitchen$bb_3d_flt_z)/2
  
  living$boxcentre_2d_x = (living$bb_2d_br_x + living$bb_2d_tl_x)/2
  living$boxcentre_2d_y = (living$bb_2d_br_y + living$bb_2d_tl_y)/2
  living$boxcentre_3d_x = (living$bb_3d_brb_x + living$bb_3d_flt_x)/2
  living$boxcentre_3d_y = (living$bb_3d_brb_y + living$bb_3d_flt_y)/2
  living$boxcentre_3d_z = (living$bb_3d_brb_z + living$bb_3d_flt_z)/2
  
  feat <- lapply(1:t_ends[i-10], function(e){
    w1_hallway = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.5), df=hallway, cols=names(hallway)[-1], colnames_prefix= 'diff_hallway_', FUN=diff, thres=0)
    w1_kitchen = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.5), df=kitchen, cols=names(kitchen)[-1], colnames_prefix= 'diff_kitchen_', FUN=diff, thres=0)
    w1_living = window_features(t_start=e-1, t_end=e, quantile_probs=seq(0,1,0.5), df=living, cols=names(living)[-1], colnames_prefix= 'diff_living_', FUN=diff, thres=0)
    return(c(w1_hallway, w1_kitchen, w1_living))
  }) %>% do.call(rbind, .) %>% data.frame
  
  return(feat)
}) %>% do.call(rbind, .) %>% data.frame

# remove infinite values
train_video_feat2[is.nan(train_video_feat2)] = NA
test_video_feat2[is.nan(test_video_feat2)] = NA


# save
write.table(train_video_feat2, 'data/working/features/train_video_feat2.csv', col.names = T, row.names = F, quote=F, sep=',')
write.table(test_video_feat2, 'data/working/features/test_video_feat2.csv', col.names = T, row.names = F, quote=F, sep=',')

############# displacements & directions of points in {1}-sec window #############

# train
train_video_feat3 <- lapply(1:10, function(i){
  
  print(i)
  
  hallway <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/video_hallway.csv'))
  kitchen <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/video_kitchen.csv'))
  living <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/video_living_room.csv'))
  
  hallway$boxcentre_2d_x = (hallway$bb_2d_br_x + hallway$bb_2d_tl_x)/2
  hallway$boxcentre_2d_y = (hallway$bb_2d_br_y + hallway$bb_2d_tl_y)/2
  hallway$boxcentre_3d_x = (hallway$bb_3d_brb_x + hallway$bb_3d_flt_x)/2
  hallway$boxcentre_3d_y = (hallway$bb_3d_brb_y + hallway$bb_3d_flt_y)/2
  hallway$boxcentre_3d_z = (hallway$bb_3d_brb_z + hallway$bb_3d_flt_z)/2
  
  kitchen$boxcentre_2d_x = (kitchen$bb_2d_br_x + kitchen$bb_2d_tl_x)/2
  kitchen$boxcentre_2d_y = (kitchen$bb_2d_br_y + kitchen$bb_2d_tl_y)/2
  kitchen$boxcentre_3d_x = (kitchen$bb_3d_brb_x + kitchen$bb_3d_flt_x)/2
  kitchen$boxcentre_3d_y = (kitchen$bb_3d_brb_y + kitchen$bb_3d_flt_y)/2
  kitchen$boxcentre_3d_z = (kitchen$bb_3d_brb_z + kitchen$bb_3d_flt_z)/2
  
  living$boxcentre_2d_x = (living$bb_2d_br_x + living$bb_2d_tl_x)/2
  living$boxcentre_2d_y = (living$bb_2d_br_y + living$bb_2d_tl_y)/2
  living$boxcentre_3d_x = (living$bb_3d_brb_x + living$bb_3d_flt_x)/2
  living$boxcentre_3d_y = (living$bb_3d_brb_y + living$bb_3d_flt_y)/2
  living$boxcentre_3d_z = (living$bb_3d_brb_z + living$bb_3d_flt_z)/2
  
  target <- read.csv(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/targets.csv'))
  
  feat <- lapply(target$end, function(e){
    
    d1_hallway = displacements(t_start = e-1, t_end = e, df = hallway, prefix = 'hallway_')
    d1_kitchen = displacements(t_start = e-1, t_end = e, df = hallway, prefix = 'kitchen_')
    d1_living = displacements(t_start = e-1, t_end = e, df = hallway, prefix = 'living_')
    return(c(d1_hallway, d1_kitchen, d1_living))
    
  }) %>% do.call(rbind, .) %>% data.frame
  
  return(feat)
  
}) %>% do.call(rbind, .) %>% data.frame

# test
sub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv') %>% data.table
t_ends = sub[,by='record_id', max(end)]$V1 # 5-29 sec

test_video_feat3 <- lapply(11:882, function(i){
  print(i)
  
  hallway <- read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/video_hallway.csv'))
  kitchen <- read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/video_kitchen.csv'))
  living <- read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/video_living_room.csv'))
  
  hallway$boxcentre_2d_x = (hallway$bb_2d_br_x + hallway$bb_2d_tl_x)/2
  hallway$boxcentre_2d_y = (hallway$bb_2d_br_y + hallway$bb_2d_tl_y)/2
  hallway$boxcentre_3d_x = (hallway$bb_3d_brb_x + hallway$bb_3d_flt_x)/2
  hallway$boxcentre_3d_y = (hallway$bb_3d_brb_y + hallway$bb_3d_flt_y)/2
  hallway$boxcentre_3d_z = (hallway$bb_3d_brb_z + hallway$bb_3d_flt_z)/2
  
  kitchen$boxcentre_2d_x = (kitchen$bb_2d_br_x + kitchen$bb_2d_tl_x)/2
  kitchen$boxcentre_2d_y = (kitchen$bb_2d_br_y + kitchen$bb_2d_tl_y)/2
  kitchen$boxcentre_3d_x = (kitchen$bb_3d_brb_x + kitchen$bb_3d_flt_x)/2
  kitchen$boxcentre_3d_y = (kitchen$bb_3d_brb_y + kitchen$bb_3d_flt_y)/2
  kitchen$boxcentre_3d_z = (kitchen$bb_3d_brb_z + kitchen$bb_3d_flt_z)/2
  
  living$boxcentre_2d_x = (living$bb_2d_br_x + living$bb_2d_tl_x)/2
  living$boxcentre_2d_y = (living$bb_2d_br_y + living$bb_2d_tl_y)/2
  living$boxcentre_3d_x = (living$bb_3d_brb_x + living$bb_3d_flt_x)/2
  living$boxcentre_3d_y = (living$bb_3d_brb_y + living$bb_3d_flt_y)/2
  living$boxcentre_3d_z = (living$bb_3d_brb_z + living$bb_3d_flt_z)/2
  
  feat <- lapply(1:t_ends[i-10], function(e){
    
    d1_hallway = displacements(t_start = e-1, t_end = e, df = hallway, prefix = 'hallway_')
    d1_kitchen = displacements(t_start = e-1, t_end = e, df = hallway, prefix = 'kitchen_')
    d1_living = displacements(t_start = e-1, t_end = e, df = hallway, prefix = 'living_')
    return(c(d1_hallway, d1_kitchen, d1_living))
    
  }) %>% do.call(rbind, .) %>% data.frame
  
  return(feat)
}) %>% do.call(rbind, .) %>% data.frame

# remove infinite values
train_video_feat3[is.nan(train_video_feat3)] = NA
train_video_feat3 = do.call(data.frame, lapply(train_video_feat3, function(x) replace(x, x == Inf, 99999)))
train_video_feat3 = do.call(data.frame, lapply(train_video_feat3, function(x) replace(x, x == -Inf, -99999)))

test_video_feat3[is.nan(test_video_feat3)] = NA
test_video_feat3 = do.call(data.frame, lapply(test_video_feat3, function(x) replace(x, x == Inf, 99999)))
test_video_feat3 = do.call(data.frame, lapply(test_video_feat3, function(x) replace(x, x == -Inf, -99999)))



# save
write.table(train_video_feat3, 'data/working/features/train_video_feat3.csv', col.names = T, row.names = F, quote=F, sep=',')
write.table(test_video_feat3, 'data/working/features/test_video_feat3.csv', col.names = T, row.names = F, quote=F, sep=',')



