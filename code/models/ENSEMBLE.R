cweights = c(1.35298455691,1.38684574053,1.59587388404,1.35318713948,0.347783666015,
             0.661081706198,1.04723628621,0.398865222651,0.207586320237,1.50578335208,
             0.110181365961,1.07803284435,1.36560417316,1.17024113802,1.1933637414,
             1.1803704493,1.34414875433,1.11683830693,1.08083910312,0.503152249073)
BS <- function(pred, truth){
  apply((pred-truth)^2, MARGIN = 1, function(x) sum(x*cweights)) %>% mean
}

traintarget = read.csv('data/working/features/train.target')[,-(1:4)]
traintarget = traintarget[complete.cases(traintarget),]
bestweight = function(a,b){
  res = lapply(seq(0,1,0.1), function(w) BS((w*a+(1-w)*b), traintarget)) %>% unlist
  #(which.min(res)-1)/10
  print(min(res))
  w = (which.min(res)-1)/10
  return(list('df' = w*a + (1-w)*b, 'weights' = c(w,1-w)))
}

ens = function(l){
  if(length(l) == 1)
    return(list('df'=l[[1]], 'weights'=c(1)))
  else{
    prev = ens(l[-length(l)])
    prev_df = prev$df
    prev_weights = prev$weights
    new = bestweight(prev_df, l[[length(l)]])
    new_df = new$df
    new_weights = c(new$weights[1]*prev_weights, new$weights[2])
    return(list('df'=new_df, 'weights' = new_weights))
  }
}


fnames = list.files(path = 'data/working/LAGGER_MODELS/', pattern = '[a-zA-Z0-9_/]+TR.csv$', recursive = T) %>% paste0('data/working/LAGGER_MODELS/',.)
cv_scores= lapply(fnames, function(f){
  fread(f) %>% data.frame %>% BS(., traintarget)
}) %>% unlist; names(cv_scores) = unlist(lapply(strsplit(fnames, 'S/'), '[',2))
tmp = data.frame(cv_scores)

############################################################################
# best single L2 model

MOFO3_330_XGBTREE_TT = read.csv('data/working/LAGGER_MODELS/MOFO3_330_XGBTREE/pred_TT.csv')
sum(complete.cases(MOFO3_330_XGBTREE_TT)) - 16600 # 3488 are NA
a = which(is.na(MOFO3_330_XGBTREE_TT[,1]))

GBLINEAR1_GBTREE3_RF1_LAGGER1_XGBTREE_TT = read.csv('data/working/LAGGER_MODELS/GBLINEAR1_GBTREE3_RF1_LAGGER1_XGBTREE/pred_TT.csv')
sum(complete.cases(GBLINEAR1_GBTREE3_RF1_LAGGER1_XGBTREE_TT)) - 16600 # 1744
b = which(is.na(GBLINEAR1_GBTREE3_RF1_LAGGER1_XGBTREE_TT[,1]))

L1_BEST_ENSEMBLE_TT = fread('data/working/models/GBTREE3_TT.csv')*0.504 + fread('data/working/models/RF1_TT.csv')*0.216 + fread('data/working/models/GBTREE4_TT.csv')*0.080 + fread('data/working/models/GBLINEAR1_TT.csv')*0.200
sum(complete.cases(L1_BEST_ENSEMBLE_TT)) - 16600 # 0

sub = MOFO3_330_XGBTREE_TT
sub[b,] = L1_BEST_ENSEMBLE_TT[b,]
sub[setdiff(a,b), ] = GBLINEAR1_GBTREE3_RF1_LAGGER1_XGBTREE_TT[setdiff(a,b), ]

samplesub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv')
samplesub[,4:23] = sub
write.table(samplesub, 'data/working/SUBS/BEST_L2_SINGLE_MODEL_TT.csv', quote=F, sep=',', row.names = F, col.names = T)

############################################################################

# best L2 ensemble
fnames = list.files(path = 'data/working/LAGGER_MODELS/', pattern = '[a-zA-Z0-9_/]+TR.csv$', recursive = T) %>% paste0('data/working/LAGGER_MODELS/',.)
TR = lapply(fnames, function(x) fread(x) %>% data.frame)
res = ens(TR) # 0.1665513
ans = sort.int(res$weights, index.return=T, decreasing = T)

fnames = list.files(path = 'data/working/LAGGER_MODELS/', pattern = '[a-zA-Z0-9_/]+TT.csv$', recursive = T) %>% paste0('data/working/LAGGER_MODELS/',.)
fnames = fnames[ans$ix[1:19]]
BEST_L2_ENSEMBLE = lapply(1:length(fnames), function(i){
  d = fread(fnames[i])*ans$x[1:19][i]
  d = as.data.frame(d)
}) %>% Reduce('+', .)
sum(complete.cases(BEST_L2_ENSEMBLE)) - 16600 # 1744
a = which(is.na(BEST_L2_ENSEMBLE[,1]))


RF1_GBTREE3_LAGGER1_XGBTREE_TT = read.csv('data/working/LAGGER_MODELS/RF1_GBTREE3_LAGGER1_XGBTREE/pred_TT.csv')
sum(complete.cases(RF1_GBTREE3_LAGGER1_XGBTREE_TT)) - 16600 # 1744
b = which(is.na(RF1_GBTREE3_LAGGER1_XGBTREE_TT[,1]))

L1_SECOND_BEST_ENSEMBLE_TT = fread('data/working/models/GBTREE3_TT.csv')*0.3528 + fread('data/working/models/RF1_TT.csv')*0.2400 + fread('data/working/models/GBTREE4_TT.csv')*0.0560 + fread('data/working/models/GBLINEAR1_TT.csv')*0.2000 + fread('data/working/models/GBTREE1_TT.csv')*0.1512 
sum(complete.cases(L1_SECOND_BEST_ENSEMBLE_TT)) - 16600 # 0

sub = BEST_L2_ENSEMBLE
sub[b,] = L1_SECOND_BEST_ENSEMBLE_TT[b,]
sub[setdiff(a,b), ] = RF1_GBTREE3_LAGGER1_XGBTREE_TT[setdiff(a,b), ]

samplesub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv')
samplesub[,4:23] = sub
write.table(samplesub, 'data/working/SUBS/BEST_L2_ENSEMBLE_TT.csv', quote=F, sep=',', row.names = F, col.names = T)

############################################################################

# best L2 ensemble ultimate
fnames = list.files(path = 'data/working/LAGGER_MODELS/', pattern = '[a-zA-Z0-9_/]+TR.csv$', recursive = T) %>% paste0('data/working/LAGGER_MODELS/',.)
TR = lapply(fnames, function(x) fread(x) %>% data.frame)

#set.seed(2); res = ens(TR[sample(1:59, 59)])
#res = ens(TR[c(47,48,50,46,49,31,13,17,27,39)]) # 0.1664596
#res = ens(TR[c(47,48,50,46,49,31,13,27)]) # 0.1664596
#res = ens(TR[c(47,50,46,49,31,13,27)]) # 0.1664553
res = ens(TR[c(47,50,49,31,13,27)]) # 0.1664189
#res = ens(TR[c(50,49,31,13,27)]) # 0.1666483

fnames = list.files(path = 'data/working/LAGGER_MODELS/', pattern = '[a-zA-Z0-9_/]+TT.csv$', recursive = T) %>% paste0('data/working/LAGGER_MODELS/',.)
fnames = fnames[c(47,50,49,31,13,27)]

BEST_L2_ENSEMBLE_ULTIMATE = lapply(1:length(fnames), function(i){
  d = fread(fnames[i])*res$weights[i]
  d = as.data.frame(d)
}) %>% Reduce('+', .)
sum(complete.cases(BEST_L2_ENSEMBLE_ULTIMATE)) - 16600 # 1744
a = which(is.na(BEST_L2_ENSEMBLE_ULTIMATE[,1]))

GBLINEAR1_GBTREE3_RF1_LAGGER1_XGBTREE_TT = read.csv('data/working/LAGGER_MODELS/GBLINEAR1_GBTREE3_RF1_LAGGER1_XGBTREE/pred_TT.csv')
sum(complete.cases(GBLINEAR1_GBTREE3_RF1_LAGGER1_XGBTREE_TT)) - 16600 # 1744
b = which(is.na(GBLINEAR1_GBTREE3_RF1_LAGGER1_XGBTREE_TT[,1]))

L1_BEST_ENSEMBLE_TT = fread('data/working/models/GBTREE3_TT.csv')*0.504 + fread('data/working/models/RF1_TT.csv')*0.216 + fread('data/working/models/GBTREE4_TT.csv')*0.080 + fread('data/working/models/GBLINEAR1_TT.csv')*0.200
sum(complete.cases(L1_BEST_ENSEMBLE_TT)) - 16600 # 0

sub = BEST_L2_ENSEMBLE_ULTIMATE
sub[b,] = L1_BEST_ENSEMBLE_TT[b,]
sub[setdiff(a,b), ] = GBLINEAR1_GBTREE3_RF1_LAGGER1_XGBTREE_TT[setdiff(a,b), ]

samplesub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv')
samplesub[,4:23] = sub
write.table(samplesub, 'data/working/SUBS/BEST_L2_ENSEMBLE_ULTIMATE_TT.csv', quote=F, sep=',', row.names = F, col.names = T)

############################################################################


sub = read.csv('data/working/SUBS/BEST_L2_ENSEMBLE_TT.csv')[,4:23] %>% data.matrix
sub[which(sub < 0.01)]=0
sub = norm(sub) %>% data.frame
sub = data.frame(sub)
samplesub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv')
samplesub[,4:23] = sub
write.table(samplesub, 'data/working/SUBS/final.csv', quote=F, sep=',', row.names = F, col.names = T)

############################################################################

fnames = list.files(path = 'data/working/LAGGER_MODELS/', pattern = '[a-zA-Z0-9_/]+TR.csv$', recursive = T) %>% paste0('data/working/LAGGER_MODELS/',.)
fnames = fnames[c(47,50,49,31,13,27)]

best_TR = lapply(1:length(fnames), function(i){
  d = fread(fnames[i])*res$weights[i]
  d = as.data.frame(d)
}) %>% Reduce('+', .)

norm = function(data){
  t(apply(data, MARGIN = 1, function(x) x/sum(x)))
}

# convert near zero to zero
best_TR = best_TR %>% data.matrix # 0.1664189
tmp = best_TR
tmp[which(tmp < 0.01)]=0 # 0.1659322
#tmp[which(tmp > 0.967)]=1
#tmp[which(tmp < 0.54 & tmp > 0.46)] = 0.5
tmp = norm(tmp) %>% data.frame
BS(tmp, traintarget)






############################################################################

BS((TR %>% Reduce('+', .))/59, traintarget)
BS((TR %>% Reduce('+', .))/59, traintarget)
cv_scores= lapply(fnames, function(f){
  fread(f) %>% data.frame %>% BS(., traintarget)
}) %>% unlist; names(cv_scores) = fnames #unlist(lapply(strsplit(fnames, 'S/'), '[',2))
tmp = data.frame(cv_scores)

fnames=c('data/working/LAGGER_MODELS/MOFO3_330_XGBTREE/pred_TR.csv',
         'data/working/LAGGER_MODELS/MOFO1_1000_XGBTREE/pred_TR.csv',
         'data/working/LAGGER_MODELS/MOFO3_1681_XGBTREE/pred_TR.csv',
         'data/working/LAGGER_MODELS/GBLINEAR1_GBTREE3_RF1_LAGGER2_XGBLINEAR/pred_TR.csv',
         'data/working/LAGGER_MODELS/M18_RF/M18_RF_TR.csv',
         'data/working/LAGGER_MODELS/M10_RF/M10_RF_TR.csv',
         'data/working/LAGGER_MODELS/M12_RF/M12_RF_TR.csv')
TR = lapply(fnames, function(x) fread(x) %>% data.frame)
BS((TR %>% Reduce('+', .))/7, traintarget)


#set.seed(2); res = ens(TR[sample(1:59, 59)])
#res = ens(TR[c(47,48,50,46,49,31,13,17,27,39)]) # 0.1664596
#res = ens(TR[c(47,48,50,46,49,31,13,27)]) # 0.1664596
#res = ens(TR[c(47,50,46,49,31,13,27)]) # 0.1664553
res = ens(TR[c(47,50,49,31,13,27)]) # 0.1664189
#res = ens(TR[c(50,49,31,13,27)]) # 0.1666483

ans = sort.int(res$weights, index.return=T, decreasing = T)

fnames = list.files(path = 'data/working/LAGGER_MODELS/', pattern = '[a-zA-Z0-9_/]+TT.csv$', recursive = T) %>% paste0('data/working/LAGGER_MODELS/',.)
fnames = fnames[c(47,50,49,31,13,27)]
fnames = fnames[ans$ix[1:19]]
BEST_L2_ENSEMBLE = lapply(1:length(fnames), function(i){
  d = fread(fnames[i])*ans$x[1:19][i]
  d = as.data.frame(d)
}) %>% Reduce('+', .)
sum(complete.cases(BEST_L2_ENSEMBLE)) - 16600 # 1744
a = which(is.na(BEST_L2_ENSEMBLE[,1]))


RF1_GBTREE3_LAGGER1_XGBTREE_TT = read.csv('data/working/LAGGER_MODELS/RF1_GBTREE3_LAGGER1_XGBTREE/pred_TT.csv')
sum(complete.cases(RF1_GBTREE3_LAGGER1_XGBTREE_TT)) - 16600 # 1744
b = which(is.na(RF1_GBTREE3_LAGGER1_XGBTREE_TT[,1]))

L1_SECOND_BEST_ENSEMBLE_TT = fread('data/working/models/GBTREE3_TT.csv')*0.3528 + fread('data/working/models/RF1_TT.csv')*0.2400 + fread('data/working/models/GBTREE4_TT.csv')*0.0560 + fread('data/working/models/GBLINEAR1_TT.csv')*0.2000 + fread('data/working/models/GBTREE1_TT.csv')*0.1512 
sum(complete.cases(L1_SECOND_BEST_ENSEMBLE_TT)) - 16600 # 0

sub = BEST_L2_ENSEMBLE
sub[b,] = L1_SECOND_BEST_ENSEMBLE_TT[b,]
sub[setdiff(a,b), ] = RF1_GBTREE3_LAGGER1_XGBTREE_TT[setdiff(a,b), ]

samplesub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv')
samplesub[,4:23] = sub
write.table(samplesub, 'data/working/SUBS/BEST_L2_ENSEMBLE_TT.csv', quote=F, sep=',', row.names = F, col.names = T)

############################################################################
fnames = list.files(path = 'data/working/LAGGER_MODELS/', pattern = '[a-zA-Z0-9_/]+TR.csv$', recursive = T) %>% paste0('data/working/LAGGER_MODELS/',.)
cv_scores= lapply(fnames, function(f){
  fread(f) %>% data.frame %>% BS(., traintarget)
}) %>% unlist; names(cv_scores) = unlist(lapply(strsplit(fnames, 'S/'), '[',2))
tmp = data.frame(cv_scores)
