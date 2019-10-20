cweights = c(1.35298455691,1.38684574053,1.59587388404,1.35318713948,0.347783666015,
             0.661081706198,1.04723628621,0.398865222651,0.207586320237,1.50578335208,
             0.110181365961,1.07803284435,1.36560417316,1.17024113802,1.1933637414,
             1.1803704493,1.34414875433,1.11683830693,1.08083910312,0.503152249073)
BS <- function(pred, truth){
  apply((pred-truth)^2, MARGIN = 1, function(x) sum(x*cweights)) %>% mean
}

##########################################################

traintarget = read.csv('data/working/features/train.target')[,-(1:4)]
traintarget = traintarget[complete.cases(traintarget),]

##########################################################

# cv scores of 10 models
fnames = list.files(path = 'data/working/models/', pattern = '[a-zA-Z0-9_]+TR.csv$') %>% paste0('data/working/models/',.)
cv_scores= lapply(fnames, function(f){
  fread(f) %>% data.frame %>% BS(., traintarget)
}) %>% unlist; names(cv_scores) = fnames

# ET1_TR.csv                           ET2_TR.csv 
# 0.1948226                            0.2095151

# GBLINEAR1_TR.csv                     GBLINEAR2_TR.csv 
# 0.1981425                            0.2038196 

# GBTREE1_TR.csv                       GBTREE2_TR.csv 
# 0.1841828                            0.1847296 

# GBTREE3_TR.csv                       GBTREE4_TR.csv 
# 0.1820920                            0.1842051 

# RF1_TR.csv                           RF2_TR.csv 
# 0.1883972                            0.2080218

##########################################################

# GBTREE1 + GBTREE2
BS(((fread(fnames[5])+fread(fnames[6]))/2) %>% data.frame, traintarget) # 0.1838969
# GBLINEAR1 + GBLINEAR2
BS(((fread(fnames[3])+fread(fnames[4]))/2) %>% data.frame, traintarget) # 0.1983161
# ET1 + ET2
BS(((fread(fnames[1])+fread(fnames[2]))/2) %>% data.frame, traintarget) # 0.1968701
# RF1 + RF2
BS(((fread(fnames[9])+fread(fnames[10]))/2) %>% data.frame, traintarget) # 0.1912554

##########################################################

# best lags
bst = matrix(NA, nrow = 5, ncol = 20, dimnames = list(paste0('lag',1:5)))
for(nlag in 1:5){
  for(c in 1:20){
    bst[nlag,c]=cor(traintarget[,c], lag(traintarget[,c], nlag), use='pairwise.complete.obs')
  }
}

bst = cor(traintarget, dplyr::lag(traintarget, 1), use='pairwise.complete.obs')


# best lag features
imp = read.csv('data/working/LAGGER_MODELS/GBTREE3_LAGGER3_XGBTREE/imp.csv')[,c('Feature', paste0('Gain_',1:20))]
imp = imp[!imp$Feature %in% paste0('V',1:20,'_LAG0'),]
imp = apply(imp[,-1], MARGIN = 2, function(x) which(x>=0.03)) %>% unlist %>% unique %>% imp[.,'Feature'] %>% as.character

#####################################################################

# best linear ensemble of LEVEL 1
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
fnames = list.files(path = 'data/working/models/', pattern = '[a-zA-Z0-9_]+TR.csv$') %>% paste0('data/working/models/',.)

TR = lapply(fnames, function(x) fread(x) %>% data.frame)
res = ens(TR[c(7,5,8,6,9,1,3,4,10,2)]) # 0.1796927, 7=0.3528 5=0.1512 8=0.0560 9=0.2400 3=0.2000
res = ens(TR[c(7,9,8,5,10,3,1,2)]) # 0.1795994, 7=0.504, 9=0.216, 8=0.080, 3=0.200 
res = ens(TR[c(7,3,2, 1,10,9,5,8)]) # 0.1797967

# best linear ensemble of LAGGED MODELS
fnames = list.files(path = 'data/working/LAGGER_MODELS/', pattern = '[a-zA-Z0-9_/]+TR.csv$', recursive = T) %>% paste0('data/working/LAGGER_MODELS/',.)
TR = lapply(fnames, function(x) fread(x) %>% data.frame)
res = ens(TR)
set.seed(1)
res = ens(TR[c(9,sample(1:12, 12))])

# best linear ensemble of LAGGED + LEVEL 1
fnames = list.files(path = 'data/working/LAGGER_MODELS/', pattern = '[a-zA-Z0-9_/]+TR.csv$', recursive = T) %>% paste0('data/working/LAGGER_MODELS/',.)
fnames = c(fnames, list.files(path = 'data/working/models/', pattern = '[a-zA-Z0-9_]+TR.csv$') %>% paste0('data/working/models/',.))
TR = lapply(fnames, function(x) fread(x) %>% data.frame)
res = ens(TR)
res = ens(TR[c(9,19,13,11, 1:8,10,12,14:18,20:22)])

# more linear combinations
fnames = list.files(path = 'data/working/LAGGER_MODELS/', pattern = '[a-zA-Z0-9_/]+TR.csv$', recursive = T) %>% paste0('data/working/LAGGER_MODELS/',.)
#fnames = c(fnames, list.files(path = 'data/working/models/', pattern = '[a-zA-Z0-9_]+TR.csv$') %>% paste0('data/working/models/',.))
cv_scores= lapply(fnames, function(f){
  fread(f) %>% data.frame %>% BS(., traintarget)
}) %>% unlist; names(cv_scores) = fnames

TR = lapply(fnames, function(x) fread(x) %>% data.frame)
res = ens(TR) # 0.1676429
res = ens(TR[c(10, 25, 26, 30, 1, 2:9, 11:24, 27:29, 31:33)]) # 0.1675454
res = ens(TR[c(25, 10,12, 22, 9, 1, 5, 8, 30, 1:33)]) # 0.1674947

# more more linear combinations
fnames = list.files(path = 'data/working/LAGGER_MODELS/', pattern = '[a-zA-Z0-9_/]+TR.csv$', recursive = T) %>% paste0('data/working/LAGGER_MODELS/',.)
#fnames = c(fnames, list.files(path = 'data/working/models/', pattern = '[a-zA-Z0-9_]+TR.csv$') %>% paste0('data/working/models/',.))
cv_scores= lapply(fnames, function(f){
  fread(f) %>% data.frame %>% BS(., traintarget)
}) %>% unlist; names(cv_scores) = unlist(lapply(strsplit(fnames, 'S/'), '[',2))
tmp = data.frame(cv_scores)

TR = lapply(fnames, function(x) fread(x) %>% data.frame)
res = ens(TR) # 0.1676646
sort.int(res$weights, index.return = T)
res = ens(TR[c(30,13,17,27,37,36,15,11,2,12,1,10,6,3:7,8,9,14,16,18,19:26,28,29,31:35,38,39)]) # 0.1674546
res = ens(TR[c(13,17,30,17,27,37,36,15,11,2,12,1,10,6,3:7,8,9,14,16,18,19:26,28,29,31:35,38,39,13,12)]) # 0.1674519
res = ens(TR[c(13,17,30,17,27,37,8,9,14,16,18,19:26,28,29,38,39,13,12,30,17,27,37,11)]) # 0.1675289


# more more more linear combinations
fnames = list.files(path = 'data/working/LAGGER_MODELS/', pattern = '[a-zA-Z0-9_/]+TR.csv$', recursive = T) %>% paste0('data/working/LAGGER_MODELS/',.)
#fnames = c(fnames, list.files(path = 'data/working/models/', pattern = '[a-zA-Z0-9_]+TR.csv$') %>% paste0('data/working/models/',.))
cv_scores= lapply(fnames, function(f){
  fread(f) %>% data.frame %>% BS(., traintarget)
}) %>% unlist; names(cv_scores) = unlist(lapply(strsplit(fnames, 'S/'), '[',2))
tmp = data.frame(cv_scores)

TR = lapply(fnames, function(x) fread(x) %>% data.frame)
res = ens(TR) # 0.1668893
res = ens(TR[c(33, 31, 32, 30, 39, 13, 17, 27, 15, 11, 2, 12, 1, 10, 6,
               42, 41, 40, 38, 37, 36, 35, 34, 1:42)]) # 0.1666368
res = ens(TR[c(13, 31, 33, 31, 32, 30, 39, 13, 17, 27, 15, 11, 2, 12, 1, 10, 6,
               42, 41, 40, 38, 37, 36, 35, 34)]) # 0.1666433


# more more more more linear combinations
fnames = list.files(path = 'data/working/LAGGER_MODELS/', pattern = '[a-zA-Z0-9_/]+TR.csv$', recursive = T) %>% paste0('data/working/LAGGER_MODELS/',.)
#fnames = c(fnames, list.files(path = 'data/working/models/', pattern = '[a-zA-Z0-9_]+TR.csv$') %>% paste0('data/working/models/',.))
cv_scores= lapply(fnames, function(f){
  fread(f) %>% data.frame %>% BS(., traintarget)
}) %>% unlist; names(cv_scores) = unlist(lapply(strsplit(fnames, 'S/'), '[',2))
tmp = data.frame(cv_scores)

TR = lapply(fnames, function(x) fread(x) %>% data.frame)
res = ens(TR) # 0.1666011
res = ens(TR[c(46,47,45,48,31,13,17,27,33,15,30,11,2,12,1,10,6,57,56,55,54,53,52,51,
               50,49,3,4,5,7,8,9,14,16,18,19,20:29,32,34:44)]) # 0.1664732

# more more more more linear combinations
fnames = list.files(path = 'data/working/LAGGER_MODELS/', pattern = '[a-zA-Z0-9_/]+TR.csv$', recursive = T) %>% paste0('data/working/LAGGER_MODELS/',.)
fnames = c(fnames, list.files(path = 'data/working/models/', pattern = '[a-zA-Z0-9_/]+TR.csv$', recursive = T) %>% paste0('data/working/models/',.))

TR = lapply(fnames, function(x) fread(x) %>% data.frame)
res = ens(TR) # 0.1665513

fnames = list.files(path = 'data/working/models/', pattern = '[a-zA-Z0-9_/]+TR.csv$', recursive = T) %>% paste0('data/working/models/',.)
fnames = c(fnames, list.files(path = 'data/working/LAGGER_MODELS/', pattern = '[a-zA-Z0-9_/]+TR.csv$', recursive = T) %>% paste0('data/working/LAGGER_MODELS/',.))
TR = lapply(fnames, function(x) fread(x) %>% data.frame)
res = ens(TR) # 0.166539

#####################################################################

# collect featimps form MOFO
fnames = list.files(path = 'data/working/L2_features/', pattern = '[a-zA-Z0-9_]+TR.csv$') %>% paste0('data/working/L2_features/',.)
train = lapply(fnames, fread) %>% do.call(cbind, .) %>% data.frame
imp = read.csv('data/working/LAGGER_MODELS/MOFO_GBTREE1/imp.csv')[,c('Feature', paste0('Gain_',1:20))]

top = apply(imp[,-1], MARGIN = 2, function(x) which(x>=0.01)) %>% unlist %>% unique %>% imp[.,'Feature'] %>% as.character
f = which(names(train) %in% top)

top = apply(imp[,-1], MARGIN = 2, function(x) which(x>=0.005)) %>% unlist %>% unique %>% imp[.,'Feature'] %>% as.character
f = which(names(train) %in% top)

top = apply(imp[,-1], MARGIN = 2, function(x) which(x>=0.001)) %>% unlist %>% unique %>% imp[.,'Feature'] %>% as.character
f = which(names(train) %in% top)





