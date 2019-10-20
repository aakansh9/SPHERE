library(dplyr)
library(glmnet)
library(data.table)

# load train features
train = lapply(1:10, function(i){
  fread(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/columns.csv'))
}) %>% do.call(rbind, .) %>% data.frame

# load train target
traintarget = lapply(1:10, function(i){
  fread(paste0('data/raw_data/public_data/train/',sprintf('%05d',i),'/targets.csv'))
}) %>% do.call(rbind, .) %>% data.frame

train = train[complete.cases(traintarget),]
traintarget = traintarget[complete.cases(traintarget),-(1:2)]

# load test features
sub = fread('data/raw_data/public_data/submission_uniform_baseline.csv') %>% data.table
test_nrows = sub[,by='record_id', max(end)]$V1 # 5-29 sec

test = lapply(11:882, function(i){
  df = read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/columns.csv')) %>% data.frame
  df = df[1:test_nrows[i-10], ]
  return(df)
}) %>% do.call(rbind, .) %>% data.frame

# remove duplicate columns
remDupcols <- function(data){
  rem = which(!(names(data) %in% colnames(unique(as.matrix(data), MARGIN=2))))
  return(rem)
}
rem = remDupcols(train)
train = train[,-rem]
test = test[,-rem]

# remove highly correlated features from data
require(caret)
remHighcor <- function(data, cutoff, ...){
  data_cor <- cor(sapply(data, as.numeric), ...)
  data_cor[is.na(data_cor)] <- 0
  rem <- findCorrelation(data_cor, cutoff=cutoff, verbose=T)
  return(rem)
}
rem = remHighcor(train, cutoff = 0.99, use='pairwise.complete.obs')
train = train[,-rem]
test = test[,-rem]

rm(rem, remHighcor, remDupcols, test_nrows, sub)
gc()

folds = list(1:1753, 1754:3395, 3396:4911, 4912:6448, 6449:7955, 7956:9347, 9348:10852, 10853:12520, 12521:14345, 14346:16124)

train[is.na(train)] = 0

require(foreach)
require(doMC)
registerDoMC(15)

#cv = cv.glmnet(x = data.matrix(train), y=data.matrix(traintarget), family='multinomial',foldid = folds, parallel=T, nlambda=20)

train = train[,-which(names(train) %in% c('pir_bath_std'))]
test = test[,-which(names(test) %in% c('pir_bath_std'))]

require(caret)
# center, scale, YeoJohnson transform
preprocessor <- preProcess(train, method=c('center','scale'))
train = predict(preprocessor, train)#; train$target = traintarget
#test = predict(preprocessor, test); test$target = testtarget

print('cross validating ...')
# tuning, model
registerDoMC(cores = detectCores()-1)
set.seed(123)
data=train

res = list()
for( c in 1:20){
  
  model = train(x=data, y=traintarget[,c], method='glmnet', 
                trControl = trainControl(method = "repeatedcv", number = 10, repeats = 1, 
                                         allowParallel = T, savePredictions = 'final', classProbs = F, index=folds), 
                tuneGrid = expand.grid(alpha=seq(0,1,0.1), lambda = seq(0,1,0.1))
  ) # alpha = 0.02 , lambda = 0.002
  
  # cv score
  tmp = model$pred
  scores = list(sum(((tmp[which(tmp$Resample == 'Resample01'), 'obs'])-(tmp[which(tmp$Resample == 'Resample01'), 'pred']))^2),
                sum(((tmp[which(tmp$Resample == 'Resample02'), 'obs'])-(tmp[which(tmp$Resample == 'Resample02'), 'pred']))^2),
                sum(((tmp[which(tmp$Resample == 'Resample03'), 'obs'])-(tmp[which(tmp$Resample == 'Resample03'), 'pred']))^2),
                sum(((tmp[which(tmp$Resample == 'Resample04'), 'obs'])- (tmp[which(tmp$Resample == 'Resample04'), 'pred']))^2),
                sum(((tmp[which(tmp$Resample == 'Resample05'), 'obs'])- (tmp[which(tmp$Resample == 'Resample05'), 'pred']))^2),
                sum(((tmp[which(tmp$Resample == 'Resample06'), 'obs'])- (tmp[which(tmp$Resample == 'Resample06'), 'pred']))^2),
                sum(((tmp[which(tmp$Resample == 'Resample07'), 'obs'])- (tmp[which(tmp$Resample == 'Resample07'), 'pred']))^2),
                sum(((tmp[which(tmp$Resample == 'Resample08'), 'obs'])- (tmp[which(tmp$Resample == 'Resample08'), 'pred']))^2),
                sum(((tmp[which(tmp$Resample == 'Resample09'), 'obs'])- (tmp[which(tmp$Resample == 'Resample09'), 'pred']))^2),
                sum(((tmp[which(tmp$Resample == 'Resample10'), 'obs'])- (tmp[which(tmp$Resample == 'Resample10'), 'pred']))^2))
  res[[c]] = unlist(scores)
}

tmp = res
res = do.call(rbind, res) %>% data.frame

cweights = c(1.35298455691,1.38684574053,1.59587388404,1.35318713948,0.347783666015,
             0.661081706198,1.04723628621,0.398865222651,0.207586320237,1.50578335208,
             0.110181365961,1.07803284435,1.36560417316,1.17024113802,1.1933637414,
             1.1803704493,1.34414875433,1.11683830693,1.08083910312,0.503152249073)
res = apply(res, MARGIN = 2, function(x) sum(x*cweights)/20) 
res %>% mean
res %>% sd


