samplesub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv')
ens = read.csv('data/working/xgboost_ensemble/test_preds/test_pred_1.csv')
xgb = read.csv('data/working/models/test_pred_1_xgboost_july28_FEATPRUNE2_01823.csv')
samplesub[,4:23] = ens
samplesub[which(samplesub$end %in% c(1,2)),4:23] = xgb[which(samplesub$end %in% c(1,2)), ]

write.table(samplesub, 'data/working/subs/ens_2.csv', quote=F, sep=',', row.names = F, col.names = T)
