
##############################################################################
## model_basic
## Cities = C2 (Sendai), C5 (Yugawara), C6 (Toyama), C7 (Kanazawa), C9 (Isemachi)
## Parameters were selected by grid search
## Features = featA (Basic Features + Calendar Features) (city=C2,C5,C6,C7,C9)
## Target Variable = Number of Tourists
## Training Points = All 1:365 days (2014-06-01 to 2015-05-31)
##############################################################################


print('Creating model_basic for cities C2, C5, C6, C7, C9')

# ... LOAD FEATURES ...
feat_list <- lapply(c(2,5,6,7,9), function(i){
  feat = read.csv(paste0('features/featA_C',i,'.csv'))[,-1]
})

# ... LOAD TARGET VARIABLE ...
target_list <- lapply(c(3,6,7,8,10), function(i){target_train[,i]})


# ... PARAMETERS ...
etas = c(0.01, 0.01, 0.01, 0.01, 0.01)
subsamples = c(0.8, 0.8, 0.8, 0.8, 0.8)
colsamples = c(0.6, 0.3, 0.6, 0.3, 0.3)
depths = c(5, 3, 3, 3, 5)
rounds = c(662, 773, 1114, 901, 547)
seeds = c(23, 23, 23, 23, 23)


print(' Creating out of sample preditions 366:548')
# ... CREATE OUT OF SAMPLE PREDICTIONS ...
submission <- get.xgb.submission(feat_list=feat_list, 
                                  target_list=target_list, 
                                  logtransform=T, 
                                  eta_vec= etas, 
                                  subsample_vec= subsamples, 
                                  colsample_vec= colsamples, 
                                  depth_vec= depths, 
                                  rounds_vec= rounds, 
                                  seed_vec= seeds, 
                                  cores=6)


# ... CREATE IN SAMPLE REDICTIONS ...
# train on days 1:183 and predict for days 184:365
print(' Creating in sample preditions 184:365')
insample1_2 <- get.xgb.submission(feat_list=lapply(1:5, function(i){feat_list[[i]][1:365, ]}), 
                                  target_list=lapply(1:5, function(i){target_list[[i]][1:183]}), 
                                  logtransform=T, # TRUE
                                  eta_vec= etas, 
                                  subsample_vec= subsamples, 
                                  colsample_vec= colsamples, 
                                  depth_vec= depths, 
                                  rounds_vec= rounds, 
                                  seed_vec= seeds, 
                                  cores=6)

# train on days 184:365 and predict for days 1:183
print(' Creating in sample preditions 1:183')
insample1_1 <- get.xgb.submission(feat_list=lapply(1:5, function(i){feat_list[[i]][c(184:365,1:183), ]}), 
                                  target_list=lapply(1:5, function(i){target_list[[i]][184:365]}), 
                                  logtransform=T, # TRUE
                                  eta_vec= etas, 
                                  subsample_vec= subsamples, 
                                  colsample_vec= colsamples, 
                                  depth_vec= depths, 
                                  rounds_vec= rounds, 
                                  seed_vec= seeds, 
                                  cores=10)


# ... SAVE EVERYTHING ...
NAME='basic'
dir.create(paste0('model_',NAME,'_DATA'), recursive=T)

# save insample + outofsample predictions
res <- rbind(insample1_1$subm, insample1_2$subm, submission$subm)
colnames(res) <- c('date', 'C2', 'C5', 'C6', 'C7', 'C9')
res$date <- sns$date
write.table(res, paste0('model_',NAME,'_DATA/prediction.csv'), row.names=F, col.names=T, sep=',', quote=F)

# save models
setwd(paste0('model_',NAME,'_DATA/'))
cities=c(2,5,6,7,9)
for(i in 1:5){ xgb.save(submission$models[[i]], paste0('M',cities[i],'.model')) }

# save feature importances
for(i in 1:5){
  write.table(submission$featimps[[i]], file=paste0('M',cities[i],'.featimp'), row.names=F, col.names=T, sep=',', quote=F)
}

setwd(main_path)




