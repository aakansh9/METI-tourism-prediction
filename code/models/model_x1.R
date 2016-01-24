
###########################################################################################
## model_x1
## Cities = C2 (Sendai), C5 (Yugawara), C6 (Toyama), C7 (Kanazawa), C9 (Isemachi)
## Parameters were selected by grid search
## Features = Top 100 features of model_w1 + featB_Ci (Google Trends Features) (i=2,5,6,7,9)
## Target Variable = Number of Tourists
## Training Points = All 1:365 days (2014-06-01 to 2015-05-31)
###########################################################################################

print('Creating model_x1 for cities C2, C5, C6, C7, C9')


# ... LOAD TOP 100 FEATURES OF model_basic + GOOGLE TRENDS FEATURES ...
feat_list <- lapply(c(2,5,6,7,9), function(i){
  
  featA = read.csv(paste0('features/featA_C',i,'.csv'))
  if (i==2){
    featA <- featA[, c(1,4,15,16,21,29,30,31,42,46,47,48,49,50,52,53,54,57,59,60,63,64,65,67,69,70,
                       73,74,75,76,79,81,84,87,89,99,100,101,103,104,105,106,107,110,113,114,117,118,
                       120,125,127,135,137,138,140,141,142,143,145,146,147,148,149,150,151,152,153,
                       156,157,158,159,160,161,162,164,165,166,168,172,180,184,193,196,203,204,211,214,
                       215,216,217,218,220,221,222,223,226,230,238,239,248,258)]
  } else if (i==5){
    featA <- featA[, c(1,12,13,20,21,24,26,27,28,29,31,32,33,34,36,37,38,41,43,44,45,46,47,52,53,54,55,57,
                       58,59,60,63,66,68,69,76,77,78,82,83,84,86,87,88,92,93,95,97,98,99,101,103,104,105,
                       106,107,109,114,115,117,118,119,120,121,123,125,127,128,130,131,132,134,135,136,137,
                       138,139,141,144,145,157,158,160,169,170,175,181,185,186,187,189,191,192,194,195,197,
                       209,221,231,232)]
  } else if (i==6){
    featA <- featA[, c(1,5,6,7,16,18,21,22,27,29,31,33,34,35,38,39,41,42,43,44,45,47,48,54,55,56,60,61,62,
                       64,65,66,67,68,70,71,72,74,75,77,79,81,84,85,86,89,90,91,92,95,96,98,102,104,105,107,
                       111,112,118,119,120,121,122,123,126,129,130,133,134,136,137,138,140,142,144,146,147,
                       148,149,150,151,152,153,154,156,172,184,197,198,203,205,207,208,210,211,226,234,238,239,248)] 
  } else if (i==7){
    featA <- featA[, c(1,4,6,17,19,24,31,37,46,48,51,54,59,61,62,63,64,67,72,74,77,78,80,83,84,85,86,90,93,95,
                       97,99,104,106,108,109,110,113,116,118,120,122,123,127,129,131,132,133,134,135,136,138,
                       141,144,145,148,149,150,151,152,154,155,158,159,161,162,164,165,166,167,168,169,171,173,
                       174,175,176,177,178,179,180,187,191,193,203,213,214,215,217,223,224,225,228,229,234,244,
                       252,255,256,257,266)]
  } else if (i==9){
    featA <- featA[, c(1,8,10,27,31,33,34,39,40,41,43,47,48,49,53,54,57,59,60,61,64,65,67,68,72,73,74,76,77,78,
                       79,81,82,84,85,91,92,93,96,97,100,101,102,104,107,108,110,111,112,114,115,116,117,118,119,
                       121,123,125,127,128,134,135,137,138,140,141,142,144,147,148,150,153,156,159,161,162,163,164,
                       165,176,177,187,188,189,199,201,203,204,205,206,210,211,220,224,225,234,236,242,245,246,247)]
  }

  featB = read.csv(paste0('features/featB_C',i,'.csv'))
  
  feat <- join(featA, featB, by='date')
  return(feat[,-1])
})

# ... LOAD TARGET VARIABLE ...
target_list <- lapply(c(3,6,7,8,10), function(i){target_train[,i]})

# ... PARAMETERS ...
etas = c(0.01, 0.01, 0.01, 0.01, 0.01)
subsamples = c(0.8, 0.8, 0.8, 0.8, 0.8)
colsamples = c(0.6, 0.4, 0.3, 0.6, 0.5)
depths = c(5, 4, 3, 4, 4)
rounds = c(1002, 941, 1192, 1044, 977)
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
NAME='x1'
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

