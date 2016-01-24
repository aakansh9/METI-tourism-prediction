
########################################################################################################
## model_last
## Cities = C6 (Toyama), C7 (Kanazawa)
## Parameters were selected by grid search
## Features = Custom features
## Target Variable = Number of Tourists
## Training Points = All 1:365 days (2014-06-01 to 2015-05-31) - New Year Time (Dec, Jan, Feb) [184:254]
########################################################################################################


print('Creating model_last for cities C6, C7')

# ... Selected Features for Toyama and Kanazawa ...

feat_list <- lapply(6:7, function(i){
  
  if (i==6){
    
    # features A (selected)
    featA <- read.csv(paste0('features/featA_C',i,'.csv'))
    featA <- featA[, c(1,5,6,7,16,18,21,22,27,31,33,34,35,38,39,42,45,47,48,53,54,60,61,62,
                       65,66,67,70,71,72,74,77,81,84,86,89,91,95,96,98,100,102,104,105,107,
                       111,119,121,122,123,126,129,130,133,134,136,137,138,140,141,142,144,
                       146,147,148,149,151,152,153,155,156,172,197,198,205,206,207,208,210,
                       211,234,238,248,249)] 
    
    # Google Trends features B (all)
    featB <- read.csv(paste0('features/featB_C',i,'.csv')) 
    
    feat <- join(featA, featB, by='date')
    
    # normalized predictions of other cities
    feat$'MODEL_x2_CITY_C5' <- preprocess.normalize(read.csv('model_x2_DATA/prediction.csv')[,-1], trainrows = 365)$C5
    feat$'MODEL_x3_CITY_C5' <- preprocess.normalize(read.csv('model_x3_DATA/prediction.csv')[,-1], trainrows = 365)$C5
    feat$'MODEL_x1_CITY_C9' <- preprocess.normalize(read.csv('model_x1_DATA/prediction.csv')[,-1], trainrows = 365)$C9
    feat$'MODEL_x2_CITY_C9' <- preprocess.normalize(read.csv('model_x2_DATA/prediction.csv')[,-1], trainrows = 365)$C9
    
    # other calendar features
    feat$CALENDER_is_wed_thu <- as.numeric(feat$CALENDER_wday %in% c(4,5))
    feat$CALENDER_month <- as.POSIXlt(sns$date)$mon+1
    feat$CALENDER_aug_indicator <- as.numeric(feat$CALENDER_month %in% c(8))
    
  } else if (i == 7){
    
    # features A (selected)
    featA <- read.csv(paste0('features/featA_C',i,'.csv'))
    featA <- featA[, c(1,3,4,6,17,24,31,37,42,44,46,48,51,57,59,61,62,63,66,67,71,72,74,78,
                       80,83,85,90,92,93,95,97,98,103,104,106,108,109,110,114,116,118,120,
                       123,127,130,131,132,133,135,136,138,139,140,141,143,146,150,151,152,
                       154,155,158,160,161,162,165,166,167,168,169,171,173,174,175,176,178,179,
                       191,204,214,217,223,224,225,228,229,238,252,255,256,257,261,266)]
    
    # features B (selected)
    featB <- read.csv(paste0('features/featB_C',i,'.csv'))
    featB <- featB[, c(1,5,9,10,11,18,19,20,21,23,31,34,35,36,43,46,52,57,58,59,60,61,62,65,66,75,
                       77,78,80,82,87,88,89,91,96,97,99,100,103,105,108,109,110,111)]
    feat <- join(featA, featB, by='date')
    
    # normalized predictions of other cities
    feat$'MODEL_basic_CITY_C2' <- preprocess.normalize(read.csv('model_basic_DATA/prediction.csv')[,-1], trainrows = 365)$C2
    feat$'MODEL_x1_CITY_C2' <- preprocess.normalize(read.csv('model_x1_DATA/prediction.csv')[,-1], trainrows = 365)$C2
    feat$'MODEL_x2_CITY_C2' <- preprocess.normalize(read.csv('model_x2_DATA/prediction.csv')[,-1], trainrows = 365)$C2
    
    # other calendar features
    feat$CALENDER_is_wed_thu <- as.numeric(feat$CALENDER_wday %in% c(4,5))
    feat$CALENDER_month <- as.POSIXlt(sns$date)$mon+1
    feat$CALENDER_aug_indicator <- as.numeric(feat$CALENDER_month %in% c(8))
  }
  
  return(feat[,-1])
})


# ... LOAD TARGET VARIABLE ...
target_list <- lapply(c(7,8), function(i){target_train[,i]})

# ... PARAMETERS ...
etas = c(0.01, 0.01)
subsamples = c(0.8, 0.8)
colsamples = c(0.9, 0.8)
depths = c(3, 3)
rounds = c(1183, 1113)
seeds = c(23, 23)

# ... CREATE OUT OF SAMPLE PREDICTIONS ...
submission <- get.xgb.submission(feat_list=lapply(1:2, function(i){feat_list[[i]][c(1:183,255:548), ]}), 
                                 target_list=lapply(1:2, function(i){target_list[[i]][c(1:183,255:365)]}), 
                                 logtransform=T, 
                                 eta_vec= etas, 
                                 subsample_vec= subsamples, 
                                 colsample_vec= colsamples, 
                                 depth_vec= depths, 
                                 rounds_vec= rounds, 
                                 seed_vec= seeds, 
                                 cores=6)

# ... SAVE EVERYTHING ...
NAME='last'
dir.create(paste0('model_',NAME,'_DATA'), recursive=T)

# save submission
sub <- read.csv('raw_data/sample_submit.csv', header=F) 
sub[,7] <- submission$subm[,2]
sub[,8] <- submission$subm[,3]
write.table(sub, paste0('submission_model_last.csv'), row.names=F, col.names=F, sep=',', quote=F)

# save models
setwd(paste0('model_',NAME,'_DATA/'))
cities=c(6,7)
for(i in 1:2){ xgb.save(submission$models[[i]], paste0('M',cities[i],'.model')) }

# save feature importances
for(i in 1:2){
  write.table(submission$featimps[[i]], file=paste0('M',cities[i],'.featimp'), row.names=F, col.names=T, sep=',', quote=F)
}

setwd(main_path)


