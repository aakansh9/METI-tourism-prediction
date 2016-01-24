
#################################################################################

MAE <- function(preds, dtrain){
  labels <- getinfo(dtrain, 'label')
  err <- sum(abs(as.numeric(labels) - as.numeric(preds)))/length(labels)
  return(list(metric='MAE', value=err))
}

MAEexp <- function(preds, dtrain){
  labels <- getinfo(dtrain, 'label')
  err <- sum(abs(exp(as.numeric(labels)) - exp(as.numeric(preds))))/length(labels)
  return(list(metric='MAEexp', value=err))
}

#################################################################################

get.xgb.submission <- function(feat_list, # list of R-row data frames
                               target_list, # list of r-length vectors (r < R)
                               logtransform=T,
                               eta_vec, subsample_vec, colsample_vec, depth_vec, rounds_vec, 
                               seed_vec, cores=4){
  
  # prepare empty lists
  models <- list()
  preds <- list()
  featimps <- list()
  
  # predict
  for(i in 1:length(feat_list)){
    
    print(paste0('Modeling and predicting for item  ', i))
    
    params <- list(booster='gbtree', 
                   objective = 'reg:linear',
                   eta = eta_vec[i], 
                   subsample = subsample_vec[i],
                   colsample_bytree = colsample_vec[i],
                   max_depth = depth_vec[i], 
                   verbose = 1,
                   nthread=cores)
    
    if(logtransform == T)
      target_list[[i]] <- log(target_list[[i]])
    
    dtrain <- xgb.DMatrix(data = data.matrix(feat_list[[i]][1:length(target_list[[i]]), ]), label = target_list[[i]], missing=NA)
    dtest <- xgb.DMatrix(data = data.matrix(feat_list[[i]][(length(target_list[[i]])+1):nrow(feat_list[[i]]), ]), missing=NA)
    
    if(logtransform == T){
      models[[i]] <- xgb.train(params, data=dtrain, feval=MAEexp, maximize=F, 
                               nrounds = rounds_vec[i], seed=seed_vec[i])
    } else {
      models[[i]] <- xgb.train(params, data=dtrain, feval=MAE, maximize=F, 
                               nrounds = rounds_vec[i], seed=seed_vec[i])
    }
    
    preds[[i]] <- predict(models[[i]], dtest)
    featimps[[i]] <- xgb.importance(feature_names=colnames(feat_list[[i]]), model=models[[i]]) 
    
    if(logtransform == T)
      preds[[i]] <- exp(preds[[i]])
    
  }
  
  # create submission
  sub_sample <- read.csv('~/Projects/Uhuru-METI-Tourism/raw_data/sample_submit.csv', header=F)
  subm <- cbind.data.frame('date'=as.character(sub_sample$V1)[1:length(preds[[1]])], data.frame(do.call(cbind, preds)))
  
  # return
  return(list('models'=models, 'featimps'=featimps, 'subm'=subm))
}

#################################################################################

