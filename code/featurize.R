
# ... create features for city Ci using different functions ...

#################################################################################

# ... featurize A = basic features + calendar features ...
featurizeA <- function(city){
  
  # ... load sensor ...
  sensor <- read.csv('cleaned_data/sensor.csv')
  sensor$date <- as.POSIXct(sensor$date)
  
  # ... load weather ...
  weather <- read.csv('cleaned_data/weather.csv')
  weather$date <- as.POSIXct(weather$date)
  
  # ... sns.feat ...
  sns.feat <- cbind.data.frame(get.sns.citydata(city=city, prefix='CITYKEYWORDS_'),
                               get.sns.stats(data = get.sns.citydata(city=NA, prefix='')[,-1], prefix='COMMONKEYWORDS_'),
                               get.sns.stats(data = get.sns.citydata(city=city, prefix='')[,-1], prefix='CITYKEYWORDS_'),
                               get.sns.stats(data = sns[,-1], prefix='ALLKEYWORDS_'),
                               preprocess.tripletsum(data=get.sns.citydata(city=NA, prefix='')[,-1], prefix = 'COMMONKEYWORDS_')
  )
  
  # ... snsgeo.feat ...
  snsgeo.feat <- cbind.data.frame(sns_geo, get.snsgeo.stats())
  
  # ... sensor.feat ...
  sensor <- cbind.data.frame(sensor, get.sensor.stats())
  nnstations <- get.sensor.nnstations(city=city, n=3)$names
  sensor.feat <- merge_recurse(lapply(1:length(nnstations), function(i){
    get.sensor.stationdata(station=nnstations[i])
  }), by='date')
  
  # ... weather.feat ...
  weather <- cbind.data.frame('date' = weather$date,
                              'w_site_id' = weather$w_site_id,
                              get.weather.textfeat(), remove.weather.text()[,-c(1:2)])
  nnsites <- get.weather.nnsites(city = city, n=2)$names
  weather.feat <- merge_recurse(lapply(1:length(nnsites), function(i){
    get.weather.sitedata(site = nnsites[i])  
  }), by='date')
  weather.feat <- weather.feat[,c('date', preprocess.reduce(weather.feat[1:365,-1], corCutoff = 0.9, 
                                                            freqCut = 10, uniqueCut = 5, na.replace=NA))] # 1:365 is important to stop leakage
  
  # ... calender.feat ...
  calendar.feat <- get.calendar.feat(city=city, prefix='CALENDER_')
  
  # ... merge ...
  feat <- merge_recurse(list(sns.feat, snsgeo.feat, sensor.feat, weather.feat, calendar.feat), by='date')
  
  # ... remove factor features (for example weather_Night, weather_Day, ... ) ...
  feat <- feat[,-which(colnames(feat) %in% names(which(unlist(lapply(feat, class)) == 'factor')))]
  
  # ... return ...
  return(feat)
}

# ... CREATE AND SAVE FEATURES ...
feat_list <- lapply(1:14, function(i){featurizeA(city=paste0('C',i))})
for(i in c(2,5,6,7,9)){
  write.table(feat_list[[i]], paste0('features/featA_C',i,'.csv'), row.names=F, col.names=T, sep=',', quote=F)
}

#################################################################################

# ... featurize B = Google Trends features ...
featurizeB <- function(city){ 
  
  if (city == 'C2'){ # Sendai
    feat <- sendai_gtrends_weekly
  } else if (city == 'C5'){ # Yugawara
    feat <- yugawaramachi_gtrends_weekly
  } else if (city == 'C6'){ # Toyama
    feat <- toyama_gtrends_weekly
  } else if (city == 'C7'){ # Kanazawa
    feat <- kanazawa_gtrends_weekly
  } else if (city == 'C9'){ # Isemachi
    feat <- isemachi_gtrends_weekly
  }
  
  # remove near zero variance features
  feat.nzv <- nearZeroVar(feat[,-1], saveMetrics=T, freqCut = 95/5, uniqueCut = 5)
  feat <- feat[,  c('date', rownames(feat.nzv)[which(feat.nzv$nzv == F)])]
  
  # make TSshift features
  feat <- preprocess.TSshiftfeatures(feat)
  
  # remove correlated and nzv variables
  feat <- feat[,c('date', preprocess.reduce(feat[,-1], freqCut=95/5, uniqueCut=5, corCutoff=0.90, na.replace=NA))]
  
  # convert from weekly to daily
  feat <- convert.weeklytodaily(feat)
  
  # normalize
  feat <- cbind.data.frame('date'=feat$date, preprocess.normalize(feat[,-1], trainrows=365))
  
  # return
  return(feat)
}

# ... CREATE AND SAVE FEATURES ...
cities = c(2,5,6,7,9)
feat_list <- lapply(cities, function(i){featurizeB(city=paste0('C',i))})
for(i in 1:5){
  write.table(feat_list[[i]], paste0('features/featB_C',cities[i],'.csv'), row.names=F, col.names=T, sep=',', quote=F)
}

#################################################################################
rm(city.codetable, exchange, isemachi_gtrends_weekly, kanazawa_gtrends_weekly, national_holidays,
   sakura_momiji, school_holidays, sendai_gtrends_weekly, sensor, sensor_station.codetable,
   sns_geo, sns_keywords_cat, toyama_gtrends_weekly, weather_site.codetable, weather,
   yugawaramachi_gtrends_weekly, cities, feat_list, i, 
   convert.weeklytodaily, featurizeB, featurizeA, get.calendar.feat, get.sensor.nnstations,
   get.sensor.stationdata, get.sensor.stats, get.sns.citydata, get.sns.stats, get.snsgeo.stats,
   get.weather.nnsites, get.weather.sitedata, get.weather.textfeat, preprocess.reduce,
   preprocess.tripletsum, preprocess.TSshiftfeatures, remove.weather.text)



