#################################################################################

# ... function to load SNS data related to city C ...
get.sns.citydata <- function(city=NA, prefix){
  
  # input: NA/city name
  # output: data frame with 548 rows and 
  #         columns c(date, xxx_bbs, xxx_blog, xxx_twitter) for that city
  
  if(is.na(city)){
    keywords <- as.character(sns_keywords_cat$keyword[which(is.na(sns_keywords_cat$city))])
  } else{
    keywords <- as.character(sns_keywords_cat$keyword[which(sns_keywords_cat$city==city)])
  }
  res <- sns[,c(1,1+which(unlist(lapply(strsplit(colnames(sns)[-1], '_'), '[',1)) %in% keywords))]
  colnames(res)[-1] <- paste0(prefix, colnames(res)[-1])
  return(res)
}

#################################################################################

# ... function to create basic statistical features from SNS data ...
get.sns.stats <- function(data, prefix){
  
  # input: data = num data frame with sns columns in order (bbs, blog, twitter),
  #        prefix = prefic for colnames
  # ouput: num data frame with 4 sum features
  
  res = cbind.data.frame('sum_bbs_blog_twitter' = rowSums(data),
                         'sum_bbs' = rowSums(data[, seq(1,ncol(data), 3)]),
                         'sum_blog' = rowSums(data[, seq(2,ncol(data), 3)]),
                         'sum_twitter' = rowSums(data[, seq(3,ncol(data), 3)]))
  colnames(res) <- paste0(prefix, colnames(res))
  return(res)
}

#################################################################################

# ... function to find kkk_bbs + kkk_blog + kkk_twitter for every keyword kkk
preprocess.tripletsum <- function(data, prefix){
  
  # input: numeric data frame
  # output: numeric data frame with every 3 columns summed up
  
  l = ((ncol(data)/3)-1) # always integer
  res = data.frame(do.call(cbind, lapply(0:l, function(i){
    return(rowSums(data[,(3*i + 1):(3*i + 3)]))
  })))
  colnames(res) <-  paste0(unlist(lapply(strsplit(colnames(data)[seq(1,ncol(data),3)], '_'), '[', 1)), '_total')
  colnames(res) <- paste0(prefix, colnames(res))
  return(res) 
}

#################################################################################

get.snsgeo.stats <- function(){
  
  # output: num 548 X 2 data frame     
  
  x <- sns_geo[,-1]
  nx <- scale(x, center=F, scale=apply(x,2,sd,na.rm=T)) # divide by sd
  res <- cbind.data.frame('sns_geo_mean' = rowMeans(x),
                   'sns_geo_norm_mean' = rowMeans(nx))
  return(res)
}

#################################################################################

get.sensor.stats <- function(){
  
  # output: num (548 X 49 = 26852) X 5 data frame
  
  x <- sensor[,-c(1:2)]
  res <- cbind.data.frame('sensor_GapDayTemp' = sensor$sensor_MaxDayTemp - sensor$sensor_MinDayTemp,
                   'sensor_RelativeHumidity_mean' = rowMeans(sensor[,6:9]),
                   'sensor_RelativeHumidity_sd' = apply(sensor[,6:9], 1, sd),
                   'sensor_TotalPrecipitation_mean' = rowMeans(sensor[,11:14]),
                   'sensor_TotalPrecipitation_sd' = apply(sensor[,11:14], 1, sd)
                   )
  return(res)
}

#################################################################################

get.sensor.stationdata <- function(station){
  
  # input: station name as string
  # output: 548 X (ncol(sensor)-1)  data frame for that station
  
  res <- split(sensor,sensor$sensor_station_id)[[station]][,-2]
  colnames(res)[-1] <- paste0(station, '_', colnames(res)[-1])
  return(res) 
}

#################################################################################

# ... function to find nearest stations to a given city ...
get.sensor.nnstations <- function(city, n){
  
  # input: city name, n
  # output: list of sorted distances and n-nearest station names
  
  city_coords <- city.codetable[which(city.codetable$new_city_id==city), c('lat', 'long')]
  station_coords <- sensor_station.codetable[,c('lat', 'long')]
  dists <- rdist.earth(city_coords, station_coords)
  res <- as.character(sensor_station.codetable$sensor_station_id[sort.int(dists, index.return=T)$ix[1:n]])
  return(list('dists' = sort(dists), 'names'=res))
}

#################################################################################

# ... convert text features/ categorical features to one hot encoding ....
get.weather.textfeat <- function(){
  
  # output: num data frame with (548 X 36 = 19728) X 50 text features from weather data
  
  res <- data.frame('id'=rownames(weather))
  
  # one hot encodings for wind features
  tmp <- data.frame(model.matrix(~weather_MaxWindSpeed_Direction-1, weather)); tmp$id <- rownames(tmp)
  res <- join(res, tmp, by='id')
  
  tmp <- data.frame(model.matrix(~weather_MaxInstanWindSpeed_Direction-1, weather)); tmp$id <- rownames(tmp)
  res <- join(res, tmp, by='id')
  
  tmp <- data.frame(model.matrix(~weather_MostWind_Direction-1, weather)); tmp$id <- rownames(tmp)
  res <- join(res, tmp, by='id')
  
  # NA(=0) vs text(=1) for weather_day/weather_night
  res$weather_weather_Day_istext <- as.numeric(!is.na(weather$weather_Weather_Day))
  res$weather_weather_Night_istext <- as.numeric(!is.na(weather$weather_Weather_Night))
  
  res$id <- NULL
  
  return(res)
}

#################################################################################

# ... remove text features from weather data ...
remove.weather.text <- function(){
  
  # output: weather data with text columns removed
  
  r = which(colnames(weather) %in% c("weather_MaxWindSpeed_Direction" , "weather_MaxInstanWindSpeed_Direction",
                                     "weather_MostWind_Direction" ,"weather_Weather_Day", "weather_Weather_Night"))
  return(weather[, -r])
}

#################################################################################

get.weather.sitedata <- function(site){
  
  # input: site name as string
  # output: 548 X (ncol(weather)-1)  data frame for that site
  
  res <- split(weather,weather$w_site_id)[[site]][,-2]
  colnames(res)[-1] <- paste0(site, '_', colnames(res)[-1])
  return(res) 
}

#################################################################################

# ... get nearest weather sites to a given city ...
get.weather.nnsites <- function(city, n){
  
  # input: city name, n
  # output: list of sorted distances and n-nearest weather site names
  
  city_coords <- city.codetable[which(city.codetable$new_city_id==city), c('lat', 'long')]
  site_coords <- weather_site.codetable[,c('lat', 'long')]
  dists <- rdist.earth(city_coords, site_coords)
  res <- as.character(weather_site.codetable$w_site_id[sort.int(dists, index.return=T)$ix[1:n]])
  return(list('dists' = sort(dists), 'names'=res))
}

#################################################################################

# ... function to create calendar features ...
get.calendar.feat <- function(city, prefix){
  
  res <- cbind.data.frame('date' = sns$date,
                   'mday' = as.POSIXlt(sns$date)$mday,
                   'month' = as.POSIXlt(sns$date)$mon+1,
                   'wday'=as.POSIXlt(sns$date)$wday+1) # basic features
  
  res1 <- cbind.data.frame('is_saturday' = as.numeric(res$wday==7),
                   'is_sunday' = as.numeric(res$wday==1))
  
  peak_indicator <- as.numeric(as.numeric(res$wday==7) + # saturdays
                                 as.numeric(res$wday==1) + # sundays
                                 national_holidays$nat_holiday_Japan +  # national holidays
                                 c(rep(0,213),1,1,1,rep(0,332)) + # New Year time 
                                 as.numeric(res$date %in% 
                                              as.POSIXct(c('2014-08-14', '2014-08-15', 
                                                           '2014-08-16', '2015-08-14', 
                                                           '2015-08-15', '2015-08-16')))  >=1 # obon
                               )
  
  trend_indicator <- as.numeric(school_holidays$school_holiday_Japan + # school holidays
                                  as.numeric(res$date %in% 
                                               as.POSIXct(c('2014-07-20', '2014-07-21', 
                                                            '2014-07-22', '2015-07-19', 
                                                            '2015-07-20', '2015-07-21'))) + # around sea day (20 july)
                                  as.numeric(res$date %in% 
                                               as.POSIXct(c('2014-12-31', '2015-01-01', 
                                                            '2015-01-02'))) + # around new year
                                  as.numeric(res$date %in% 
                                               as.POSIXct(c('2015-05-01', '2015-05-02', 
                                                            '2015-05-03'))) +  # pre golden week (may1-3)
                                  as.numeric(res$date %in% 
                                               as.POSIXct(c('2014-08-14', '2014-08-15', 
                                                            '2014-08-16', '2015-08-14', 
                                                            '2015-08-15', '2015-08-16'))) + # around obon
                                  sakura_momiji[,paste0('saku_momi_',city)] >=1 
                                ) 
  
  peak_estimate <- function(){
    pestimate <- factor(strsplit(paste(national_holidays$nat_holiday_Japan, collapse=''), '00*')[[1]])
    levels(pestimate) <- c('0','01','010','01000')
    tmp = rep(0,548)
    tmp[which(national_holidays$nat_holiday_Japan != 0)] <- strsplit(paste(as.character(pestimate), collapse=''),'')[[1]]
    pestimate <- as.numeric(tmp)
    return(pestimate)
  }

  res2 <- cbind.data.frame('peak_indicator' = peak_indicator,
                           'peak_indicator_ma' = ma(peak_indicator, order=3),
                           'trend_indicator' = trend_indicator,
                           'trend_indicator_ma' = ma(trend_indicator, order=3),
                           'peak_estimate' = peak_estimate(),
                           'peak_estimate_ma' = ma(peak_estimate(), order=3)
                           )

  national_holiday_conseccounts <- function(){
      tmp <- data.frame(do.call(cbind, lapply(2:12, function(i){
        tmp = strsplit(paste(national_holidays[,i], collapse=''), '00*')[[1]]
        res = rep(0, nrow(national_holidays))
        res[which(national_holidays[,i] != 0)] <- strsplit(paste(as.numeric(tmp)*nchar(tmp), collapse=''),'')[[1]]
        return(as.numeric(res))
      })))
      colnames(tmp) <- paste0(colnames(national_holidays[,2:12]),'_consec_count')
      return(tmp)
  }
  
  result <- cbind.data.frame(res, res1, res2, 
                             national_holidays[,-1], 
                             school_holidays[,-1],
                             national_holiday_conseccounts())
  
  result$JunetoNov_indicator <- c(rep(1,183), rep(0,182), rep(1,183))
  
  colnames(result)[-1] <- paste0(prefix, colnames(result)[-1])
  return(result)  
}

#################################################################################

# ... function to remove near zero variance and correlated features from data ...
preprocess.reduce <- function(data, freqCut=95/5, uniqueCut=5, corCutoff=0.90, na.replace=NA){
  
  data[is.na(data)]<-na.replace
  
  # remove near zero variance features
  data.nzv <- nearZeroVar(data, saveMetrics=T, freqCut = freqCut, uniqueCut = uniqueCut)
  data <- data[,  c(rownames(data.nzv)[which(data.nzv$nzv == F)])]
  
  # remove correlated features
  data.cor <- cor(sapply(data, as.numeric), use='pairwise.complete.obs')
  data.cor[is.na(data.cor)] <- 0
  rem <- findCorrelation(data.cor, cutoff=corCutoff, verbose=F)
  if(length(rem)==0){ 
    data <- data[, c(rownames(data.cor))]
  } else {
    data <- data[, c(rownames(data.cor)[-rem])]
  }
  return(colnames(data))
}

#################################################################################

# ... function to normalize data for 0 mean and 1 sd ...
preprocess.normalize <- function(data, trainrows = NA){
  
  # data = numeric data frame
  # trainrows = rows to consider for calculating mean and sd
  
  if(is.na(trainrows)){
    
    res <- as.data.frame(do.call(cbind, lapply(1:ncol(data), function(i){
      data[,i] <- (data[,i] - mean(data[,i], na.rm=T))/sd(data[,i], na.rm = T)
    })))
    colnames(res) <- colnames(data)
    
    return(res)
    
  } else {
    
    data1 <- data[1:trainrows, ]
    data2 <- data[(trainrows+1):nrow(data), ]
    
    res1 <- as.data.frame(do.call(cbind, lapply(1:ncol(data1), function(i){
      data1[,i] <- (data1[,i] - mean(data1[,i], na.rm=T))/sd(data1[,i], na.rm = T)
    })))
    colnames(res1) <- colnames(data1)
    
    res2 <- as.data.frame(do.call(cbind, lapply(1:ncol(data2), function(i){
      data2[,i] <- (data2[,i] - mean(data1[,i], na.rm=T))/sd(data1[,i], na.rm = T)
    })))
    colnames(res2) <- colnames(data2)
    
    res <- rbind(res1, res2)
    return(res)
  }
  
}

#################################################################################

# ... function to convert weekly google trends features to daily features by imputing same values ...

convert.weeklytodaily <- function(data){ # data should be for 83 weeks (2014-05-04 to 2015-11-29)
  
  data <- data[rep(1:nrow(data), rep(7, nrow(data))), ]; rownames(data) <- NULL
  data <- data[-which(data$date < as.POSIXct('2014-06-01') | data$date > as.POSIXct('2015-11-30')),]; rownames(data)<-NULL
  data <- data[1:548, ]
  data[,1] <- sns$date
  return(data)
}

#################################################################################

# ... function to create Time Series features for Google Trends data ...
preprocess.TSshiftfeatures <- function(data, removerows=NULL){
  
  result <- as.data.frame(do.call(cbind, lapply(2:ncol(data), function(i){
    
    res <- cbind.data.frame('s1' = c(NA, data[,i])[1:nrow(data)], # shift by 1 week
                     's2' = c(NA, NA, data[,i])[1:nrow(data)], # shift by 2 weeks
                     's3' = c(NA, NA, NA, data[, i])[1:nrow(data)]) # shift by 3 weeks
    res <- cbind.data.frame(res,
                            'mean_s1s2' = (res$s1 + res$s2)/2, # mean of last 2 weeks
                            'mean_s1s2s3' = (res$s1 + res$s2 + res$s3)/3) # mean of last 3 weeks
    colnames(res) <- paste0(colnames(data)[i],'_',colnames(res))
    return(res)
  })))
  result[removerows, ] <- NA
  result <- cbind.data.frame('date'= data[,1], result)
  
  return(result)
}

#################################################################################



