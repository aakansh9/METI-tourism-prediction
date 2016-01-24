######### sns data #########
sns_train <- read.csv(RAW_sns_train_path); sns_train$date <- as.POSIXct(sns_train$date)
sns_test <- read.csv(RAW_sns_test_path); sns_test$date <- as.POSIXct(sns_test$date)


######### exchange data #########
exchange_train <- read.csv(RAW_exchange_train_path); exchange_train$date <- as.POSIXct(exchange_train$date)

# ... impute missing exchange train data ...
exchange_train <- merge(sns_train[,1:2], exchange_train, by='date', all.x=T)[,-2]
exchange_train[1, 2:6] <- exchange_train[2, 2:6]
exchange_train[364:365, 2:6] <- exchange_train[363, 2:6]
for (colnum in 2:6){
  col = exchange_train[, colnum]
  for (i in 2:363){
    if (is.na(col[i]))
      exchange_train[i, colnum] <- mean(col[i-2:i+2], na.rm=T)
  }
}; rm(col, colnum, i)

exchange_test <- read.csv(RAW_exchange_test_path); exchange_test$date <- as.POSIXct(exchange_test$date)

# ... impute missing exchange test data ...
exchange_test <- merge(sns_test[,1:2], exchange_test, by='date', all.x=T)[,-2]
exchange_test[181:182, 2:6] <- (exchange_test[180, 2:6] + exchange_test[183, 2:6])/2
for (colnum in 2:6){
  col = exchange_test[, colnum]
  for (i in 5:180){
    if (is.na(col[i]))
      exchange_test[i, colnum] <- mean(col[i-2:i+2], na.rm=T)
  }
}; rm(col, colnum, i)


######### code tables #########
weathersite_codes <- read.table(RAW_weather_site_codes_path, header = T)
colnames(weathersite_codes) <- c('w_site_id', 'jp_name')
weathersite_codes$en_name <- c('Hakodate', 'Hokuto', 'Sendai', 'Shiogama', 'Ishinomaki', 'Tokyo', 'EdogawaRinkai',
                               'Odawara', 'Yokohama', 'Gotenba', 'Ajiro', 'Mishima', 'Kanazawa', 'Kahoku', 
                               'HakusanKawachi', 'Himi', 'Toyama', 'Tonami', 'Omata' ,'Tsu', 'Toba', 'MinamiIse', 
                               'Kyoto', 'Sonobe', 'Kyotanabe', 'Izumo', 'Matsue', 'Hikawa', 'Hiroshima', 
                               'HigashiHiroshima', 'Wu', 'Nagasaki', 'Izuhara', 'Fukue', 'Ishigaki', 'Ibaruma')
coords <- c('41.768793_140.72881', 
            '41.824060_140.652903', 
            '38.268215_140.869356',
            '38.314362_141.022027',
            '38.434480_141.302917',
            '35.689487_139.691706',
            '35.647772_139.868506',
            '35.264564_139.152154',
            '35.443708_139.638026',
            '35.308584_138.934506',
            '34.662545_135.563483',
            '35.118402_138.918513',
            '36.561325_136.656205',
            '36.719826_136.706727',
            '36.397274_136.623725',
            '36.855981_136.972849',
            '36.695952_137.213677',
            '36.647534_136.962009',
            '36.375067_139.374327',
            '34.718596_136.505697',
            '34.481435_136.843362',
            '34.352095_136.703689',
            '35.011636_135.768029',
            '35.102556_135.483076',
            '34.814459_135.767695',
            '35.367035_132.754682',
            '35.468060_133.048375',
            '34.813002_135.441991',
            '34.385203_132.455293',
            '34.426380_132.743307',
            '34.249254_132.565805',
            '32.750286_129.877667',
            '34.191353_129.306606',
            '35.147943_136.912382',
            '24.406403_124.175444',
            '24.508246_124.282209')
weathersite_codes$lat <- as.numeric(unlist(lapply(strsplit(coords, '_'),'[',1)))
weathersite_codes$long <- as.numeric(unlist(lapply(strsplit(coords, '_'),'[',2)))

prefecture_codes <- read.table(RAW_prefecture_codes_path)
colnames(prefecture_codes) <- c('pref_id', 'jp_name')
prefecture_codes$en_name <- c('Hokkaido' ,'Aomori' ,'Iwate','Miyagi','Akita','Yamagata','Fukushima',
                              'Ibaraki','Tochigi','Gunma','Saitama','Chiba','Tokyo','Kanagawa','Niigata',
                              'Toyama','Ishikawa','Fukui','Yamanashi','Nagano','Gifu','Shizuoka','Aichi',
                              'Mie','Shiga','Kyoto','Osaka','Hyogo','Nara','Wakayama','Tottori','Shimane',
                              'Okayama','Hiroshima','Yamaguchi','Tokushima','Kagawa','Ehime','Kochi',
                              'Fukuoka','Saga','Nagasaki','Kumamoto','Oita','Miyazaki','Kagoshima','Okinawa')
prefecture_codes$pref_id <- paste0('pref', prefecture_codes$pref_id)



city_codes <- read.table(RAW_city_codes_path, header = T)
colnames(city_codes) <- c('city_id', 'jp_name')
city_codes$en_name <- c('Hakodate_Hokkaido', 'Sendai_Miyagi', 'Chuo_Tokyo', 'Hakonemachi_Kanagawa',
                        'Yugawaramachi_Kanagawa', 'Toyama_Toyama', 'Kanazawa_Ishikawa', 'Atami_Shizuoka',
                        'Isemachi_Mie', 'Kyoto_Kyoto', 'Izumo_Shimane', 'Hiroshima_Hiroshima',
                        'Nagasaki_Nagasaki', 'Ishigaki_Okinawa')
coords <- c('41.768793_140.72881',
            '38.268215_140.869356',
            '35.670651_139.771861',
            '35.232355_139.106937',
            '35.147964_139.108317',
            '36.695952_137.213677',
            '36.561325_136.656205',
            '35.096276_139.071705',
            '34.487514_136.709336',
            '35.011636_135.768029',
            '35.367035_132.754682',
            '34.385203_132.455293',
            '32.750286_129.877667',
            '24.340661_124.15558')
city_codes$lat <- as.numeric(unlist(lapply(strsplit(coords, '_'),'[',1)))
city_codes$long <- as.numeric(unlist(lapply(strsplit(coords, '_'),'[',2)))
rm(coords)
city_codes$city_id <- paste0('C', city_codes$city_id)
city_codes$new_city_id <- paste0('C',1:14)


sensor_station_codes <- read.table(RAW_sensor_station_codes_path, header =T)
colnames(sensor_station_codes) <- c('sensor_station_id', 'jp_address')
sensor_station_codes$sensor_station_id <- paste0('s', sensor_station_codes$sensor_station_id)
coords <- c('41.885753_141.069138',
            '41.797894_141.175521',
            '41.733501_140.936589',
            '41.839638_140.908151',
            '41.830111_141.111773',
            '38.215356_140.930691',
            '38.303645_140.815656',
            '38.337774_140.883856',
            '38.213056_140.803003',
            '38.328819_140.602278',
            '38.373454_140.788677',
            '35.667710_139.768642',
            '35.642892_139.749722',
            '35.308046_139.245081',
            '35.202527_139.13751',
            '35.031539_138.9942',
            '34.959161_139.115397',
            '34.503531_136.792989',
            '34.502485_136.74088',
            '34.586195_136.622497',
            '36.712442_137.320839',
            '36.629575_137.093617',
            '36.783768_137.365624',
            '36.448945_136.629014',
            '36.532212_136.532169',
            '36.584665_136.584238',
            '36.488413_136.766839',
            '35.124062_135.642529',
            '35.108185_135.820398',
            '35.002351_135.870446',
            '34.974505_135.811855',
            '35.319311_132.766004',
            '35.270273_132.580303',
            '35.456200_132.76015',
            '35.335693_132.837335',
            '34.473608_132.23905',
            '34.391258_132.458753',
            '34.582540_132.632287',
            '34.435684_132.445177',
            '34.343496_132.524193',
            '34.364384_132.35486',
            '32.878681_129.684375',
            '24.286667_123.881667',
            '24.340661_124.15558',
            '32.627714_129.833959',
            '32.802885_129.974917',
            '32.704140_129.90945',
            '32.871144_129.793472',
            '35.639389_139.82234')
sensor_station_codes$lat <- as.numeric(unlist(lapply(strsplit(coords, '_'),'[',1)))
sensor_station_codes$long <- as.numeric(unlist(lapply(strsplit(coords, '_'),'[',2)))
rm(coords)

######### geo_location data #########
geo_location_train <- read.csv(RAW_geo_location_train_path)
colnames(geo_location_train)[-1] <- city_codes$city_id
geo_location_train$date <- as.POSIXct(geo_location_train$date)
geo_location_test <- read.csv(RAW_geo_location_test_path)
colnames(geo_location_test)[-1] <- city_codes$city_id
geo_location_test$date <- as.POSIXct(geo_location_test$date)




######### sensor data #########
sensor_train <- read.csv(RAW_sensor_train_path)
colnames(sensor_train) <- c('date', 'sensor_station_id', 'AvgDayTemp', 'MaxDayTemp',
                            'MinDayTemp', 'RelativeHumidity_0', 'RelativeHumidity_6',
                            'RelativeHumidity_12', 'RelativeHumidity_18', 'MaxPrecipitation_X1',
                            'TotalPrecipitation_0_6', 'TotalPrecipitation_6_12', 
                            'TotalPrecipitation_12_18', 'TotalPrecipitation_18_24')
sensor_train$date <- as.POSIXct(sensor_train$date)
sensor_train$sensor_station_id <- paste0('s', sensor_train$sensor_station_id)


sensor_test <- read.csv(RAW_sensor_test_path)
colnames(sensor_test) <- c('date', 'sensor_station_id', 'AvgDayTemp', 'MaxDayTemp',
                            'MinDayTemp', 'RelativeHumidity_0', 'RelativeHumidity_6',
                            'RelativeHumidity_12', 'RelativeHumidity_18', 'MaxPrecipitation_X1',
                            'TotalPrecipitation_0_6', 'TotalPrecipitation_6_12', 
                            'TotalPrecipitation_12_18', 'TotalPrecipitation_18_24')
sensor_test$date <- as.POSIXct(sensor_test$date)
sensor_test$sensor_station_id <- paste0('s', sensor_test$sensor_station_id)




######### weather data #########
weather_train <- read.csv(RAW_weather_train_path)
colnames(weather_train) <- c('date', 'w_site_id', 
                             'Sunshine','Sunshine_AnnualAvg',
                             'GlobalSolarRad', 'GlobalSolarRad_AnnualAvg',
                             'AvgWindSpeed', 'MaxWindSpeed',
                             'MaxWindSpeed_Direction', 'MaxInstanWindSpeed',
                             'MaxInstanWindSpeed_Direction', 'MostWind_Direction',
                             'AvgCloudCover', 'AvgCloudCover_AnnualAvg',
                             'Weather_Day', 'Weather_Night',
                             'AvgLocalPressure', 'Snowfall',
                             'Snowfall_AnnualAvg', 'DeepestSnow',
                             'DeepestSnow_AnnualAvg')
weather_train$date <- as.POSIXct(weather_train$date)
weather_train$w_site_id <- as.character(weather_train$w_site_id)

# convert '' to NA and dtype to factor
weather_train$MaxWindSpeed_Direction[which(weather_train$MaxWindSpeed_Direction=='')] <- NA
weather_train$MaxWindSpeed_Direction <- as.factor(as.character(weather_train$MaxWindSpeed_Direction))
weather_train$MaxInstanWindSpeed_Direction[which(weather_train$MaxInstanWindSpeed_Direction=='')] <- NA
weather_train$MaxInstanWindSpeed_Direction <- as.factor(as.character(weather_train$MaxInstanWindSpeed_Direction))
weather_train$MostWind_Direction[which(weather_train$MostWind_Direction=='')] <- NA
weather_train$MostWind_Direction <- as.factor(as.character(weather_train$MostWind_Direction))
weather_train$Weather_Day[which(weather_train$Weather_Day=='')] <- NA
weather_train$Weather_Day <- as.factor(as.character(weather_train$Weather_Day))
weather_train$Weather_Night[which(weather_train$Weather_Night=='')] <- NA
weather_train$Weather_Night <- as.factor(as.character(weather_train$Weather_Night))



weather_test <- read.csv(RAW_weather_test_path)
colnames(weather_test) <- c('date', 'w_site_id', 
                             'Sunshine','Sunshine_AnnualAvg',
                             'GlobalSolarRad', 'GlobalSolarRad_AnnualAvg',
                             'AvgWindSpeed', 'MaxWindSpeed',
                             'MaxWindSpeed_Direction', 'MaxInstanWindSpeed',
                             'MaxInstanWindSpeed_Direction', 'MostWind_Direction',
                             'AvgCloudCover', 'AvgCloudCover_AnnualAvg',
                             'Weather_Day', 'Weather_Night',
                             'AvgLocalPressure', 'Snowfall',
                             'Snowfall_AnnualAvg', 'DeepestSnow',
                             'DeepestSnow_AnnualAvg')
weather_test$date <- as.POSIXct(weather_test$date)
weather_test$w_site_id <- as.character(weather_test$w_site_id)

# convert '' to NA and dtype to factor
weather_test$MaxWindSpeed_Direction[which(weather_test$MaxWindSpeed_Direction=='')] <- NA
weather_test$MaxWindSpeed_Direction <- as.factor(as.character(weather_test$MaxWindSpeed_Direction))
weather_test$MaxInstanWindSpeed_Direction[which(weather_test$MaxInstanWindSpeed_Direction=='')] <- NA
weather_test$MaxInstanWindSpeed_Direction <- as.factor(as.character(weather_test$MaxInstanWindSpeed_Direction))
weather_test$MostWind_Direction[which(weather_test$MostWind_Direction=='')] <- NA
weather_test$MostWind_Direction <- as.factor(as.character(weather_test$MostWind_Direction))
weather_test$Weather_Day[which(weather_test$Weather_Day=='')] <- NA
weather_test$Weather_Day <- as.factor(as.character(weather_test$Weather_Day))
weather_test$Weather_Night[which(weather_test$Weather_Night=='')] <- NA
weather_test$Weather_Night <- as.factor(as.character(weather_test$Weather_Night))




######### train target data #########
target_train <- read.csv(RAW_target_train_path)
target_train$date <- as.POSIXct(target_train$date)
tmp <- data.frame(do.call(rbind, strsplit(colnames(target_train)[-1], '_')))
tmp$X1 <- factor(tmp$X1)
levels(tmp$X1) <- paste0('C',1:14)
tmp$X2 <- factor(tmp$X2)
levels(tmp$X2) <- c(as.character(1:47), 'inbound', 'japan', 'total',
                    'unknown', 'middleage', 'minor', 'old', 'young')
colnames(target_train)[-1] <- paste0(tmp$X1, '_', tmp$X2)
rm(tmp)

# ... save all cleaned datasets ...

# sns
sns <- rbind(sns_train, sns_test)
sns$"温泉_twitter.1" <-NULL # repeated
sns$"温泉_bbs.1" <- NULL
sns$"温泉_blog.1" <- NULL
write.table(sns, CLEANED_sns_path, row.names=F, col.names = T, sep=',', quote=F)

# exchange
exchange <- rbind(exchange_train, exchange_test)
colnames(exchange)[-1] <- paste0('exchange_', colnames(exchange)[-1])
write.table(exchange, CLEANED_exchange_path, row.names=F, col.names = T, sep=',', quote=F)

# SNS geo_location
sns_geo <- rbind(geo_location_train, geo_location_test)
colnames(sns_geo)[-1] <- paste0('sns_geo_', colnames(sns_geo)[-1])
write.table(sns_geo, CLEANED_sns_geo_path, row.names=F, col.names = T, sep=',', quote=F)

# weather 
weather <- rbind(weather_train, weather_test)
colnames(weather)[-c(1:2)] <- paste0('weather_', colnames(weather)[-c(1:2)])
write.table(weather, CLEANED_weather_path, row.names=F, col.names = T, sep=',', quote=F)

# sensor
sensor <- rbind(sensor_train, sensor_test)
colnames(sensor)[-c(1:2)] <- paste0('sensor_', colnames(sensor)[-c(1:2)])
write.table(sensor, CLEANED_sensor_path, row.names=F, col.names = T, sep=',', quote=F)

# codetables
write.table(city_codes, CLEANED_city_codes_path, row.names=F, col.names = T, sep=',', quote=F)
write.table(prefecture_codes, CLEANED_prefecture_codes_path, row.names=F, col.names = T, sep=',', quote=F)
write.table(sensor_station_codes, CLEANED_sensor_station_codes_path, row.names=F, col.names = T, sep=',', quote=F)
write.table(weathersite_codes, CLEANED_weather_site_codes_path, row.names=F, col.names = T, sep=',', quote=F)

# target train
write.table(target_train, CLEANED_target_train_path, row.names=F, col.names = T, sep=',', quote=F)

rm(city_codes, exchange, exchange_test, exchange_train, geo_location_test, geo_location_train, 
   prefecture_codes, sensor, sensor_test, sensor_train, sensor_station_codes, sns, sns_geo, sns_test,
   sns_train, weather, weather_train, weather_test, target_train, weathersite_codes)







