# ... sns ...
sns <- read.csv(CLEANED_sns_path)
sns$date <- as.POSIXct(sns$date)

# ... sns keywords category ...
sns_keywords_cat <- read.csv(sns_keywords_category_path)
sns_keywords_cat$city <- as.character(sns_keywords_cat$city)

# ... sns_geo ...
sns_geo <- read.csv(CLEANED_sns_geo_path)
sns_geo$date <- as.POSIXct(sns_geo$date)

# ... exchange ...
exchange <- read.csv(CLEANED_exchange_path)
exchange$date <- as.POSIXct(exchange$date)

# ... sensor ...
sensor <- read.csv(CLEANED_sensor_path)
sensor$date <- as.POSIXct(sensor$date)

# ... weather ...
weather <- read.csv(CLEANED_weather_path)
weather$date <- as.POSIXct(weather$date)

# ... target train ...
target_train <- read.csv(CLEANED_target_train_path)
target_train$date <- as.POSIXct(target_train$date)

# ... code tables ...
city.codetable <- read.csv(CLEANED_city_codes_path)
weather_site.codetable <- read.csv(CLEANED_weather_site_codes_path)
sensor_station.codetable <- read.csv(CLEANED_sensor_station_codes_path)

# ... national holidays ...
national_holidays <- read.csv(national_holidays_path)
national_holidays$date <- as.POSIXct(national_holidays$date)

# ... school holidays ...
school_holidays <- read.csv(school_holidays_path)
school_holidays$date <- as.POSIXct(school_holidays$date)

# ... sakura momiji season ...
sakura_momiji <- read.csv(sakura_momiji_path)
sakura_momiji$date <- as.POSIXct(sakura_momiji$date)


# ... CITYWISE GOOGLE TRENDS DATA ...

sendai_gtrends_weekly <- read.csv(sendai_gtrends_path)
sendai_gtrends_weekly$date <- as.POSIXct(sendai_gtrends_weekly$date)

yugawaramachi_gtrends_weekly <- read.csv(yugawaramachi_gtrends_path)
yugawaramachi_gtrends_weekly$date <- as.POSIXct(yugawaramachi_gtrends_weekly$date)

toyama_gtrends_weekly <- read.csv(toyama_gtrends_path)
toyama_gtrends_weekly$date <- as.POSIXct(toyama_gtrends_weekly$date)

kanazawa_gtrends_weekly <- read.csv(kanazawa_gtrends_path)
kanazawa_gtrends_weekly$date <- as.POSIXct(kanazawa_gtrends_weekly$date)

isemachi_gtrends_weekly <- read.csv(isemachi_gtrends_path)
isemachi_gtrends_weekly$date <- as.POSIXct(isemachi_gtrends_weekly$date)

rm(CLEANED_city_codes_path, CLEANED_exchange_path, CLEANED_sns_path, CLEANED_prefecture_codes_path,
   CLEANED_sensor_path, CLEANED_sensor_station_codes_path, CLEANED_sns_geo_path,
   CLEANED_target_train_path, CLEANED_weather_path, CLEANED_weather_site_codes_path,
   RAW_city_codes_path, RAW_exchange_test_path, RAW_exchange_train_path, RAW_geo_location_test_path,
   RAW_geo_location_train_path, RAW_prefecture_codes_path, RAW_sensor_station_codes_path,
   RAW_sensor_test_path, RAW_sensor_train_path, RAW_sns_test_path, RAW_sns_train_path,
   RAW_target_train_path, RAW_weather_site_codes_path, RAW_weather_test_path, RAW_weather_train_path)

rm(isemachi_gtrends_path, kanazawa_gtrends_path, national_holidays_path,
   sakura_momiji_path, school_holidays_path, sendai_gtrends_path, sns_keywords_category_path,
   toyama_gtrends_path, yugawaramachi_gtrends_path)


