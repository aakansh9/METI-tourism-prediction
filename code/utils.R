# ... install and load the following packages ...
require(data.table)
require(fields) # rdist.earth
require(reshape) # merge_recurse
require(plyr) # join
require(caret) # nzv
require(forecast) # ma, stl
require(xgboost)
require(Metrics)


require(Ckmeans.1d.dp) # plot xgboost feature importance
require(ggplot2); theme_set(theme_gray(base_size=12, base_family="HiraKakuProN-W3"))

# ... set main_path as path to this directory ...
main_path = '~/Projects/METI_tourism/'
setwd(main_path)


# ... run the code ...
dir.create('cleaned_data', recursive = T)
dir.create('features', recursive = T)
source('code/paths.R') # load path names
source('code/clean.R') # data cleaning
source('code/load.R') # load cleaned data
source('code/featurize_functions.R') # load functions to create features
source('code/featurize.R') # create features A (Basic + Calendar features) and features B (Google Trends features) and save

source('code/models/model_functions.R')
source('code/models/model_basic.R')
source('code/models/model_x1.R')
source('code/models/model_x2.R')
source('code/models/model_x3.R')
source('code/models/model_last.R')
source('code/models/ensemble.R')








