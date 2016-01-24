sub <- read.csv('submission_model_last.csv', header = F)
w1 <- read.csv('model_basic_DATA/prediction.csv')[366:548, ]
x2 <- read.csv('model_x2_DATA/prediction.csv')[366:548, ]

pred_C6 <- (sub[,7] + w1$C6)/2
pred_C7 <- (sub[,8] + x2$C7)/2

sub <- read.csv('raw_data/sample_submit.csv', header=F) 
sub[,7] <- pred_C6
sub[,8] <- pred_C7
write.table(sub, paste0('submission_ensemble.csv'), row.names=F, col.names=F, sep=',', quote=F)
