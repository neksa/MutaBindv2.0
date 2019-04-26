#!/usr/bin/env Rscript

library('randomForest')
rm(list=ls())

test <- read.csv(file='2.D.1.txt',header = TRUE, sep = '\t')
## MutaBind
# load('s3.fit')
# load('s3.rf')
load('model.mlr')
load('model.rf')

# test$dVdwWT <- test$dE_vdw_wt
# test$dVdwMut <- test$dE_vdw_mut
# test$dPbWT <- test$dG_solv_wt
# test$dPbMut <- test$dG_solv_mut
# test$ACC <- test$SA_com_wt
# test$ACCm <- test$SA_part_wt
# test$MutNameP <- test$dPro_mut
# test$ResNameP <- test$dPro_wt
# test$FoldX_fold <- test$ddG_fold
# test$provean <- test$CS

test$mlr_pred <- predict(model.mlr,test)
test$rf_pred <- predict(model.rf,test)
test$MutaBind <- (test$mlr_pred+test$rf_pred)/2
cor(test$DDGexp,test$MutaBind)

## 1O2+1O2R(MutaBindS)
train.f <- read.csv(file='1.O.2+1.O.2.R.txt',header = TRUE, sep = '\t')

label <- 'DDGexp~dE_vdw_wt+dE_vdw_mut+dG_solv_mut+dG_solv_wt+dPro_mut+dPro_wt+SA_com_wt+SA_part_wt+CS+ddG_fold'
train.f.mlr <- lm(as.formula(label),data=train.f)
set.seed(100)
train.f.rf <- randomForest(as.formula(label), data = train.f)
test$mlr_pred <- predict(train.f.mlr,test)
test$rf_pred <-  predict(train.f.rf,test)
test$MutaBindS <- (test$mlr_pred+test$rf_pred)/2
cor(test$DDGexp,test$MutaBindS)
