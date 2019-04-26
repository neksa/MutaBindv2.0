#!/usr/bin/env Rscript

library(xgboost)

set.seed(100)

f <- read.table(file="SkempiS_WithModelFeatures.txt", header = TRUE, sep = '\t')
rownames(f) <- paste(f$PDB.id,f$Partner1,f$Partner2,f$Mutated.Chain,f$Mutation,f$label_dataset,sep = '_')                # for single mutation
# rownames(f) <- paste(f$PDB.id,f$Partner1,f$Partner2,f$Mutation,f$label_dataset,sep = '_')                              # for multiple mutation

xgb_columns <- c("DDGexp", "dE_vdw_wt", "dE_vdw_mut", "dG_solv_wt", "dG_solv_mut","SA_com_wt", "SA_part_wt", "CS", "ddG_fold", "dPro_wt", "dPro_mut")
data <- f[, xgb_columns[2:length(xgb_columns)]]
label <- f[, xgb_columns[1]]

# Build XGBoost model
xgb.best <- list(max_depth = 4, eta = 0.005, gamma = 0.4, colsample_bytree = 0.3, min_child_weight = 3, subsample = 0.4)
train_data <- xgb.DMatrix(data=as.matrix(data), label=label)
model.boost <- xgb.train(params=xgb.best, data=train_data, nrounds=100)
prediction <- predict(model.boost, train_data)
cor(f$DDGexp, prediction)
save(model.boost, file = 'model.boost')

test <- read.csv(file='2.D.1.txt',header = TRUE, sep = '\t')
test_data <- xgb.DMatrix(data=as.matrix(test[, xgb_columns[2:length(xgb_columns)]]), label=test[, xgb_columns[1]])
test$boost_pred <- predict(model.boost, test_data)
cor(test$DDGexp, test$boost_pred)

## 1O2+1O2R(MutaBindS)
train.f <- read.csv(file='1.O.2+1.O.2.R.txt',header = TRUE, sep = '\t')

train_data_2 <- xgb.DMatrix(data=as.matrix(train.f[, xgb_columns[2:length(xgb_columns)]]), label=train.f[, xgb_columns[1]])
model.boost_2 <- xgb.train(params=xgb.best, data=train_data, nrounds=100)
pred_2 <- predict(model.boost_2, test_data)
cor(test$DDGexp, pred_2)
