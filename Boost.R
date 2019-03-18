library(doParallel)
library(caret)
library(xgboost)

set.seed(123)

f <- read.table(file="SkempiS_WithModelFeatures.txt", header = TRUE, sep = '\t')
rownames(f) <- paste(f$PDB.id,f$Partner1,f$Partner2,f$Mutated.Chain,f$Mutation,f$label_dataset,sep = '_')                # for single mutation
c(dim(f)[1],length(unique(f$PDB.id)))

data = f[, c("DDGexp", "dE_vdw_wt", "dE_vdw_mut", "dG_solv_mut", "dG_solv_wt", "dPro_mut", "dPro_wt", "SA_com_wt", "SA_part_wt", "CS", "ddG_fold")]
# x <- as.matrix(data, rownames.force=NA)

cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 3, verboseIter=TRUE)

xgb.grid <- expand.grid(nrounds = 500,
                        max_depth = seq(6,10),
                        eta = c(0.01,0.3, 1),
                        gamma = c(0.0, 0.2, 1),
                        colsample_bytree = c(0.5,0.8, 1),
                        min_child_weight=seq(1,3),
                        subsample = 1
)

# The tuning parameter grid should have columns nrounds, max_depth, eta, gamma, colsample_bytree, min_child_weight, subsample
#print(xgb.grid)

#cl <- makeCluster(detectCores())
#registerDoParallel(cl)
#
#xgb_tune <-train(DDGexp ~.,
#                 data=data,
#                 method="xgbTree",
#                 metric = "RMSE",
#                 trControl=cv.ctrl,
#                 tuneGrid=xgb.grid,
#                 verbose=TRUE
#)
#stopCluster(cl)

xgb.best <- list(max_depth = 8, eta = 0.01, gamma = 0.2, colsample_bytree = 0.8, min_child_weight = 1, subsample = 1)

intrain <- createDataPartition(y=data$DDGexp, p=0.9, list=FALSE, groups=5)

train_data <- xgb.DMatrix(data=as.matrix(data[intrain, -which(names(data)=="DDGexp")]), label=data[intrain, ]$DDGexp)
test_data <- xgb.DMatrix(data=as.matrix(data[-intrain, -which(names(data)=="DDGexp")]), label=data[-intrain, ]$DDGexp)
xgb_tune <- xgb.train(params=xgb.best, data=train_data, nrounds=500)

# performance on training set
prediction <- predict(xgb_tune, train_data)
cor(data[intrain,]$DDGexp, prediction)

# performance on testing set
prediction <- predict(xgb_tune, test_data)
cor(data[-intrain,]$DDGexp, prediction)








