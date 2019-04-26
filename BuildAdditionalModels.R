#!/usr/bin/env Rscript

# library(car)
library(randomForest)
library(caret)
set.seed(100)

# setwd("")     # can change
f <- read.table(file="SkempiS_4191.txt", header = TRUE, sep = '\t')
# rownames(f) <- paste(f$PDB.id,f$Partner1,f$Partner2,f$Mutated.Chain,f$Mutation,f$label_dataset,sep = '_')                # for single mutation
# rownames(f) <- paste(f$PDB.id,f$Partner1,f$Partner2,f$Mutation,f$label_dataset,sep = '_')                              # for multiple mutation
# c(dim(f)[1],length(unique(f$PDB.id)))

label <- 'DDGexp~dE_vdw_wt+dE_vdw_mut+dG_solv_mut+dG_solv_wt+dPro_mut+dPro_wt+SA_com_wt+SA_part_wt+CS+ddG_fold'         # model features of single mutation
# label <- 'DDGexp~dE_vdw_wt+dE_vdw_mut+dG_solv_mut+dG_solv_wt+ddG_fold+CS+ddE_elec_.site.site.+ddrSA'                      # model features of multiple mutation

# # Build Model, MLR
# model.mlr <- lm(as.formula(label), data = f)
# summary(model.mlr)
# cor.test(f$DDGexp, fitted(model.mlr))
# f$ddG_pred_mlr <- fitted(model.mlr)

# # Build Model, RF
# model.rf <- randomForest(as.formula(label), data = f) 
# f$ddG_pred_rf <- model.rf$predicted
# cor.test(f$DDGexp,f$ddG_pred_rf)

# ## Build Model, RF optimized
# # set.seed(100)
# # model.opt.rf <- randomForest(as.formula(label), data = f, mtry=3, nodesize=3, ntree=5000) 
# # f$ddG_pred_opt_rf <- model.opt.rf$predicted
# # cor.test(f$DDGexp,f$ddG_pred_opt_rf)

# ## mean
# f$MutaBindS <- (f$ddG_pred_mlr + f$ddG_pred_rf) / 2
# cor.test(f$DDGexp,f$MutaBindS)
# sqrt(mean((f$DDGexp - f$MutaBindS)^2, na.rm = TRUE))   # RMSE

# ## save model
# # save(model.mlr,file = 'model.mlr')  
# # save(model.rf,file = 'model.rf')


print("SPLIT TRAIN/TEST")
data = f[, c("DDGexp", "dE_vdw_wt", "dE_vdw_mut", "dG_solv_mut", "dG_solv_wt", "dPro_mut", "dPro_wt", "SA_com_wt", "SA_part_wt", "CS", "ddG_fold")]

intrain <- createDataPartition(y=data$DDGexp, p=0.6, list=FALSE, groups=5)
train_data <- data[intrain,] #, -which(names(data)=="DDGexp")]
train_y <- data[intrain, ]$DDGexp
test_data <- data[-intrain,] # , -which(names(data)=="DDGexp")]
test_y <- data[-intrain, ]$DDGexp

# Build Model, MLR
model.mlr <- lm(as.formula(label), data = train_data)
# summary(model.mlr)
# performance on training set
prediction <- predict(model.mlr, train_data)
cor(train_y, prediction)

# performance on testing set
prediction <- predict(model.mlr, test_data)
cor(test_y, prediction)


# Build Model, RF
model.rf <- randomForest(as.formula(label), data = train_data) 

# performance on training set
prediction <- predict(model.rf, train_data)
cor(train_y, prediction)

# performance on testing set
prediction <- predict(model.rf, test_data)
cor(test_y, prediction)


## mean
a <- predict(model.mlr, train_data)
b <- predict(model.rf, train_data)
MutaBindS <- ( a + b ) / 2
cor(train_y, MutaBindS)
sqrt(mean((train_y - MutaBindS)^2, na.rm = TRUE))   # RMSE

a <- predict(model.mlr, test_data)
b <- predict(model.rf, test_data)
MutaBindS <- ( a + b ) / 2
cor(test_y, MutaBindS)
sqrt(mean((test_y - MutaBindS)^2, na.rm = TRUE))   # RMSE







