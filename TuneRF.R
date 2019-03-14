rm(list=ls())

library(car)
library(mlbench)
library(randomForest)
set.seed(100)
library(caret)

f <- read.table(file="SkempiS_WithModelFeatures.txt", header = TRUE, sep = '\t')
rownames(f) <- paste(f$PDB.id,f$Partner1,f$Partner2,f$Mutated.Chain,f$Mutation,f$label_dataset,sep = '_')                # for single mutation
# rownames(f) <- paste(f$PDB.id,f$Partner1,f$Partner2,f$Mutation,f$label_dataset,sep = '_')                              # for multiple mutation
c(dim(f)[1],length(unique(f$PDB.id)))

label <- 'DDGexp~dE_vdw_wt+dE_vdw_mut+dG_solv_mut+dG_solv_wt+dPro_mut+dPro_wt+SA_com_wt+SA_part_wt+CS+ddG_fold'         # model features of single mutation
# label <- 'DDGexp~dE_vdw_wt+dE_vdw_mut+dG_solv_mut+dG_solv_wt+ddG_fold+CS+ddE_elec_.site.site.+ddrSA'                  # model features of multiple mutation

print(colnames(f))

data = f[, c("DDGexp", "dE_vdw_wt", "dE_vdw_mut", "dG_solv_mut", "dG_solv_wt", "dPro_mut", "dPro_wt", "SA_com_wt", "SA_part_wt", "CS", "ddG_fold")]
y = data$DDGexp
x = data[, -which(names(data)=="DDGexp")]

# Build Model, RF with Hyperparameter Optimization
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")

# tunegrid <- expand.grid(.mtry=c(1:11), .ntree=c(100, 500, 1000, 1500, 2000, 2500))
tunegrid <- expand.grid(.mtry=c(1:11))
print(tunegrid)

# model <- train(x=x, y=y, method="rf", trControl=control, tuneGrid=tunegrid)
model <- train(as.formula(label), data = f, method="rf", trControl=control, tuneGrid=tunegrid)

summary(model)
print(model)
# plot(model)

# model.rf <- randomForest(as.formula(label), data = f) 
# f$ddG_pred_rf <- model.rf$predicted
# cor.test(f$DDGexp,f$ddG_pred_rf)

# save model
save(model.rf,file = 'model.opt.rf')
