
library(glmnet)

# set.seed(100)

f <- read.table(file="SkempiS_WithModelFeatures.txt", header = TRUE, sep = '\t')
#rownames(f) <- paste(f$PDB.id,f$Partner1,f$Partner2,f$Mutated.Chain,f$Mutation,f$label_dataset,sep = '_')                # for single mutation
# rownames(f) <- paste(f$PDB.id,f$Partner1,f$Partner2,f$Mutation,f$label_dataset,sep = '_')                              # for multiple mutation
#c(dim(f)[1],length(unique(f$PDB.id)))

label <- 'DDGexp~dE_vdw_wt+dE_vdw_mut+dG_solv_mut+dG_solv_wt+dPro_mut+dPro_wt+SA_com_wt+SA_part_wt+CS+ddG_fold'         # model features of single mutation
# label <- 'DDGexp~dE_vdw_wt+dE_vdw_mut+dG_solv_mut+dG_solv_wt+ddG_fold+CS+ddE_elec_.site.site.+ddrSA'                  # model features of multiple mutation

# print(colnames(f))

data = f[, c("DDGexp", "dE_vdw_wt", "dE_vdw_mut", "dG_solv_mut", "dG_solv_wt", "dPro_mut", "dPro_wt", "SA_com_wt", "SA_part_wt", "CS", "ddG_fold")]
y = data$DDGexp
x = data[, -which(names(data)=="DDGexp")]
###############################

x.matrix <- model.matrix(as.formula(label), data)
rr.cv <- cv.glmnet(x.matrix, y, alpha = 0.5)
rr.bestlam <- rr.cv$lambda.min
# print(rr.cv)
rr.fit <- glmnet(x.matrix, y, alpha = 0.5)
rr.pred <- predict(rr.fit, x.matrix, s = rr.bestlam, type="response")

cor.test(rr.pred, y)

# summary(rr.fit)
# print(rr.fit)
