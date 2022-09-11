
### GLMNET alpha and lambda ###

# libraries

library(caret)
library(glmnet)
library(mlbench)
library(psych)
library(ggplot2)

# the data

load("C:/Ordenador_German/MASTER_OMICS/TFM/Para Fran/16 Agosto/RScoreNutrients.RData")

#alpha<-read.csv("C:/Ordenador_German/MASTER_OMICS/TFM/alphadiversSigCasesControlsCOMPLETE.csv")

#alpha<-alpha[alpha$subject_id %in% rownames(Foods.i),]

#FoodGramsPM.Group<-read.csv("C:/Ordenador_German/MASTER_OMICS/TFM/Section Diet/SCORES/020822_FoodGramsPMGroup.csv")

#Food.groups<-FoodGramsPM.Group[,c(2,87:126)]

#rm(FoodGramsPM, FoodGramsPM.Group)

#Food.groups.i<-Food.groups[Food.groups$subject %in% rownames(Foods.i),]

#identical(rownames(Foods.i), as.character(Food.groups.i$subject))

#some<-as.data.frame(Foods.i[,c(83:89)])

#Food.groups.i<-cbind(Food.groups.i, some)

#rm(some)

#identical(as.character(alpha$subject_id), rownames(Foods.i))

#Y2<-alpha$Richness

#Y3<-alpha$exp.Shannon.

#save.image("C:/Ordenador_German/MASTER_OMICS/TFM/Para Fran/16 Agosto/RScoreNutrients.RData")

# joing Y with X
# For nutrients:

X<-Nutrients.i[,-1]
data<-cbind(Y3, X)

str(data)

pairs.panels(X[,c(1:15)], cex.cor = 2)
pairs.panels(X[,c(16:30)], cex.cor = 2)
pairs.panels(X[,c(31:46)], cex.cor = 2)

# data partition:

set.seed(222)
ind<-sample(2, nrow(data), replace=TRUE, prob = c(0.7, 0.3))
train<-data[ind==1,]

test<-data[ind==2,]

# custom control parameters:

custom<-trainControl(method="repeatedcv",
                     number=10,
                     repeats=5,
                     verboseIter = TRUE)

#######################################
# linear model

set.seed(1234)
lm<-train(Y3~.,
          train,
          method="lm",
          trControl=custom)

# results
lm$results
lm
summary(lm)
plot(lm$finalModel)


#######################################
# ridge regression

set.seed(1234)
ridge<-train(Y3 ~ ., 
             train,
             method="glmnet", 
             tuneGrid=expand.grid(alpha=0, 
                               lambda=seq(0.0001, 1, length=5)),
             trControl=custom)
#best value of lambda is 1; the penalty is increased

# plot results
plot(ridge)
ridge
plot(ridge$finalModel, xvar="lambda", label=T)
plot(ridge$finalModel, xvar="dev", label=T)
plot(varImp(ridge, scale=F))
plot(varImp(ridge, scale=T))


#######################################
# lasso regression

set.seed(1234)
lasso<-train(Y3 ~ ., 
             train,
             method="glmnet", 
             tuneGrid=expand.grid(alpha=1, 
                                  lambda=seq(0.0001, 1, length=5)),
             trControl=custom)
#best value of lambda is 1; the penalty is increased

# plot results
plot(lasso)
lasso
plot(lasso$finalModel, xvar="lambda", label=T)
plot(lasso$finalModel, xvar="dev", label=T)
plot(varImp(lasso, scale=F))
plot(varImp(lasso, scale=T))


# must somewhere between 0 and 0.01
set.seed(1234)
lasso<-train(Y3 ~ ., 
             train,
             method="glmnet", 
             tuneGrid=expand.grid(alpha=1, 
                                  lambda=seq(0.0001, 0.01, length=5)),
             trControl=custom)

#best value of lambda is 0.5; the penalty is increased

# plot results
plot(lasso)
lasso
plot(lasso$finalModel, xvar="lambda", label=T)
plot(lasso$finalModel, xvar="dev", label=T)
plot(varImp(lasso, scale=F))
plot(varImp(lasso, scale=T))


#######################################
# enet regression

set.seed(1234)
en<-train(Y3 ~ ., 
             train,
             method="glmnet", 
             tuneGrid=expand.grid(alpha=seq(0,1, length=10), 
                                  lambda=seq(0.0001, 0.01, length=5)),
             trControl=custom)
#Fitting alpha = 0.111, lambda = 1 on full training se


# plot results
plot(en)
en
plot(en$finalModel, xvar="lambda", label=T)
plot(en$finalModel, xvar="dev", label=T)
plot(varImp(en, scale=F))
plot(varImp(en, scale=T))



###########################################
# compare the models

model_list<-list(LinearModel=lm, Ridge=ridge, LASSO=lasso, ENET=en)
res<-resamples(model_list)
res
summary(res)
bwplot(res)
xyplot(res, metric = "RMSE")
#model_list<-list(Ridge=ridge, LASSO=lasso, ENET=en)
#res<-resamples(model_list)
#xyplot(res, metric = "RMSE")
#model_list<-list(Ridge=ridge, ENET=en)
#res<-resamples(model_list)
#xyplot(res, metric = "RMSE")


# the lowest RMSE is for ridge regression:0.7791. So this is the method we need to use.
# the R sqared, % of variance explained, reaches the highest value with ridge regression: 24%
# plots also show that ridge regression is more appropriate: always more appropriate than LM



best<-ridge$finalModel
coef(best, s=ridge$bestTune$lambda)
#model coefficients at that value of ??

#save final model

saveRDS(ridge, "C:/Ordenador_German/MASTER_OMICS/TFM/final_model.rds")

fm<-readRDS("C:/Ordenador_German/MASTER_OMICS/TFM/final_model.rds")

# prediction:

p1<-predict(fm, train)
sqrt(mean((train$medv)^2))

p2<-predict(fm, test)
sqrt(mean((test$medv)^2))


# run again on all the data, and then predict:


X<-as.matrix(X)

predict(fm, newx = X[1:5,], s=ridge$bestTune$lambda)




#####################################################
#####################################################
# for foods:


X<-Foods.i
data<-cbind(Y3, X)

str(data)

pairs.panels(X[,c(1:15)], cex.cor = 2)
pairs.panels(X[,c(16:30)], cex.cor = 2)
pairs.panels(X[,c(31:46)], cex.cor = 2)

# data partition:

set.seed(222)
ind<-sample(2, nrow(data), replace=TRUE, prob = c(0.7, 0.3))
train<-data[ind==1,]


test<-data[ind==2,]

# custom control parameters:

custom<-trainControl(method="repeatedcv",
                     number=10,
                     repeats=5,
                     verboseIter = TRUE)

#######################################
# linear model

set.seed(1234)
lm<-train(Y3~.,
          train,
          method="lm",
          trControl=custom)

# results
lm$results
lm
summary(lm)
plot(lm$finalModel)

#######################################
# ridge regression

set.seed(1234)
ridge<-train(Y3 ~ ., 
             train,
             method="glmnet", 
             tuneGrid=expand.grid(alpha=0, 
                                  lambda=seq(0.0001, 1, length=5)),
             trControl=custom)
#best value of lambda is 1; the penalty is increased

# plot results
plot(ridge)
ridge
plot(ridge$finalModel, xvar="lambda", label=T)
plot(ridge$finalModel, xvar="dev", label=T)
plot(varImp(ridge, scale=F))
plot(varImp(ridge, scale=T))



#######################################
# lasso regression

set.seed(1234)
lasso<-train(Y3 ~ ., 
             train,
             method="glmnet", 
             tuneGrid=expand.grid(alpha=1, 
                                  lambda=seq(0.0001, 1, length=5)),
             trControl=custom)
#best value of lambda is 1; the penalty is increased

# plot results
plot(lasso)
lasso
plot(lasso$finalModel, xvar="lambda", label=T)
plot(lasso$finalModel, xvar="dev", label=T)
plot(varImp(lasso, scale=F))
plot(varImp(lasso, scale=T))


# must somewhere between 0 and 0.01
set.seed(1234)
lasso<-train(Y3 ~ ., 
             train,
             method="glmnet", 
             tuneGrid=expand.grid(alpha=1, 
                                  lambda=seq(0.0001, 0.01, length=5)),
             trControl=custom)

#best value of lambda is 0.01; the penalty is increased

# plot results
plot(lasso)
lasso
plot(lasso$finalModel, xvar="lambda", label=T)
plot(lasso$finalModel, xvar="dev", label=T)
plot(varImp(lasso, scale=F))
plot(varImp(lasso, scale=T))



#######################################
# enet regression

set.seed(1234)
en<-train(Y3 ~ ., 
          train,
          method="glmnet", 
          tuneGrid=expand.grid(alpha=seq(0,1, length=10), 
                               lambda=seq(0.0001, 0.01, length=5)),
          trControl=custom)
#Fitting alpha = 0.111, lambda = 1 on full training se


# plot results
plot(en)
en
plot(en$finalModel, xvar="lambda", label=T)
plot(en$finalModel, xvar="dev", label=T)
plot(varImp(en, scale=F))
plot(varImp(en, scale=T))



###########################################
# compare the models

model_list<-list(LinearModel=lm, Ridge=ridge, LASSO=lasso, ENET=en)
res<-resamples(model_list)
res
summary(res)
bwplot(res)
xyplot(res, metric = "RMSE")



# best model:
ridge$bestTune
#alpha lambda
#    0      1
lasso$bestTune
#alpha lambda
#     1   0.01
en$bestTune
#alpha lambda
#     1   0.01

best<-ridge$finalModel
coef(best, s=ridge$bestTune$lambda)


best<-en$finalModel
coef(best, s=en$bestTune$lambda)


# For FOOD GROUPS



X<-Food.groups.i[,-1]
data<-cbind(Y3, X)

str(data)

pairs.panels(X[,c(1:15)], cex.cor = 2)
pairs.panels(X[,c(16:30)], cex.cor = 2)
pairs.panels(X[,c(31:46)], cex.cor = 2)

# data partition:

set.seed(222)
ind<-sample(2, nrow(data), replace=TRUE, prob = c(0.7, 0.3))
train<-data[ind==1,]

test<-data[ind==2,]

# custom control parameters:

custom<-trainControl(method="repeatedcv",
                     number=10,
                     repeats=5,
                     verboseIter = TRUE)

#######################################
# linear model

set.seed(1234)
lm<-train(Y3~.,
          train,
          method="lm",
          trControl=custom)

# results
lm$results
lm
summary(lm)
plot(lm$finalModel)


#######################################
# ridge regression

set.seed(1234)
ridge<-train(Y3 ~ ., 
             train,
             method="glmnet", 
             tuneGrid=expand.grid(alpha=0, 
                                  lambda=seq(0.0001, 1, length=5)),
             trControl=custom)
#best value of lambda is 1; the penalty is increased

# plot results
plot(ridge)
ridge
plot(ridge$finalModel, xvar="lambda", label=T)
plot(ridge$finalModel, xvar="dev", label=T)
plot(varImp(ridge, scale=F))
plot(varImp(ridge, scale=T))


#######################################
# lasso regression

set.seed(1234)
lasso<-train(Y3 ~ ., 
             train,
             method="glmnet", 
             tuneGrid=expand.grid(alpha=1, 
                                  lambda=seq(0.0001, 1, length=5)),
             trControl=custom)
#best value of lambda is 1; the penalty is increased

# plot results
plot(lasso)
lasso
plot(lasso$finalModel, xvar="lambda", label=T)
plot(lasso$finalModel, xvar="dev", label=T)
plot(varImp(lasso, scale=F))
plot(varImp(lasso, scale=T))


# must somewhere between 0 and 0.01
set.seed(1234)
lasso<-train(Y3 ~ ., 
             train,
             method="glmnet", 
             tuneGrid=expand.grid(alpha=1, 
                                  lambda=seq(0.0001, 0.01, length=5)),
             trControl=custom)

#best value of lambda is 0.5; the penalty is increased

# plot results
plot(lasso)
lasso
plot(lasso$finalModel, xvar="lambda", label=T)
plot(lasso$finalModel, xvar="dev", label=T)
plot(varImp(lasso, scale=F))
plot(varImp(lasso, scale=T))


#######################################
# enet regression

set.seed(1234)
en<-train(Y3 ~ ., 
          train,
          method="glmnet", 
          tuneGrid=expand.grid(alpha=seq(0,1, length=10), 
                               lambda=seq(0.0001, 0.01, length=5)),
          trControl=custom)
#Fitting alpha = 0.111, lambda = 1 on full training se


# plot results
plot(en)
en
plot(en$finalModel, xvar="lambda", label=T)
plot(en$finalModel, xvar="dev", label=T)
plot(varImp(en, scale=F))
plot(varImp(en, scale=T))



###########################################
# compare the models

model_list<-list(LinearModel=lm, Ridge=ridge, LASSO=lasso, ENET=en)
res<-resamples(model_list)
res
summary(res)
bwplot(res)
xyplot(res, metric = "RMSE")
#model_list<-list(Ridge=ridge, LASSO=lasso, ENET=en)
#res<-resamples(model_list)
#xyplot(res, metric = "RMSE")
#model_list<-list(Ridge=ridge, ENET=en)
#res<-resamples(model_list)
#xyplot(res, metric = "RMSE")


# the lowest RMSE is for ridge regression:0.7791. So this is the method we need to use.
# the R sqared, % of variance explained, reaches the highest value with ridge regression: 24%
# plots also show that ridge regression is more appropriate: always more appropriate than LM



best<-ridge$finalModel
coef(best, s=ridge$bestTune$lambda)


# some descriptives:

micro<-meta[meta$MetaG==1,]
pangen<-meta[meta$MetaG==0,]

micro<-micro[,c(2:38)]
pangen<-pangen[,c(2:38)]

str(micro)

micro2<-as.data.frame(lapply(micro, as.factor))
micro2$agec<-micro$agec
micro2$cpy1<-micro$cpy1

res<-compareGroups(casecontrol~., data=micro2, ref.no="no", method=1, include.miss = TRUE, compute.ratio = FALSE)
restab<-createTable(res, show.ratio = FALSE, hide.no = "n")
restab


pangen2<-as.data.frame(lapply(pangen, as.factor))
pangen2$agec<-pangen$agec
pangen2$cpy1<-pangen$cpy1

res<-compareGroups(casecontrol~., data=pangen2, ref.no="no", method=1, include.miss = TRUE, compute.ratio = FALSE)
restab<-createTable(res, show.ratio = FALSE, hide.no = "n")
restab

##########################################################################################################
### Parameter optimization and selection of variables for the diet scores with the microbial species: oral and gut, for the most prevalent and the signature in gut
## y needs to the scores: DRRD and rMED
## X needs to be the microbial species


diet.scores<-read.csv("C:/Ordenador_German/MASTER_OMICS/TFM/Section Diet/SCORES/020822_scores.csv")

# extract the most prevalent taxa

load("C:/Ordenador_German/MASTER_OMICS/TFM/Lasta MetaG data/metag.pc.metadata.imputed.RData")

#load("C:/Ordenador_German/MASTER_OMICS/TFM/Lasta MetaG data/metag.pc.metadata.RData")

#load("C:/Ordenador_German/MASTER_OMICS/TFM/Lasta MetaG data/data.table.metag.RData")

#rm(motu.abs.3mg)

rarefiedOR.metaG<-read.csv("C:/Ordenador_German/MASTER_OMICS/TFM/Lasta MetaG data/RarefiedORMetaG.csv", header=TRUE)
rarefiedST.metaG<-read.csv("C:/Ordenador_German/MASTER_OMICS/TFM/Lasta MetaG data/RarefiedSTMetaG.csv", header=TRUE)

#load("C:/Ordenador_German/MASTER_OMICS/TFM/Lasta MetaG data/metag.alphadiv.RData")

# obtain relative frequencies FOR STOOL SAMPLES

#Transpose the data to have sample names on rows
dim(rarefiedST.metaG)
rownames(rarefiedST.metaG)<-rarefiedST.metaG$X
rarefiedST.metaG$X<-NULL

abund_table<-t(rarefiedST.metaG)
dim(abund_table)
str(abund_table)

#Apply proportion normalisation
x<-abund_table/rowSums(abund_table)

#x<-x[rownames(stool),]
x<-x[,order(colSums(x),decreasing=TRUE)]

# need to have X as a matrix where subjects are rownames and species are in columns

#rm(X)

#x<-x[rownames(oral),]
#x<-x[,order(colSums(x),decreasing=TRUE)]


#Extract list of top N Taxa
N<-49
taxa_list<-colnames(x)[1:N]

#remove "__Unknown__" and add it to others
taxa_list<-taxa_list[!grepl("Unknown",taxa_list)]
N<-length(taxa_list)

#Generate a new table with everything added to Others
#new_x<-data.frame(x[,colnames(x) %in% taxa_list])
new_x<-data.frame(x[,colnames(x) %in% taxa_list],Others=rowSums(x[,!colnames(x) %in% taxa_list]))


# new_x will be X, but need to make sure that samples are well ordered with diet.scores
dim(new_x)

# introduce sample alias in diet.scores from metag

metag<-metag.imputed[,c("sample_alias", "subject_id")]

diet.scores<-merge(diet.scores, metag, by.x="subject", by.y="subject_id")

# retain in diet.scores only samples of new_x

diet.scores<-diet.scores[diet.scores$sample_alias %in% rownames(new_x),]

new_x<-new_x[rownames(new_x) %in% diet.scores$sample_alias,]

identical(as.character(diet.scores$sample_alias), rownames(new_x))

X<-as.matrix(new_x)

Y4<-diet.scores$DRRDSCORE
Y5<-diet.scores$MDSCORE

## Start FEATURE SELECTION:

data<-cbind(Y4, X)

str(data)

# data partition:

set.seed(222)
ind<-sample(2, nrow(data), replace=TRUE, prob = c(0.7, 0.3))
train<-data[ind==1,]

test<-data[ind==2,]

# custom control parameters:

custom<-trainControl(method="repeatedcv",
                     number=10,
                     repeats=5,
                     verboseIter = TRUE)

#######################################
# linear model

set.seed(1234)
lm<-train(Y4~.,
          train,
          method="lm",
          trControl=custom)

# results
lm$results
lm
summary(lm)
plot(lm$finalModel)

Results<-summary(lm)
Results<-Results[[4]]
plot(lm$finalModel)

p.bh<-p.adjust(Results[,4], "BH") #False discovery rate (Benjamini Hochberg)
Results.adj=data.frame(cbind(Results,p.bh))
Results.adj


#######################################
# ridge regression

set.seed(1234)
ridge<-train(Y4 ~ ., 
             train,
             method="glmnet", 
             tuneGrid=expand.grid(alpha=0, 
                                  lambda=seq(0.0001, 1, length=5)),
             trControl=custom)
#best value of lambda is 1 and alpha 0; the penalty is increased

# plot results
plot(ridge)
ridge
plot(ridge$finalModel, xvar="lambda", label=T)
plot(ridge$finalModel, xvar="dev", label=T)
plot(varImp(ridge, scale=F))
plot(varImp(ridge, scale=T))


#######################################
# lasso regression

set.seed(1234)
lasso<-train(Y4 ~ ., 
             train,
             method="glmnet", 
             tuneGrid=expand.grid(alpha=1, 
                                  lambda=seq(0.0001, 1, length=5)),
             trControl=custom)
#best value of lambda is 1; lambda=0.5 the penalty is increased

# plot results
plot(lasso)
lasso
plot(lasso$finalModel, xvar="lambda", label=T)
plot(lasso$finalModel, xvar="dev", label=T)
plot(varImp(lasso, scale=F))
plot(varImp(lasso, scale=T))



#######################################
# enet regression

set.seed(1234)
en<-train(Y4 ~ ., 
          train,
          method="glmnet", 
          tuneGrid=expand.grid(alpha=seq(0,1, length=10), 
                               lambda=seq(0.0001, 0.01, length=5)),
          trControl=custom)
#Fitting alpha = 0, lambda = 0.01 on full training se


# plot results
plot(en)
en
plot(en$finalModel, xvar="lambda", label=T)
plot(en$finalModel, xvar="dev", label=T)
plot(varImp(en, scale=F))
plot(varImp(en, scale=T))



###########################################
# compare the models

model_list<-list(LinearModel=lm, Ridge=ridge, LASSO=lasso, ENET=en)
res<-resamples(model_list)
res
summary(res)
bwplot(res)
xyplot(res, metric = "RMSE")
#model_list<-list(Ridge=ridge, LASSO=lasso, ENET=en)
#res<-resamples(model_list)
#xyplot(res, metric = "RMSE")
#model_list<-list(Ridge=ridge, ENET=en)
#res<-resamples(model_list)
#xyplot(res, metric = "RMSE")


# the lowest RMSE is for ridge regression:0.7791. So this is the method we need to use.
# the R sqared, % of variance explained, reaches the highest value with ridge regression: 24%
# plots also show that ridge regression is more appropriate: always more appropriate than LM



best<-ridge$finalModel
coef(best, s=ridge$bestTune$lambda)
#model coefficients at that value of ??

best<-lasso$finalModel
coef(best, s=lasso$bestTune$lambda)
#model coefficients at that value of ??


#save final model

saveRDS(ridge, "C:/Ordenador_German/MASTER_OMICS/TFM/final_model DRRD.rds")

fm<-readRDS("C:/Ordenador_German/MASTER_OMICS/TFM/final_model DRRD.rds")

# prediction:

p1<-predict(fm, train)
sqrt(mean((train$medv)^2))

p2<-predict(fm, test)
sqrt(mean((test$medv)^2))


# run again on all the data, and then predict:


X<-as.matrix(X)

predict(fm, newx = X[1:5,], s=ridge$bestTune$lambda)


## same for the rMED score:


data<-cbind(Y5, X)

str(data)

# data partition:

set.seed(222)
ind<-sample(2, nrow(data), replace=TRUE, prob = c(0.7, 0.3))
train<-data[ind==1,]

test<-data[ind==2,]

# custom control parameters:

custom<-trainControl(method="repeatedcv",
                     number=10,
                     repeats=5,
                     verboseIter = TRUE)


#######################################
# linear model

set.seed(1234)
lm<-train(Y5~.,
          train,
          method="lm",
          trControl=custom)

# results
lm$results
lm
Results<-summary(lm)
Results<-Results[[4]]
plot(lm$finalModel)

p.bh<-p.adjust(Results[,4], "BH") #False discovery rate (Benjamini Hochberg)
Results.adj=data.frame(cbind(Results,p.bh))
Results.adj


#######################################
# ridge regression

set.seed(1234)
ridge<-train(Y5 ~ ., 
             train,
             method="glmnet", 
             tuneGrid=expand.grid(alpha=0, 
                                  lambda=seq(0.0001, 1, length=5)),
             trControl=custom)
#best value of lambda is 1 and alpha 0; the penalty is increased

# plot results
plot(ridge)
ridge
plot(ridge$finalModel, xvar="lambda", label=T)
plot(ridge$finalModel, xvar="dev", label=T)
plot(varImp(ridge, scale=F))
plot(varImp(ridge, scale=T))


#######################################
# lasso regression

set.seed(1234)
lasso<-train(Y5 ~ ., 
             train,
             method="glmnet", 
             tuneGrid=expand.grid(alpha=1, 
                                  lambda=seq(0.0001, 1, length=5)),
             trControl=custom)
#best value of lambda is 1; lambda=1 the penalty is increased

# plot results
plot(lasso)
lasso
plot(lasso$finalModel, xvar="lambda", label=T)
plot(lasso$finalModel, xvar="dev", label=T)
plot(varImp(lasso, scale=F))
plot(varImp(lasso, scale=T))


# must somewhere between 0 and 0.01
set.seed(1234)
lasso<-train(Y5 ~ ., 
             train,
             method="glmnet", 
             tuneGrid=expand.grid(alpha=1, 
                                  lambda=seq(0.0001, 0.02, length=5)),
             trControl=custom)

#best value of lambda is 0.5; the penalty is increased

# plot results
plot(lasso)
lasso
plot(lasso$finalModel, xvar="lambda", label=T)
plot(lasso$finalModel, xvar="dev", label=T)
plot(varImp(lasso, scale=F))
plot(varImp(lasso, scale=T))




#######################################
# enet regression

set.seed(1234)
en<-train(Y5 ~ ., 
          train,
          method="glmnet", 
          tuneGrid=expand.grid(alpha=seq(0,1, length=10), 
                               lambda=seq(0.0001, 0.01, length=5)),
          trControl=custom)
#Fitting alpha = 0, lambda = 0.01 on full training se


# plot results
plot(en)
en
plot(en$finalModel, xvar="lambda", label=T)
plot(en$finalModel, xvar="dev", label=T)
plot(varImp(en, scale=F))
plot(varImp(en, scale=T))



###########################################
# compare the models

model_list<-list(LinearModel=lm, Ridge=ridge, LASSO=lasso, ENET=en)
res<-resamples(model_list)
res
summary(res)
bwplot(res)
xyplot(res, metric = "RMSE")
#model_list<-list(Ridge=ridge, LASSO=lasso, ENET=en)
#res<-resamples(model_list)
#xyplot(res, metric = "RMSE")
#model_list<-list(Ridge=ridge, ENET=en)
#res<-resamples(model_list)
#xyplot(res, metric = "RMSE")



best<-lasso$finalModel
coef(best, s=lasso$bestTune$lambda)


best<-ridge$finalModel
coef(best, s=ridge$bestTune$lambda)


#save final model

saveRDS(ridge, "C:/Ordenador_German/MASTER_OMICS/TFM/final_model rMED.rds")

fm<-readRDS("C:/Ordenador_German/MASTER_OMICS/TFM/final_model rMED.rds")


##############################################################
## Microbiome signature:


# the 27 species of the signature:

species<-c("Methanobrevibacter smithii [r_03695]", "Veillonella atypica [r_01941]", "Firmicutes sp. [r_03641]", "Clostridium sp. [r_03622]", "Bacteroides finegoldii [r_03474]", "Firmicutes sp. [r_03629]", "bacterium LF-3 [r_03628]", "Alloscardovia omnicolens [r_02114]", "Prevotella species  [m_12780]", "Veillonella species  [m_13135]", "Butyrivibrio crossotus [r_03686]", "Clostridiales species  [m_13012]", "Megamonas funiformis/rupellensis [r_02318]", "Holdemanella biformis [m_12329]", "Dorea sp. CAG:317 [r_07668]", "Bifidobacterium ruminantium [r_02702]", "Bacteroides caecimuris [r_03476]", "Bacteroides sp. CAG:144 [m_12596]", "Faecalibacterium species  [m_12403]", "Rikenellaceae sp. [r_03593]", "Paraprevotella clara [r_03698]", "Clostridium sp. CAG:217 [m_12270]", "[Eubacterium] rectale [r_03657]", "Bacteroides coprocola [r_11279]", "Faecalibacterium prausnitzii [r_06110]", "Bifidobacterium bifidum [r_03116]", "Romboutsia timonensis [r_09389]")

diet.scores<-read.csv("C:/Ordenador_German/MASTER_OMICS/TFM/Section Diet/SCORES/020822_scores.csv")

#grep("09389", colnames(x))

#aa<-x[,c(195,235)]

# extract those species from the data:

x<-abund_table/rowSums(abund_table)


#Generate a new table with everything added to Others
new_x<-data.frame(x[,colnames(x) %in% species])

# now we need to select the cases and controls, and add the dietary and metadata information

dim(new_x)
# rows are subjects

# introduce sample alias in diet.scores from metag

metag<-metag.imputed[,c("sample_alias", "subject_id")]

diet.scores<-merge(diet.scores, metag, by.x="subject", by.y="subject_id")

# retain in diet.scores only samples of new_x

diet.scores<-diet.scores[diet.scores$sample_alias %in% rownames(new_x),]

new_x<-new_x[rownames(new_x) %in% diet.scores$sample_alias,]

identical(as.character(diet.scores$sample_alias), rownames(new_x))

X<-as.matrix(new_x)

# everything prepared:

Y4<-diet.scores$DRRDSCORE
Y5<-diet.scores$MDSCORE


## Start FEATURE SELECTION:

data<-cbind(Y4, X)

str(data)

# data partition:

set.seed(222)
ind<-sample(2, nrow(data), replace=TRUE, prob = c(0.7, 0.3))
train<-data[ind==1,]

test<-data[ind==2,]

# custom control parameters:

custom<-trainControl(method="repeatedcv",
                     number=10,
                     repeats=5,
                     verboseIter = TRUE)

#######################################
# linear model

set.seed(1234)
lm<-train(Y4~.,
          train,
          method="lm",
          trControl=custom)

# results
lm$results
lm
summary(lm)
plot(lm$finalModel)

Results<-summary(lm)
Results<-Results[[4]]
plot(lm$finalModel)

p.bh<-p.adjust(Results[,4], "BH") #False discovery rate (Benjamini Hochberg)
Results.adj=data.frame(cbind(Results,p.bh))
Results.adj


#######################################
# ridge regression

set.seed(1234)
ridge<-train(Y4 ~ ., 
             train,
             method="glmnet", 
             tuneGrid=expand.grid(alpha=0, 
                                  lambda=seq(0.0001, 1, length=5)),
             trControl=custom)
#best value of lambda is 1 and alpha 0; the penalty is increased

# plot results
plot(ridge)
ridge
plot(ridge$finalModel, xvar="lambda", label=T)
plot(ridge$finalModel, xvar="dev", label=T)
plot(varImp(ridge, scale=F))
plot(varImp(ridge, scale=T))


#######################################
# lasso regression

set.seed(1234)
lasso<-train(Y4 ~ ., 
             train,
             method="glmnet", 
             tuneGrid=expand.grid(alpha=1, 
                                  lambda=seq(0.0001, 1, length=5)),
             trControl=custom)
#best value of lambda is 1; lambda=0.5 the penalty is increased

# plot results
plot(lasso)
lasso
plot(lasso$finalModel, xvar="lambda", label=T)
plot(lasso$finalModel, xvar="dev", label=T)
plot(varImp(lasso, scale=F))
plot(varImp(lasso, scale=T))



#######################################
# enet regression

set.seed(1234)
en<-train(Y4 ~ ., 
          train,
          method="glmnet", 
          tuneGrid=expand.grid(alpha=seq(0,1, length=10), 
                               lambda=seq(0.0001, 0.01, length=5)),
          trControl=custom)
#Fitting alpha = 0, lambda = 0.01 on full training se


# plot results
plot(en)
en
plot(en$finalModel, xvar="lambda", label=T)
plot(en$finalModel, xvar="dev", label=T)
plot(varImp(en, scale=F))
plot(varImp(en, scale=T))



###########################################
# compare the models

model_list<-list(LinearModel=lm, Ridge=ridge, LASSO=lasso, ENET=en)
res<-resamples(model_list)
res
summary(res)
bwplot(res)
xyplot(res, metric = "RMSE")
#model_list<-list(Ridge=ridge, LASSO=lasso, ENET=en)
#res<-resamples(model_list)
#xyplot(res, metric = "RMSE")
#model_list<-list(Ridge=ridge, ENET=en)
#res<-resamples(model_list)
#xyplot(res, metric = "RMSE")


# the lowest RMSE is for ridge regression:0.7791. So this is the method we need to use.
# the R sqared, % of variance explained, reaches the highest value with ridge regression: 24%
# plots also show that ridge regression is more appropriate: always more appropriate than LM



best<-ridge$finalModel
coef(best, s=ridge$bestTune$lambda)
#model coefficients at that value of ??

best<-lasso$finalModel
coef(best, s=lasso$bestTune$lambda)
#model coefficients at that value of ??


#save final model

saveRDS(ridge, "C:/Ordenador_German/MASTER_OMICS/TFM/final_model DRRD sig.rds")

fm<-readRDS("C:/Ordenador_German/MASTER_OMICS/TFM/final_model DRRD sig.rds")

# prediction:

p1<-predict(fm, train)
sqrt(mean((train$medv)^2))

p2<-predict(fm, test)
sqrt(mean((test$medv)^2))


# run again on all the data, and then predict:


X<-as.matrix(X)

predict(fm, newx = X[1:5,], s=ridge$bestTune$lambda)


## same for the rMED score:


data<-cbind(Y5, X)

str(data)

# data partition:

set.seed(222)
ind<-sample(2, nrow(data), replace=TRUE, prob = c(0.7, 0.3))
train<-data[ind==1,]

test<-data[ind==2,]

# custom control parameters:

custom<-trainControl(method="repeatedcv",
                     number=10,
                     repeats=5,
                     verboseIter = TRUE)


#######################################
# linear model

set.seed(1234)
lm<-train(Y5~.,
          train,
          method="lm",
          trControl=custom)

# results
lm$results
lm
Results<-summary(lm)
Results<-Results[[4]]
plot(lm$finalModel)

p.bh<-p.adjust(Results[,4], "BH") #False discovery rate (Benjamini Hochberg)
Results.adj=data.frame(cbind(Results,p.bh))
Results.adj


#######################################
# ridge regression

set.seed(1234)
ridge<-train(Y5 ~ ., 
             train,
             method="glmnet", 
             tuneGrid=expand.grid(alpha=0, 
                                  lambda=seq(0.0001, 1, length=5)),
             trControl=custom)
#best value of lambda is 1 and alpha 0; the penalty is increased

# plot results
plot(ridge)
ridge
plot(ridge$finalModel, xvar="lambda", label=T)
plot(ridge$finalModel, xvar="dev", label=T)
plot(varImp(ridge, scale=F))
plot(varImp(ridge, scale=T))


#######################################
# lasso regression

set.seed(1234)
lasso<-train(Y5 ~ ., 
             train,
             method="glmnet", 
             tuneGrid=expand.grid(alpha=1, 
                                  lambda=seq(0.0001, 1, length=5)),
             trControl=custom)
#best value of lambda is 1; lambda=1 the penalty is increased

# plot results
plot(lasso)
lasso
plot(lasso$finalModel, xvar="lambda", label=T)
plot(lasso$finalModel, xvar="dev", label=T)
plot(varImp(lasso, scale=F))
plot(varImp(lasso, scale=T))


# must somewhere between 0 and 0.01
set.seed(1234)
lasso<-train(Y5 ~ ., 
             train,
             method="glmnet", 
             tuneGrid=expand.grid(alpha=1, 
                                  lambda=seq(0.0001, 0.02, length=5)),
             trControl=custom)

#best value of lambda is 0.5; the penalty is increased

# plot results
plot(lasso)
lasso
plot(lasso$finalModel, xvar="lambda", label=T)
plot(lasso$finalModel, xvar="dev", label=T)
plot(varImp(lasso, scale=F))
plot(varImp(lasso, scale=T))




#######################################
# enet regression

set.seed(1234)
en<-train(Y5 ~ ., 
          train,
          method="glmnet", 
          tuneGrid=expand.grid(alpha=seq(0,1, length=10), 
                               lambda=seq(0.0001, 1, length=5)),
          trControl=custom)
#Fitting alpha = 0, lambda = 0.01 on full training se


# plot results
plot(en)
en
plot(en$finalModel, xvar="lambda", label=T)
plot(en$finalModel, xvar="dev", label=T)
plot(varImp(en, scale=F))
plot(varImp(en, scale=T))



###########################################
# compare the models

model_list<-list(LinearModel=lm, Ridge=ridge, LASSO=lasso, ENET=en)
res<-resamples(model_list)
res
summary(res)
bwplot(res)
xyplot(res, metric = "RMSE")
#model_list<-list(Ridge=ridge, LASSO=lasso, ENET=en)
#res<-resamples(model_list)
#xyplot(res, metric = "RMSE")
#model_list<-list(Ridge=ridge, ENET=en)
#res<-resamples(model_list)
#xyplot(res, metric = "RMSE")



best<-lasso$finalModel
coef(best, s=lasso$bestTune$lambda)


best<-ridge$finalModel
coef(best, s=ridge$bestTune$lambda)


#save final model

saveRDS(ridge, "C:/Ordenador_German/MASTER_OMICS/TFM/final_model rMED sig.rds")

fm<-readRDS("C:/Ordenador_German/MASTER_OMICS/TFM/final_model rMED sig.rds")


