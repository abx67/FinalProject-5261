###########################################################
############# Apply PCA to cluster result #################
###########################################################

# load functions
source('./lib/train.R')
source('./lib/test.R')
source('./lib/cross_validation.R')

#################################################################################

# compute pca
stock.pca <- prcomp(t(data_mat_norm),scale = T)

# 
pca.var <- stock.pca$sdev^2
pve <- pca.var / sum(pca.var)
sum(head(pve,100))

#################################################################################

# load all data
data_all <- stock.pca$x[,1:100] # using pca1 and pca2
label_all <- kc$cluster-1

# split into train and test
## set seed
set.seed(1)

# total index
ind <- 1:length(label_all)

# test data index
test.ind <- sample(ind, size = round(length(ind)*0.25), replace = FALSE)

# train data index
train.ind <- setdiff(ind,test.ind)

# train label
label_train <- label_all[train.ind]
label_test <- label_all[test.ind]


# train data
data_train <- data_all[train.ind,] # rgb train data
# test data
data_test <- data_all[test.ind,] # rgb train data

#################################################################################

# train model
fit.model <- train(data_train, label_train,
                   run.xgboost = T, run.gbm = F,
                   run.adaboost = F,
                   par=NULL,
                   num_class = 3)

test.pred <- test(fit.model, data_test,
                  run.xgboost = T, run.gbm = F,
                  run.adaboost = F, par=NULL)

mean(test.pred == label_test)


cv.result <- cv(data_train ,label_train,
                run.xgboost = F,
                run.gbm = F,
                run.adaboost = T,
                K = 5, 
                par = NULL,
                num_class = 4)
cv.result
## for 
# $error
# [1] 0.06571372
# 
# $sd
# [1] 0.02931508