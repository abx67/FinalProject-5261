###############################################################
############# Apply xgboost to cluster result #################
###############################################################

# load functions
source('./lib/train.R')
source('./lib/test.R')
source('./lib/cross_validation.R')

#################################################################################

# load all data
data_all <- t(data_mat_norm)
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
                  run.xgboost = F, run.gbm = T,
                  run.adaboost = F,
                  par=NULL,
                  num_class = 2)

test.pred <- test(fit.model, data_test,
                 run.xgboost = F, run.gbm = T,
                 run.adaboost = F, par=NULL)

mean(test.pred == label_test)


cv.result <- cv(data_train ,label_train,
                run.xgboost = T,
                run.gbm = F,
                run.adaboost = F,
                K = 5, 
                par = NULL,
                num_class = 2)
