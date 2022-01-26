#rm(list = ls())

source("./src/04_dataMart.R", encoding = "UTF-8")



## 검증용
#######################################################################
################## split -> train, test     ########################### 
################## lm   + 다중공선성 확인   ########################### 
#######################################################################


set.seed(123)
proportion = 0.7
idx = sample(1:nrow(train2), size = round(proportion * nrow(train2)))
train2_train = train2[idx, ]
train2_valid = train2[-idx, ]

y_train = train2_train$target
y_test = train2_valid$target

mod.lm1 = lm(target ~., data = train2_train[,-1])
summary(mod.lm1)

# 변수 선택법
mod.lm2 = step(mod.lm1, direction = "both")
summary(mod.lm2)







pred.lm = predict(mod.lm2, train2_valid[,-c(1,ncol(train2_valid))])

out.lm = data.frame(id = train2_valid$id,
                    real = train2_valid$target,
                    pred = pred.lm)


forecast::accuracy(out.lm$real, out.lm$pred)


#######################################################################
############################ RF ####################################### 
#######################################################################


set.seed(123)


mod.rdf = randomForest(target ~ ., data = train2_train[,-1], 
                       ntree=4800,importance=T)

pred.rdf = predict(mod.rdf, train2_valid)


out.rdf = data.frame(id = train2_valid$id,
                     real = train2_valid$target,
                     pred = pred.rdf)


forecast::accuracy(out.rdf$real, out.rdf$pred)

NMAE(out$real, out.rdf$pred)
#######################################################################

#######################################################################
############################ GBM ###################################### 
#######################################################################

# GBM
trainSparse <- sparse.model.matrix(target~. , data = train2_train[,-1])
testSparse <- sparse.model.matrix(target~. , data = train2_valid[,-1])


mod.gbm <- gbm.fit(x = as.matrix(trainSparse), y = y_train, n.trees = 50,
                   shrinkage = 0.1 ,interaction.depth = 3, n.minobsinnode = 10,
                   distribution = "gaussian",bag.fraction = 0.5)

# Predict
pred.gbm <- predict(mod.gbm, newdata = as.matrix(testSparse), n.trees = 50)

out.gbm = data.frame(id = train2_valid$id,
                     real = train2_valid$target,
                     pred = pred.gbm)

forecast::accuracy(out.gbm$real, out.gbm$pred)

NMAE(out$real, out$gbm_pred)
#######################################################################

#######################################################################
############################ XGB ###################################### 
#######################################################################



trainSparse = xgb.DMatrix(data.matrix(train2_train[,-c(1,length(train2_train))]), label=y_train, missing=NA)
testSparse  = xgb.DMatrix(data.matrix(train2_valid[,-c(1,length(train2_valid))]), missing = NA)

foldsCV <- createFolds(y_train, k=20, list=TRUE, returnTrain=FALSE)


set.seed(123)

### 모델링
cat("modeling\n")

param.xgb <- list(subsample = 1
                  , max_depth = 5
                  , colsample_bytree = 0.5
                  , eta = 0.08
                  , eval_metric = 'rmse'#average
                  , min_child_weight = 1.2)



# xgb_cv <- xgb.cv(data=trainSparse,
#                  params=param.xgb,
#                  nrounds=100,
#                  prediction=TRUE,
#                  maximize=TRUE,
#                  folds=foldsCV,
#                  #early_stopping_rounds = 50,
#                  print_every_n = 5
# )

#mod.xgb = xgboost(data = trainSparse, nrounds = 300)
mod.xgb = xgboost(data = trainSparse,
                  eta = 0.05,
                  nfold = 10, 
                  max_depth = 8, 
                  nround = 500, 
                  subsample = 0.8,
                  colsample_bytree = 0.8,
                  eval_metric = 'rmse',
                  verbose = 1)



# 변수 중요도
xgb_imp = xgb.importance(model = mod.xgb)
xgb.ggplot.importance(importance_matrix = xgb_imp)


pred.xgb = predict(mod.xgb,testSparse)

out.xgb = data.frame(id = train2_valid$id,
                     real = train2_valid$target,
                     pred = pred.xgb)


forecast::accuracy(out.xgb$real, out.xgb$pred)



NMAE(out$real, out.xgb$pred)


#######################################################################


#######################################################################
############################ LGB ###################################### 
#######################################################################


train_sparse = Matrix(as.matrix(train2_train[,-c(1,length(train2_train))]), sparse=TRUE)
test_sparse  = Matrix(as.matrix(train2_valid[,-c(1,length(train2_valid))]), sparse=TRUE)

lgb.train = lgb.Dataset(data=train_sparse, label=y_train)


lgb.param = list(objective = "regression", 
                 metric = "rmse", 
                 learning_rate= 0.08,
                 num_leaves= 5,
                 max_depth= 10,
                 min_child_samples= 100,
                 max_bin= 100,
                 subsample= 0.5, 
                 subsample_freq= 1,
                 colsample_bytree= 0.5,
                 min_child_weight= 0,
                 min_split_gain= 0,
                 scale_pos_weight= 99.7)


lgb.normalizedgini = function(preds, dtrain){
  actual = getinfo(dtrain, "label")
  score  = NormalizedGini(preds,actual)
  return(list(name = "gini", value = score, higher_better = TRUE))
}



best.iter = 700

# Train final model
mod.lgb = lgb.train(params = lgb.param, data = lgb.train,
                    num_threads = 10 , nrounds = best.iter,
                    eval_freq = 10, eval = lgb.normalizedgini)


pred.lgb = predict(mod.lgb, test_sparse)

out.lgb = data.frame(id = train2_valid$id,
                     real = train2_valid$target,
                     pred = pred.lgb)

forecast::accuracy(out.lgb$real, out.lgb$pred)

NMAE(out$real, out.lgb$pred)



#######################################################################


#######################################################################
############################ SVR ###################################### 
#######################################################################


paramsvr <- list(gamma = 1e-4, cost = 100, epsilon = 0.001)


trainSparse <- sparse.model.matrix(target~. , data = train2_train[,-1])
testSparse <- sparse.model.matrix(target~. , data = train2_valid[,-1])




mod.svr <- svm(x = as.matrix(trainSparse) , y = y_train, type = "eps-regression",
               kernel = "radial",cost = paramsvr[2], gamma = paramsvr[1], 
               epsilon = paramsvr[3])


pred.svr <- predict(mod.svr, newdata = as.matrix(testSparse)) %>% as.vector()


out.svr = data.frame(id = train2_valid$id,
                     real = train2_valid$target,
                     pred = pred.svr)


forecast::accuracy(out.svr$real, out.svr$pred)



#######################################################################




#############################################
######### 예측 모형 평가지표 종합 ###########
#############################################


## 검증용

colnames(out.lm)[3] = "lm_pred"
colnames(out.rdf)[3] = "rdf_pred"
colnames(out.gbm)[3] = "gbm_pred"
colnames(out.xgb)[3] = "xgb_pred"
colnames(out.lgb)[3] = "lgb_pred"
colnames(out.svr)[3] = "svr_pred"

out = do.call("cbind", list(out.lm,
                            out.rdf,
                            out.gbm,
                            out.xgb,
                            out.lgb,
                            out.svr))
out = out[c(1,2,grep("pred",colnames(out)))]
out[,2:ncol(out)] = exp(out[,2:ncol(out)])-1
# 
# out = out %>% 
#   group_by(id) %>% 
#   summarise(real,
#             lm_pred,
#             rdf_pred,
#             gbm_pred,
#             xgb_pred,
#             lgb_pred,
#             svr_pred,
#             lm_rdf_pred = mean(lm_pred, rdf_pred),
#             lm_gbm_pred = mean(lm_pred, gbm_pred),
#             lm_xgb_pred = mean(lm_pred, xgb_pred),
#             lm_lgb_pred = mean(lm_pred, lgb_pred),
#             lm_svr_pred = mean(lm_pred, svr_pred),
#             rdf_gbm_pred = mean(rdf_pred, gbm_pred),
#             rdf_xgb_pred = mean(rdf_pred, xgb_pred),
#             rdf_lgb_pred = mean(rdf_pred, lgb_pred),
#             rdf_svr_pred = mean(rdf_pred, svr_pred),
#             gbm_xgb_pred = mean(gbm_pred, xgb_pred),
#             gbm_lgb_pred = mean(gbm_pred, lgb_pred),
#             gbm_svr_pred = mean(gbm_pred, svr_pred),
#             xgb_lgb_pred = mean(xgb_pred, lgb_pred),
#             xgb_svr_pred = mean(xgb_pred, svr_pred),
#             lgb_svr_pred = mean(lgb_pred, svr_pred),
#             lm_rdf_gbm_pred = mean(lm_pred, rdf_pred, gbm_pred),
#             lm_rdf_xgb_pred = mean(lm_pred, rdf_pred, xgb_pred),
#             lm_rdf_lgb_pred = mean(lm_pred, rdf_pred, lgb_pred),
#             lm_rdf_svr_pred = mean(lm_pred, rdf_pred, svr_pred),
#             lm_gbm_xgb_pred = mean(lm_pred, gbm_pred, xgb_pred),
#             lm_gbm_lgb_pred = mean(lm_pred, gbm_pred, lgb_pred),
#             lm_gbm_svr_pred = mean(lm_pred, gbm_pred, svr_pred),
#             lm_xgb_lgb_pred = mean(lm_pred, xgb_pred, lgb_pred),
#             lm_xgb_svr_pred = mean(lm_pred, xgb_pred, svr_pred),
#             lm_lgb_svr_pred = mean(lm_pred, lgb_pred, svr_pred),
#             lm_lgb_svr_pred = mean(lm_pred, lgb_pred, svr_pred),
#             rdf_gbm_xgb_pred = mean(rdf_pred, gbm_pred, xgb_pred),
#             rdf_gbm_lgb_pred = mean(rdf_pred, gbm_pred, lgb_pred),
#             rdf_gbm_svr_pred = mean(rdf_pred, gbm_pred, svr_pred),
#             rdf_xgb_lgb_pred = mean(rdf_pred, xgb_pred, lgb_pred),
#             rdf_xgb_svr_pred = mean(rdf_pred, xgb_pred, svr_pred),
#             rdf_lgb_svr_pred = mean(rdf_pred, lgb_pred, svr_pred),
#             gbm_xgb_lgb_pred = mean(gbm_pred, xgb_pred, lgb_pred),
#             gbm_xgb_lgb_pred = mean(gbm_pred, xgb_pred, svr_pred),
#             xgb_lgb_svr_pred = mean(xgb_pred, lgb_pred, svr_pred),
#             lm_rdf_gbm_xgb_pred = mean(lm_pred, rdf_pred, gbm_pred, xgb_pred),
#             lm_rdf_gbm_lgb_pred = mean(lm_pred, rdf_pred, gbm_pred, lgb_pred),
#             lm_rdf_gbm_svr_pred = mean(lm_pred, rdf_pred, gbm_pred, svr_pred),
#             lm_rdf_xgb_lgb_pred = mean(lm_pred, rdf_pred, xgb_pred, lgb_pred),
#             lm_rdf_xgb_svr_pred = mean(lm_pred, rdf_pred, xgb_pred, svr_pred),
#             lm_rdf_lgb_svr_pred = mean(lm_pred, rdf_pred, lgb_pred, svr_pred),
#             lm_gbm_xgb_lgb_pred = mean(lm_pred, gbm_pred, xgb_pred, lgb_pred),
#             lm_gbm_xgb_svr_pred = mean(lm_pred, gbm_pred, xgb_pred, svr_pred),
#             lm_gbm_lgb_svr_pred = mean(lm_pred, gbm_pred, lgb_pred, svr_pred),
#             lm_xgb_lgb_svr_pred = mean(lm_pred, xgb_pred, lgb_pred, svr_pred),
#             rdf_gbm_xgb_lgb_pred = mean(rdf_pred, gbm_pred, xgb_pred, lgb_pred),
#             rdf_gbm_xgb_svr_pred = mean(rdf_pred, gbm_pred, xgb_pred, svr_pred),
#             rdf_xgb_lgb_svr_pred = mean(rdf_pred, xgb_pred, lgb_pred, svr_pred),
#             gbm_xgb_lgb_svr_pred = mean(gbm_pred, xgb_pred, lgb_pred, svr_pred),
#             lm_rdf_gbm_xgb_lgb_pred = mean(lm_pred, rdf_pred, gbm_pred, xgb_pred, lgb_pred),
#             lm_rdf_gbm_xgb_svr_pred = mean(lm_pred, rdf_pred, gbm_pred, xgb_pred, svr_pred),
#             lm_rdf_gbm_lgb_svr_pred = mean(lm_pred, rdf_pred, gbm_pred, lgb_pred, svr_pred),
#             lm_rdf_xgb_lgb_svr_pred = mean(lm_pred, rdf_pred, xgb_pred, lgb_pred, svr_pred),
#             lm_gbm_xgb_lgb_svr_pred = mean(lm_pred, gbm_pred, xgb_pred, lgb_pred, svr_pred),
#             rdf_gbm_xgb_lgb_svr_pred = mean(rdf_pred, gbm_pred, xgb_pred, lgb_pred, svr_pred),
#             lm_rdf_gbm_xgb_lgb_svr_pred = mean(lm_pred, rdf_pred, gbm_pred, xgb_pred, lgb_pred, svr_pred))
# 
# out = out %>% as.data.frame()
# 
# lm_eval = cbind(model_name = "Regression", forecast::accuracy(out.lm$real, out.lm$lm_pred))
# rdf_eval = cbind(model_name = "RF", forecast::accuracy(out.rdf$real, out.rdf$rdf_pred))
# gbm_eval = cbind(model_name = "GBM", forecast::accuracy(out.gbm$real, out.gbm$gbm_pred))
# xgb_eval = cbind(model_name = "XGB", forecast::accuracy(out.xgb$real, out.xgb$xgb_pred))
# lgb_eval = cbind(model_name = "LGB", forecast::accuracy(out.lgb$real, out.lgb$lgb_pred))
# svr_eval = cbind(model_name = "SVR", forecast::accuracy(out.svr$real, out.svr$svr_pred))
# 
# eval = do.call("rbind", list(lm_eval, 
#                              rdf_eval,
#                              gbm_eval,
#                              xgb_eval,
#                              lgb_eval,
#                              svr_eval))
# 
# eval = eval %>% as.data.frame()
# rownames(eval) = NULL
# 
# eval = eval %>% arrange(MAE)
# eval
# 
# for(i in 3:ncol(out)) {
#   print(colnames(out[,c(2,i)]))
#   print(NMAE(out[,2], out[,i]))
# }


NMAE(out$real, out$lm_pred)
NMAE(out$real, out$rdf_pred)
NMAE(out$real, out$gbm_pred)
NMAE(out$real, out$xgb_pred)
NMAE(out$real, out$lgb_pred)
NMAE(out$real, out$svr_pred)
NMAE(out$real, out$rdf_pred *0.35 + out$gbm_pred *0.10 + out$svr_pred *0.00 + out$xgb_pred *0.20 + out$lm_pred*0.35 + out$lgb_pred*0.00) # 8654

NMAE(out$real, out$rdf_pred *0.55 + out$gbm_pred *0.45) # 897
NMAE(out$real, out$rdf_pred *0.55 + out$gbm_pred *0.25 + out$svr_pred *0.2) # 894
NMAE(out$real, out$rdf_pred *0.45 + out$gbm_pred *0.25 + out$svr_pred *0.2 + out$xgb_pred *0.1) # 891
NMAE(out$real, out$rdf_pred *0.35 + out$gbm_pred *0.25 + out$svr_pred *0.2 + out$xgb_pred *0.2) # 890
NMAE(out$real, out$rdf_pred *0.3 + out$gbm_pred *0.2 + out$svr_pred *0.2 + out$xgb_pred *0.2 + out$lm_pred*0.1) # 886
NMAE(out$real, out$rdf_pred *0.35 + out$gbm_pred *0.15 + out$svr_pred *0.2 + out$xgb_pred *0.2 + out$lm_pred*0.1) # 885
NMAE(out$real, out$rdf_pred *0.45 + out$gbm_pred *0.2 + out$svr_pred *0.15 + out$xgb_pred *0.1 + out$lm_pred*0.1 + out$lgb_pred*0.) # 886
NMAE(out$real, out$rdf_pred *0.35 + out$gbm_pred *0.10 + out$svr_pred *0.00 + out$xgb_pred *0.20 + out$lm_pred*0.35 + out$lgb_pred*0.00) # 877

pred_rate = seq(0,1,0.05)

rate_df = data.frame()
for(lm_rate in pred_rate) {
  for(rdf_rate in pred_rate) {
    for(gbm_rate in pred_rate) {
      for(xgb_rate in pred_rate) {
        for(lgb_rate in pred_rate) {
          for(svr_rate in pred_rate) {

            eval_nmae = NMAE(out$real,
                   out$lm_pred*lm_rate + out$rdf_pred*rdf_rate + out$gbm_pred*gbm_rate + out$xgb_pred*xgb_rate + out$lgb_pred*lgb_rate + out$svr_pred*svr_rate)
            if(lm_rate + rdf_rate + gbm_rate + xgb_rate + lgb_rate + svr_rate == 1) {
              tmp = data.frame(lm_rate = lm_rate,
                               rdf_rate = rdf_rate,
                               gbm_rate = gbm_rate,
                               xgb_rate = xgb_rate,
                               lgb_rate = lgb_rate,
                               svr_rate = svr_rate,
                               nmae = eval_nmae)
              rate_df = rbind(rate_df, tmp)

            }

          }
        }
      }
    }
  }
  print(lm_rate)
}
Sys.time() # 2시간 소요
rate_df %>% arrange(nmae) %>% head

