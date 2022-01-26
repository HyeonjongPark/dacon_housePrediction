#rm(list = ls())

source("./src/04_dataMart.R", encoding = "UTF-8")



## 검증용
#######################################################################
################## split -> train, test     ########################### 
################## lm   + 다중공선성 확인   ########################### 
#######################################################################
set.seed(8627)

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


#set.seed(123)


mod.rdf = randomForest(target ~ ., data = train2_train[,-1],
                       ntree=1000,importance=T)

pred.rdf = predict(mod.rdf, train2_valid)


out.rdf = data.frame(id = train2_valid$id,
                     real = train2_valid$target,
                     pred = pred.rdf)


forecast::accuracy(out.rdf$real, out.rdf$pred)

NMAE(out$real, out.rdf$pred)

# for(i in 1:10) {
#   
#   proportion = 0.7
#   idx = sample(1:nrow(train2), size = round(proportion * nrow(train2)))
#   train2_train = train2[idx, ]
#   train2_valid = train2[-idx, ]
#   
#   y_train = train2_train$target
#   y_test = train2_valid$target
#   
#   mod.rdf = randomForest(target ~ ., data = train2_train[,-1], 
#                          ntree=1000,importance=T)
#   pred.tmp = predict(mod.rdf, train2_valid)
#   pred.rdf = (pred.rdf + pred.tmp) / 2
#   
#   out.rdf = data.frame(id = train2_valid$id,
#                        real = train2_valid$target,
#                        pred = pred.rdf)
#   
#   print(NMAE(out$real, out.rdf$pred))
#   
# }


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

### 모델링
cat("modeling\n")

param.xgb <- list(subsample = 1
                  , max_depth = 5
                  , colsample_bytree = 0.5
                  , eta = 0.08
                  , eval_metric = 'rmse'#average
                  , min_child_weight = 1.2)



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

NMAE(out$real, out.svr$pred)

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
#out[,2:ncol(out)] = 10^(out[,2:ncol(out)])-1
out[,2:ncol(out)] = exp(out[,2:ncol(out)])-1

NMAE(out$real, out$lm_pred)
NMAE(out$real, out$rdf_pred)
NMAE(out$real, out$gbm_pred)
NMAE(out$real, out$xgb_pred)
NMAE(out$real, out$lgb_pred)
NMAE(out$real, out$svr_pred)


# comPlusVec = train2_valid %>% mutate(comb.Built = Year.Built_cal + Year.Remod.Add_cal + Garage.Yr.Blt_cal) %>% 
#   transform(comb.Area = Gr.Liv.Area + Garage.Area + Total.Bsmt.SF + X1st.Flr.SF) %>% 
#   transform(comb.roomCount = Garage.Cars + Full.Bath) %>% 
#   transform(comb.plusAll = abs(comb.Qual) * (abs(comb.Area) + abs(comb.roomCount)) - (abs(comb.Qual) * abs(comb.Built))) %>% 
#   select(comb.plusAll)
# comPlusVec$comb.plusAll
# NMAE(out$real, out$lm_pred*0.50 + out$rdf_pred *0.15 + out$gbm_pred *0.10 + out$xgb_pred *0.15 + out$lgb_pred*0.10 + out$svr_pred *0.00) # 8654


NMAE(out$real, out$lm_pred*0.35 + out$rdf_pred *0.35 + out$gbm_pred *0.10 + out$xgb_pred *0.20 + out$lgb_pred*0.00 + out$svr_pred *0.00) # 8654
NMAE(out$real, out$lm_pred*0.50 + out$rdf_pred *0.15 + out$gbm_pred *0.10 + out$xgb_pred *0.15 + out$lgb_pred*0.10 + out$svr_pred *0.00) # 8654


# pred_rate = seq(0,1,0.05)
# 
# rate_df = data.frame()
# for(lm_rate in pred_rate) {
#   for(rdf_rate in pred_rate) {
#     for(gbm_rate in pred_rate) {
#       for(xgb_rate in pred_rate) {
#         for(lgb_rate in pred_rate) {
#           for(svr_rate in pred_rate) {
# 
#             eval_nmae = NMAE(out$real,
#                    out$lm_pred*lm_rate + out$rdf_pred*rdf_rate + out$gbm_pred*gbm_rate + out$xgb_pred*xgb_rate + out$lgb_pred*lgb_rate + out$svr_pred*svr_rate)
#             if(lm_rate + rdf_rate + gbm_rate + xgb_rate + lgb_rate + svr_rate == 1) {
#               tmp = data.frame(lm_rate = lm_rate,
#                                rdf_rate = rdf_rate,
#                                gbm_rate = gbm_rate,
#                                xgb_rate = xgb_rate,
#                                lgb_rate = lgb_rate,
#                                svr_rate = svr_rate,
#                                nmae = eval_nmae)
#               rate_df = rbind(rate_df, tmp)
# 
#             }
# 
#           }
#         }
#       }
#     }
#   }
#   print(lm_rate)
# }
# Sys.time() # 2시간 소요
# rate_df %>% arrange(nmae) %>% head

