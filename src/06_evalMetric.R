

## 제출용


colnames(out.lm)[2] = "lm_pred"
colnames(out.rdf)[2] = "rdf_pred"
colnames(out.gbm)[2] = "gbm_pred"
colnames(out.xgb)[2] = "xgb_pred"
colnames(out.lgb)[2] = "lgb_pred"
colnames(out.svr)[2] = "svr_pred"

out = do.call("cbind", list(out.lm,
                            out.rdf,
                            out.gbm,
                            out.xgb,
                            out.lgb,
                            out.svr))
out = out[c(1,grep("pred",colnames(out)))]
out[,2:ncol(out)] = exp(out[,2:ncol(out)])-1

# submission = out %>% 
#   group_by(id) %>% 
#   summarise(target = mean(rdf_pred, gbm_pred, xgb_pred)) %>% as.data.frame()
out$lm_pred*0.35 + out$rdf_pred *0.35 + out$gbm_pred *0.10 + out$xgb_pred *0.20 + + out$lgb_pred*0.00 + out$svr_pred *0.00

out = out %>% 
  mutate(target = out$lm_pred*0.35 + out$rdf_pred *0.35 + out$gbm_pred *0.10 + out$xgb_pred *0.20 + + out$lgb_pred*0.00 + out$svr_pred *0.00)


submission = out %>% 
  select(id, target)

# submission$id = as.integer(as.character(submission$id))
# submission = submission %>% arrange(id)

submission %>% head

write.csv(submission, paste0("./out/submission/sub9.csv"), row.names = FALSE)
