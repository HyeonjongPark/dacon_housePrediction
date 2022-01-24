

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

# submission = out %>% 
#   group_by(id) %>% 
#   summarise(target = mean(rdf_pred, gbm_pred, xgb_pred)) %>% as.data.frame()

submission = out %>% 
  group_by(id) %>% 
  summarise(target = rdf_pred) %>% as.data.frame()


submission$id = as.integer(as.character(submission$id))
submission = submission %>% arrange(id)

submission %>% head
sub_ver = "sub5"
write.csv(submission, paste0("./out/submission/",sub_ver,".csv"), row.names = FALSE)
