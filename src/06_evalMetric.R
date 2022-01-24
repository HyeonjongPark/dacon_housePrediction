

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

submission = out %>% select(id, rdf_pred)
colnames(submission)[2] = "target"


submission %>% head

write.csv(submission, "./out/submission/sub1.csv", row.names = FALSE)
