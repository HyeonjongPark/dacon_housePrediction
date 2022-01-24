
NMAE = function(true, pred) {
  mae = mean(abs(true-pred))
  score = mae / mean(abs(true))
  return(score)
}

Convert_Numeric = function(X) {
  L = levels(X)
  Y = as.numeric(factor(X, labels = seq(1:length(L))))
  return(Y)
}


normal = function(x) {
  return((x-min(x))/(max(x)-min(x)))
}