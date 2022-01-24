
NMAE = function(true, pred) {
  mae = mean(abs(true-pred))
  score = mae / mean(abs(true))
  return(score)
}