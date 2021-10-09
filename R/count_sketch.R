#' @title CountSketch
#'
#' @description Provides a subsample of data using CountSketch
#'
#' @param data (n times d)-dimensional matrix of data
#' @param m subsample size that is less than n 
#'
#' @return (m times d)-dimensional matrix of data  
#'
#' @export
count_sketch = function(data, m){
  
  n = nrow(data)
  hv = sample.int(m, n, replace=TRUE)        
  gv = (stats::runif(n) < 0.5) * 2 - 1
  subsample = rcpp_count_sketch(as.matrix(data), hv, gv, m)
 
  subsample
}
