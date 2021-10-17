#' @title Sketch
#'
#' @description Provides a subsample of data using sketches
#'
#' @param data (n times d)-dimensional matrix of data
#' @param m subsample size that is less than n 
#' @param method method for sketching
#'
#' @return (m times d)-dimensional matrix of data  
#' @examples
#' fullsample <- matrix(rnorm(100), nrow = 20,ncol = 5)
#' subsample <-  sketch(fullsample, 10)
#' subsample <-  sketch(fullsample, 10, "unif_with_replacement")
#' subsample <-  sketch(fullsample, 10, "CountSketch")
#' subsample <-  sketch(fullsample, 10, "fft")
#'
#' @references Sokbae Lee, S and Ng, S. (2020). An Econometric Perspective on Algorithmic Subsampling.
#' Annual Review of Economics, 12:1, 45-80.
#'
#' @export
sketch = function(data, m, method = "unif"){
  
  data = as.matrix(data)
  n = nrow(data)
  d = ncol(data)
  
  if (m >= n){
        stop("Error: m needs to be strictly smaller than n.")
  }
  
  if (m < n){
    
    if (method == "unif"){ 
    
    index <- sample.int(n, m, replace = FALSE)
    subsample <- data[index,]
    }
    
    if (method == "unif_with_replacement"){ 
      
      index <- sample.int(n, m, replace = FALSE)
      subsample <- data[index,]
    }
    
    if (method == "CountSketch"){ 

      hv = sample.int(m, n, replace=TRUE)        
      gv = (stats::runif(n) < 0.5) * 2 - 1
      subsample = rcpp_count_sketch(data, hv, gv, m)
    }
    
    if (method == "fft"){
      
      data[1:floor(n/2),] <- -data[1:floor(n/2),]  # sign flip
      shuffle <- sample.int(n, n, replace = TRUE)  # random permutation
      data <- data[shuffle,]
      # Fast Discrete Fourier Transform (FFT)
      data_fft <- {}
      for (j in 1:d){
        # apply FFT to each column and take only the real part
        data_fft <- cbind(data_fft, Re(stats::fft(data[,j])))
      }
      # random sampling without replacement
      index <- sample.int(nrow(data_fft), m, replace = FALSE)
      subsample = data_fft[index,]
    }
 
  subsample
  }
  
}
