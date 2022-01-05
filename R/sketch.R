#' @title Sketch
#'
#' @description Provides a subsample of data using sketches
#'
#' @param data (n times d)-dimensional matrix of data. 
#' The first column needs to be a vector of the dependent variable (Y) for leverage score sampling.
#' @param m subsample size that is less than n 
#' @param method method for sketching:
#' "unif" uniform sampling without replacement (default);
#' "unif_with_replacement" uniform sampling with replacement;
#' "leverage" leverage score sampling;
#' "CountSketch" CountSketch;
#' "fft" subsampled randomized trigonometric transforms using the real part of 
#' fast discrete Fourier transform (stats::ftt).
#' @return An S3 object has the following elements.
#' \item{subsample}{(m times d)-dimensional matrix of data}
#' \item{prob}{m-dimensional vector of probabilities. 
#' This output is generated only when "leverage" is selected.}   
#' @examples
#' ## Least squares: sketch and solve
#' # setup
#' n <- 1e+6 # full sample size
#' d <- 5    # dimension of covariates
#' m <- 1e+3 # sketch size
#' # generate psuedo-data
#' X <- matrix(stats::rnorm(n*d), nrow = n, ncol = d)
#' beta <- matrix(rep(1,d), nrow = d, ncol = 1)
#' eps <- matrix(stats::rnorm(n), nrow = n, ncol = 1)
#' Y <- X %*% beta + eps
#' intercept <- matrix(rep(1,n), nrow = n, ncol = 1)
#' # full sample including the intercept term
#' fullsample <- cbind(Y,intercept,X)
#' # generate a sketch using CountSketch
#' s_cs <-  sketch(fullsample, m, "CountSketch")
#' # solve without the intercept
#' ls_cs <- lm(s_cs$subsample[,1] ~ s_cs$subsample[,2] - 1)
#' # generate a sketch using leverage score sampling
#' s_lev <-  sketch(fullsample, m, "leverage")
#' # solve without the intercept with weighting
#' ls_lev <- lm(s_lev$subsample[,1] ~ s_lev$subsample[,2] - 1, weights = s_lev$prob)
#' 
#' @references Sokbae Lee, S and Ng, S. (2020). An Econometric Perspective on Algorithmic Subsampling.
#' Annual Review of Economics, 12:1, 45-80.
#'
#' @export
sketch = function(data, m, method = "unif"){
  
  data <- as.matrix(data)
  n <- nrow(data)
  d <- ncol(data)
  
  if (m >= n){
        stop("Error: m needs to be strictly smaller than n.")
  }
  
  if (m < n){
    
    if (method == "unif"){ 
    
    index <- sample.int(n, m, replace = FALSE)
    subsample <- data[index,]
    prob <- NA
    }
    
    if (method == "unif_with_replacement"){ 
      
      index <- sample.int(n, m, replace = TRUE)
      subsample <- data[index,]
      prob <- NA
    }
    
    if (method == "leverage"){ 
      
      Xmatrix <- data[,-1]
      svdX <- svd(Xmatrix)
      lev <- apply(svdX$u^2, 1, sum)/ncol(Xmatrix)
      index <- sample.int(n, m, replace = TRUE, prob = lev)
      subsample <- data[index,]
      prob <- lev[index]
    }
    
    if (method == "CountSketch"){ 

      hv = sample.int(m, n, replace=TRUE)        
      gv = (stats::runif(n) < 0.5) * 2 - 1
      subsample <- rcpp_count_sketch(data, hv, gv, m)
      prob <- NA
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
      subsample <- data_fft[index,]
      prob <- NA
    }
 
  outputs <- list("subsample"=subsample, "prob"=prob)
  
  outputs
  }
  
}
