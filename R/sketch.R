#' @title Sketch
#'
#' @description Provides a subsample of data using sketches
#'
#' @param data (n times d)-dimensional matrix of data. 
#' @param m (expected) subsample size that is less than n 
#' @param method method for sketching:
#' "unif" uniform sampling with replacement (default);
#' "unif_without_replacement" uniform sampling without replacement;
#' "bernoulli" Bernoulli sampling;
#' "gaussian" Gaussian projection;
#' "countsketch" CountSketch;
#' "srht" subsampled randomized Hadamard transform;
#' "fft" subsampled randomized trigonometric transforms using the real part of 
#' fast discrete Fourier transform (stats::ftt).
#' @return (m times d)-dimensional matrix of data
#' For Bernoulli sampling, the number of rows is not necessarily m.    
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
#' s_cs <-  sketch(fullsample, m, "countsketch")
#' # solve without the intercept
#' ls_cs <- lm(s_cs[,1] ~ s_cs[,2] - 1)
#' # generate a sketch using SRHT
#' s_srht <-  sketch(fullsample, m, "srht")
#' # solve without the intercept
#' ls_srht <- lm(s_srht[,1] ~ s_srht[,2] - 1)
#' 
#' @export
sketch = function(data, m, method = "unif"){
  
  n <- nrow(data)
  d <- ncol(data)
  shuffle <- sample.int(n, n, replace = TRUE)  # random permutation
  data <- data[shuffle,]
  data <- as.matrix(data)

  if (m >= n){
        stop("Error: m needs to be strictly smaller than n.")
  }
  
  if (m < n){
    
    if (method == "unif"){ 
    
    index <- sample.int(n, m, replace = TRUE)
    subsample <- sqrt(n/m)*data[index,]
    }
    
    if (method == "unif_without_replacement"){ 
      
      index <- sample.int(n, m, replace = FALSE)
      subsample <- sqrt(n/m)*data[index,]
    }
    
    if (method == "bernoulli"){ 
      
      index <- 1:n
      index <- index[stats::runif(n) < m/n]
      subsample <- sqrt(n/m)*data[index,]
    }
    
    if (method == "gaussian"){ 
      
    Pi_matrix <- matrix(stats::rnorm(n*m), nrow = m, ncol = n)
    subsample <- (1/sqrt(m)) * (Pi_matrix %*% data)
    }
    
    if (method == "countsketch"){ 

      hv = sample.int(m, n, replace=TRUE)        
      gv = (stats::runif(n) < 0.5) * 2 - 1
      subsample <- rcpp_count_sketch(data, hv, gv, m)
    }
    
    if (method == "srht"){
      
      data[1:floor(n/2),] <- -data[1:floor(n/2),]  # sign flip
      # SRHT
      data_srht <- {}
      for (j in 1:d){
        # zero padding
        n_new <- 2^ceiling(log2(n))
        data_j <- c(data[,j],rep(0,n_new-n))
        # multiply Hadamard matrix to each column
        data_srht <- cbind(data_srht, phangorn::fhm(data_j))
      }
      data_srht <- data_srht/sqrt(n)
      # random sampling with replacement
      index <- sample.int(nrow(data_srht), m, replace = TRUE)
      subsample <- sqrt(n/m)*data_srht[index,]
    }
    
    if (method == "fft"){
      
      data[1:floor(n/2),] <- -data[1:floor(n/2),]  # sign flip
      # Fast Discrete Fourier Transform (FFT)
      data_fft <- {}
      for (j in 1:d){
        # apply FFT to each column and take only the real part
        data_fft <- cbind(data_fft, Re(stats::fft(data[,j])))
      }
      data_fft <- data_fft/sqrt(n)
      # random sampling with replacement
      index <- sample.int(nrow(data_fft), m, replace = TRUE)
      subsample <- sqrt(n/m)*data_fft[index,]
    }
    
  subsample
  }
  
}
