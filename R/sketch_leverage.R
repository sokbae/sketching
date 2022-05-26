#' @title Sketch using leverage score type sampling
#'
#' @description Provides a subsample of data using sketches
#'
#' @param data (n times d)-dimensional matrix of data. 
#' The first column needs to be a vector of the dependent variable (Y)
#' @param m subsample size that is less than n 
#' @param method method for sketching:
#' "leverage" leverage score sampling using X (default);
#' "root_leverage" square-root leverage score sampling using X.
#' @return An S3 object has the following elements.
#' \item{subsample}{(m times d)-dimensional matrix of data}
#' \item{prob}{m-dimensional vector of probabilities}
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
#' # generate a sketch using leverage score sampling
#' s_lev <-  sketch_leverage(fullsample, m, "leverage")
#' # solve without the intercept with weighting
#' ls_lev <- lm(s_lev$subsample[,1] ~ s_lev$subsample[,2] - 1, weights = s_lev$prob)
#' @references Ma, P., Zhang, X., Xing, X., Ma, J. and Mahoney, M.. (2020). Asymptotic Analysis of Sampling Estimators for Randomized Numerical Linear Algebra Algorithms. Proceedings of the Twenty Third International Conference on Artificial Intelligence and Statistics, PMLR 108:1026-1035.
#'
#' @export
sketch_leverage = function(data, m, method = "leverage"){
  
  n <- nrow(data)
  d <- ncol(data)
  shuffle <- sample.int(n, n, replace = TRUE)  # random permutation
  data <- data[shuffle,]
  data <- as.matrix(data)

  if (m >= n){
        stop("Error: m needs to be strictly smaller than n.")
  }
  
  if (m < n){
    
    if (method == "leverage"){ 
      
      Xmatrix <- data[,-1]
      svdX <- svd(Xmatrix)
      lev <- apply(svdX$u^2, 1, sum)
      lev_pi <- lev/sum(lev)
      index <- sample.int(n, m, replace = TRUE, prob = lev_pi)
      subsample <- sqrt(n/m)*data[index,]
      prob <- lev_pi[index]
    }
    
    if (method == "root_leverage"){ 
      
      Xmatrix <- data[,-1]
      svdX <- svd(Xmatrix)
      lev <- apply(svdX$u^2, 1, sum)
      rlev <- sqrt(lev)
      rlev_pi <- rlev/sum(rlev)
      index <- sample.int(n, m, replace = TRUE, prob = rlev_pi)
      subsample <- sqrt(n/m)*data[index,]
      prob <- rlev_pi[index]
    }

  outputs <- list("subsample"=subsample, "prob"=prob)
  
  outputs
  }
  
}
