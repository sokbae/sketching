#' @title Simulating observations from the data-generating process considered in Lee and Ng (2022)
#'
#' @description Simulates observations from the data-generating process considered in Lee and Ng (2022)
#'
#' @param n sample size
#' @param d dimension of regressors from a multivariate normal distribution
#' @param hetero TRUE if the conditional variance of the error term is heteroskedastic and
#'  FALSE if it is homoskedastic  (default: FALSE)
#' 
#' @return An S3 object has the following elements.
#' \item{Y}{n observations of outcomes}
#' \item{X}{n times d matrix of regressors}
#' \item{beta}{d dimensional vector of coefficients}
#' 
#' @examples
#'   data <- simulation_dgp(100, 5, hetero = TRUE)
#'   y <- data$Y
#'   x <- data$X
#'   model  <- lm(y ~ x) 
#'
#' @references Lee, S. and Ng, S. (2022). "Least Squares Estimation Using Sketched Data with Heteroskedastic Errors," arXiv:2007.07781.
#'
#' @export
simulation_dgp <- function(n, d, hetero = FALSE){

  # Setup
  mu <- rep(0,d)
  rho <- 0.5
  Sigma_first_row <- rho^(0:(d-1)) 
  Sigma <- stats::toeplitz(Sigma_first_row)
  beta <- matrix(rep(1,d), nrow = d, ncol = 1)
  
  true_beta <- beta[d]
  
  # generate data
  X <- MASS::mvrnorm(n, mu, Sigma)
  eps <- matrix(stats::rnorm(n), nrow = n, ncol = 1)
  
  if (hetero == TRUE){
    het <- exp(X[,d])
  } else{
    het <- 1
  }
  
  Y <- X %*% beta + het*eps

  outputs <- list("Y"=Y,"X"=X,"beta"=beta)
  
outputs    
}
