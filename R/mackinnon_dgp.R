#' @title Simulating observations from the data-generating process in MacKinnon (2013)
#' @description Simulates observations from the data-generating process considered in Lee and Weidner (2021)
#'
#' @param n sample size
#' @param ps_spec specification of the propensity score: "overlap" or "non-overlap" (default: "overlap")
#' @param x_discrete TRUE if the distribution of the covariate is uniform on {-3.0, -2.9, ..., 3.0} and
#'  FALSE if the distribution of the covariate is uniform on [--3,3] (default: FALSE)
#' 
#' @return An S3 object of type "ATbounds". The object has the following elements.
#' \item{outcome}{n observations of binary outcomes}
#' \item{treat}{n observations of binary treatments}
#' \item{covariate}{n observations of a scalar covariate}
#' \item{ate_oracle}{the sample analog of E[Y(1) - Y(0)]}
#' \item{att_oracle}{the sample analog of E[D{Y(1) - Y(0)}|D=1]}
#' 
#' @examples
#'   data <- simulation_dgp(100, ps_spec = "overlap")
#'   y <- data$outcome
#'   d <- data$treat
#'   x <- data$covariate
#'   ate <- data$ate_oracle
#'   att <- data$att_oracle
#'
#' @references Sokbae Lee and Martin Weidner. Bounding Treatment Effects by Pooling Limited Information across Observations.
#'
#' @export
simulation_dgp <- function(n, ps_spec = "overlap", x_discrete = FALSE){
    
  x <- stats::runif(n, min = -3, max = 3)
  
  if (x_discrete == TRUE){
      x <- round(x*10)/10 # discrete X in {-3.0, -2.9, ..., 3.0}  
  }
  
  if (ps_spec == "overlap"){  
    px <- 0.5
  } else if (ps_spec == "non-overlap"){
    px <- 0.25*(x >= 2) + 0.5*(abs(x) < 2) 
  }
  
  treat <- as.integer(px < stats::runif(n)) # treat = 1 always if x <= -2 for the non-overlap case
  
  ps <- 1-px
  
  y_1 <- 1 + px + stats::rnorm(n)
  y_0 <- px + stats::rnorm(n)
  y_1  <- as.integer(y_1 > 0)
  y_0  <- as.integer(y_0 > 0)
  
  y <- treat*y_1 + (1-treat)*y_0
  
  ate_oracle <- mean(y_1 - y_0)
  att_oracle <- mean(treat*(y_1-y_0))/mean(ps)
  
  outputs <- list("outcome"=y,"treat"=treat,"covariate"=x,
                  "ate_oracle"=ate_oracle,"att_oracle"=att_oracle)
  
  class(outputs) = 'ATbounds'

outputs    
}
