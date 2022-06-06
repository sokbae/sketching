Y <- AK$LWKLYWGE
intercept <- AK$CNST
X_end <- AK$EDUC
X_exg <- AK[,3:11]
X <- cbind(X_exg, X_end)
Z_inst <- AK[,12:(ncol(AK)-1)]
Z <- cbind(X_exg, Z_inst)
fullsample <- cbind(Y,intercept,X)
n <- nrow(fullsample)
d <- ncol(X)

test_that("Different methods generate different results I", {
  
  m <- 1000
  subsample <- sketch(fullsample, m, method = "bernoulli")
  ys <- subsample[,1]
  reg <- subsample[,-1]
  submodel1 <- lm(ys ~ reg - 1) 
  
  subsample <- sketch(fullsample, m, method = "countsketch")
  ys <- subsample[,1]
  reg <- subsample[,-1]
  submodel2 <- lm(ys ~ reg - 1) 
  
  expect_false(submodel1$coefficients[d+1] == submodel2$coefficients[d+1])
  
})

test_that("Different methods generate different results II", {
  
  m <- 1000
  subsample <- sketch(fullsample, m, method = "fft")
  ys <- subsample[,1]
  reg <- subsample[,-1]
  submodel1 <- lm(ys ~ reg - 1) 
  
  subsample <- sketch(fullsample, m, method = "srht")
  ys <- subsample[,1]
  reg <- subsample[,-1]
  submodel2 <- lm(ys ~ reg - 1) 
  
  expect_false(submodel1$coefficients[d+1] == submodel2$coefficients[d+1])
  
})

test_that("Different methods generate different results III", {
  
  m <- 100
  subsample <- sketch(fullsample, m, method = "unif")
  ys <- subsample[,1]
  reg <- subsample[,-1]
  submodel1 <- lm(ys ~ reg - 1) 
  
  subsample <- sketch(fullsample, m, method = "gaussian")
  ys <- subsample[,1]
  reg <- subsample[,-1]
  submodel2 <- lm(ys ~ reg - 1) 
  
  expect_false(submodel1$coefficients[d+1] == submodel2$coefficients[d+1])
  
})



test_that("Uniform sampling with and without replacement", {
  
  m <- 1000
  subsample <- sketch(fullsample, m, method = "unif")
  ys <- subsample[,1]
  reg <- subsample[,-1]
  submodel1 <- lm(ys ~ reg - 1) 
  
  subsample <- sketch(fullsample, m, method = "unif_without_replacement")
  ys <- subsample[,1]
  reg <- subsample[,-1]
  submodel2 <- lm(ys ~ reg - 1) 
  
  expect_false(submodel1$coefficients[d+1] == submodel2$coefficients[d+1])
  
})

test_that("Uniform sampling vs leverage sampling", {
  
  m <- 1000

  subsample <- sketch_leverage(fullsample, m, method = "leverage")
  ys <- subsample$subsample[,1]
  reg <- subsample$subsample[,-1]
  prb <- subsample$prob
  
  submodel_weight <- 1/(m*prb)
  submodel1 <- lm(ys ~ reg - 1, weights = submodel_weight) 
  
  subsample <- sketch_leverage(fullsample, m, method = "root_leverage")
  ys <- subsample$subsample[,1]
  reg <- subsample$subsample[,-1]
  prb <- subsample$prob
  
  submodel_weight <- 1/(m*prb)
  submodel2 <- lm(ys ~ reg - 1, weights = submodel_weight) 
  
  expect_false(submodel1$coefficients[d+1] == submodel2$coefficients[d+1])
  
})




