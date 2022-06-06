test_that("m must be strictly smaller than n", {
  
  fullsample <- matrix(rnorm(100),nrow=20,ncol=5)

  expect_error(sketch(fullsample, 20), "Error: m needs to be strictly smaller than n.")
  
})

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

test_that("Different DGPs generate different data", {
  
  data <- simulation_dgp(100, 5, hetero = TRUE)
  y <- data$Y
  x <- data$X
  model1  <- lm(y ~ x) 
  
  data <- simulation_dgp(100, 5, hetero = FALSE)
  y <- data$Y
  x <- data$X
  model2  <- lm(y ~ x) 
  
  expect_false(model1$coefficients[1] == model2$coefficients[1])
  
})


data <- simulation_dgp(100, 5, hetero = TRUE)
y <- data$Y
x <- data$X
model  <- lm(y ~ x) 