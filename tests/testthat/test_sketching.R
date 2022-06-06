test_that("m must be strictly smaller than n", {
  
  fullsample <- matrix(rnorm(100),nrow=20,ncol=5)

  expect_error(sketch(fullsample, 20), "Error: m needs to be strictly smaller than n.")
  
})

test_that("m must be strictly smaller than n", {
  
  fullsample <- matrix(rnorm(100),nrow=20,ncol=5)
  
  expect_error(sketch_leverage(fullsample, 20), "Error: m needs to be strictly smaller than n.")
  
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
