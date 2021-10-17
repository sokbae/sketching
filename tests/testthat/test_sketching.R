test_that("m must be strictly smaller than n", {
  
  fullsample <- matrix(rnorm(100),nrow=20,ncol=5)

  expect_error(sketch(fullsample, 20), "Error: m needs to be strictly smaller than n.")
  
})

