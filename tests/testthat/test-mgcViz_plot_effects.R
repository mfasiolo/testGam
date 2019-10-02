context("plot effects")

test_that("plot.ptermMatrixNumeric", {
  
  library(splines)
  library(mgcViz)
  
  set.seed(2)
  dat <- gamSim(1, n = 200, dist="normal", scale=2)
  dat$fac <- as.factor(sample(1:5, 200, replace=TRUE))
  
  #####
  ## gamV, bamV, qgamV, gammV, gamm4V version 
  ####
  obj <- plt <- list()

  obj[[1]] <- gamV(y ~ bs(x0, knots = 0.5, degree = 1) + s(x3), data = dat)
  obj[[2]] <- bamV(y ~ bs(x0, knots = 0.5, degree = 1) + s(x3), data = dat)
  obj[[3]] <- qgamV(y ~ bs(x0, knots = 0.5, degree = 1) + s(x3), data = dat, qu = 0.5)
  obj[[4]] <- gammV(y ~ bs(x0, knots = 0.5, degree = 1) + s(x3), random=list(fac=~1), data = dat)
  obj[[5]] <- gamm4V(y ~ bs(x0, knots = 0.5, degree = 1) + s(x3), random=~(1|fac), data = dat)
  
  for(ii in 1:5){
    expect_error(plt[[ii]] <- plot(obj[[ii]], allTerms = TRUE, trans = exp, select = 2), NA)
  }
  plt[[1]]
  
  #####
  ## getViz version (test only gam version)
  ####
  # gamV
  b <- gam(y ~ bs(x0, knots = 0.5, degree = 1) + s(x3), data = dat)
  b <- getViz(b)
  expect_error(pl <- plot(b, allTerms = TRUE, trans = exp, select = 2), NA)
  
  
})
