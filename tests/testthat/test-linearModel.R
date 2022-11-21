test_that("slope works", {
  expect_equal(linearModel(X=matrix(c(1,2,3,4,5,6),  nrow=6),Y=c(1, 2, 3, 4, 5, 6))["beta1","Estimate"], 1)
})


test_that("overspecified model works", {
  xPrime=matrix(rnorm(30, mean = 3, sd = 1), nrow= 6, ncol = 5)
  yPrime=c(rep(10, times = 6))
  expect_equal(linearModel(X=xPrime, Y= yPrime), "model singularities")
})


test_that("coefficients works", {
  xPrime=matrix(rnorm(300, mean = 3, sd = 1), nrow= 60, ncol = 5)
  betaTrue =  c(1, 3, -1, 2, 3)
  yPrime=(xPrime) %*% betaTrue +rnorm(60, 0, 2)
  dat = cbind.data.frame(xPrime, yPrime)
  names(dat) = c( "beta1", "beta2", "beta3", "beta4", "beta5", "Y")
  foo = lm(Y~beta1+beta2+beta3+beta4+beta5, data = dat)
  fooCoeff <- foo$coefficients
  names(fooCoeff) <- NULL
  expect_equal(unlist(round(linearModel(X=xPrime, Y= yPrime)["Estimate"], 5), use.names = FALSE),
               round(fooCoeff, 5))
})



test_that("tval works", {
  xPrime=matrix(rnorm(300, mean = 3, sd = 1), nrow= 60, ncol = 5)
  betaTrue =  c(1, 3, -1, 2, 3)
  yPrime=(xPrime) %*% betaTrue +rnorm(60, 0, 2)
  dat = cbind.data.frame(xPrime, yPrime)
  names(dat) = c( "beta1", "beta2", "beta3", "beta4", "beta5", "Y")
  foo = lm(Y~beta1+beta2+beta3+beta4+beta5, data = dat)
  fooPval = summary(foo)$coefficients[,3]
  names(fooPval) = NULL
  expect_equal(unlist(round(linearModel(X=xPrime, Y= yPrime)["t-value"], 5), use.names = FALSE),
               round(fooPval, 5))
})



