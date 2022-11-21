## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
require(myHW3)
require(bench)
require(ggplot2)

## -----------------------------------------------------------------------------
X = matrix(rnorm(666), ncol = 3)
Y = sample(1:nrow(X)) +rnorm(nrow(X), mean = 0, sd = .2)

## -----------------------------------------------------------------------------
model1 = linearModel(X, Y)

## -----------------------------------------------------------------------------
dat = cbind.data.frame(X, Y)
names(dat) = c("x1", "x2", "x3", "y")
test1 = lm(y~., data = dat)

## -----------------------------------------------------------------------------
summary(test1)
model1

## -----------------------------------------------------------------------------
lmPval = summary(test1)$coefficients[,4]
names(lmPval) = NULL
all.equal(unlist(round(model1["p-value"], 3), use.names = FALSE),
          round(lmPval, 3))

## -----------------------------------------------------------------------------
lmEst = summary(test1)$coefficients[,1]
names(lmEst) = NULL
all.equal(unlist(round(model1["Estimate"], 5), use.names = FALSE),
          round(lmEst, 5))

## -----------------------------------------------------------------------------
lmT = summary(test1)$coefficients[,3]
names(lmT) = NULL
all.equal(unlist(round(model1["t-value"], 3), use.names = FALSE),
          round(lmT, 3))

## -----------------------------------------------------------------------------
lmSHat = summary(test1)$coefficients[,2]
names(lmSHat) = NULL
all.equal(unlist(round(model1["Std. Error"], 5), use.names = FALSE),
          round(lmSHat, 5))

## -----------------------------------------------------------------------------
X2 = cbind(X[,1:2], X[,1]*X[,2])
model2 = linearModel(X2, Y)
test2 = lm(Y~x1*x2, data = dat)
summary(test2)$coefficients
model2

## -----------------------------------------------------------------------------
lmPval = summary(test2)$coefficients[,4]
names(lmPval) = NULL
all.equal(unlist(round(model2["p-value"], 3), use.names = FALSE),
          round(lmPval, 3))

## -----------------------------------------------------------------------------
lmEst = summary(test2)$coefficients[,1]
names(lmEst) = NULL
all.equal(unlist(round(model2["Estimate"], 5), use.names = FALSE),
          round(lmEst, 5))

## -----------------------------------------------------------------------------
lmT = summary(test2)$coefficients[,3]
names(lmT) = NULL
all.equal(unlist(round(model2["t-value"], 3), use.names = FALSE),
          round(lmT, 3))

## -----------------------------------------------------------------------------
lmSHat = summary(test2)$coefficients[,2]
names(lmSHat) = NULL
all.equal(unlist(round(model2["Std. Error"], 5), use.names = FALSE),
          round(lmSHat, 5))

## -----------------------------------------------------------------------------
X = matrix(rgamma(100, shape = 2), nrow = 100)
Y = (rnorm(100, mean = 5, sd = 2))

dat = cbind.data.frame(X, Y)
mark( linearModel(X, Y),
      summary(lm(Y~X, data = dat))$coefficients, check = FALSE)


## -----------------------------------------------------------------------------
res = NULL
for(i in seq(from = 1, to = 5, by = .5)){
    #generate appropriate data
  X = matrix(rgamma(10^i, shape = 2), nrow = 10^i)
  Y = (rnorm(10^i, mean = 5, sd = 2)) 

  
  dat = cbind.data.frame(X, Y)
  #compare models
  fooMark = mark( linearModel(X, Y),
      summary(lm(Y~X, data = dat))$coefficients, check = FALSE)
  res$size = c(res$size, 10^i)
  #store results in data frame
  res$runTimeMine = c(res$runTimeMine, fooMark$median[1])
  res$runTimeBase = c(res$runTimeBase, fooMark$median[2])
  res$runMemMine = c(res$runMemMine, fooMark$mem_alloc[1])
  res$runMemBase = c(res$runMemBase, fooMark$mem_alloc[2])
}
res = as.data.frame(res)
colMeans(res)
p = ggplot(data = res)+geom_line(aes(x = size, y = runTimeMine), color = "red")+
  geom_line(aes(x = size, y = runTimeBase), color = "blue")+
  labs(x = "Run Time", y = "Number of observations")+
  ggtitle("Comparing Run Times"); p
q = ggplot(data = res)+geom_line(aes(x = size, y = runMemMine), color = "red")+
  geom_line(aes(x = size, y = runMemBase), color = "blue")+
  labs(x = "Allocation", y = "Number of observations")+
  ggtitle("Comparing Memory Allocation"); q

## -----------------------------------------------------------------------------
X = matrix(rnorm(42), ncol = 6, nrow = 7)
Y= rnorm(7)
linearModel(X,Y)

## -----------------------------------------------------------------------------
X = as.matrix(rpois(500, lambda = 2), ncol = 1)
Y = X*5
linearModel(X= X, Y=Y)
dat = cbind.data.frame(X,Y)
summary(lm(Y~X, data = dat))$coefficients

## -----------------------------------------------------------------------------
x1= rgamma(20, 1, 1)
x2 = rbeta(20, 3, 5)

Y = pi *x1*x1 +rnorm(20, 0, 5)

X = matrix(c(x1, x2), nrow = 20, ncol = 2)

linearModel(X, Y)
dat = cbind.data.frame(X, Y); names(dat) = c("X1", "X2")
m1 = lm(Y ~ X1+X2, data = dat)
summary(m1)$coefficients

X = matrix(x1*x1, nrow = 20)
linearModel(X, Y)
m2 = lm(Y~I(X1^2), data = dat)
summary(m2)$coefficients


## -----------------------------------------------------------------------------
Y = matrix(rpois(500, lambda = 3), nrow = 500)
X = matrix(rnorm(500, mean = 3, sd = 1), nrow = 500)
dat = cbind.data.frame(Y, X)

linearModel(X, Y)
summary(lm(Y~X, data = dat))$coefficients

## -----------------------------------------------------------------------------
X = matrix(rexp(200, rate = 1/4), nrow = 200)
Y = 3* X + rnorm( 200, mean = 0, sd = .2)
X[seq(from = 1, to = 200, by = 2)] = runif(length(seq(from = 1, to = 200, by = 2)), min = -50, max = 50)

dat = cbind.data.frame(Y, X)

linearModel(X, Y)
summary(lm(Y~X, data = dat))$coefficients

## -----------------------------------------------------------------------------
X= matrix(rgamma(400, .4, 7), ncol = 8, nrow = 50) 
Y = 3*X[, 1]+ .2 *X[,3] + 5 + rnorm(50, 2, 1)
dat = cbind.data.frame(X, Y)
linearModel(X, Y)
summary(lm(Y~., data = dat))$coefficients

## -----------------------------------------------------------------------------
X = matrix(sample(3, size = 500, replace = TRUE))
X2 = ifelse(X == 2, 1, 0)
X3 = ifelse(X==3, 1, 0)
Y = rnorm(500, 0, sd = 1) + .25 *X2 -3*X3
dat = cbind.data.frame(X2, X3, Y)

linearModel(matrix(c(X2, X3), nrow = 500), Y)
summary(lm(Y~., data = dat))$coefficients

