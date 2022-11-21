#' linearModel
#'
#' Provided the same information that summary (lm) does in base R
#' This package does not have the functionality to include interactions,
#' higher order terms, or functions of variables. Those must be included as
#' part of the design matrix.
#' Furthermore, all data that is put into the function is assumed to be a
#' part of the regression. There is no variable selection process.
#'
#' @param X Design matrix with covariate values in it.
#' @param Y Observed Y, each row corresponds to one outcome.
#'
#' @examples
#' set.seed(1616)
#' linearModel(X = matrix(rnorm(500), ncol = 5, nrow = 100), Y = rnorm(100))
#'
#' @import stats
#' @return A dataframe with estimates of coefficients, standard errors, t-values and p-values.
#' @export
#'
linearModel = function(X , Y){
  X= cbind(rep(1, times = nrow(X)),X) #create design matrix iwht intercept
  betaHat = solve(t(X)%*%X)%*%t(X)%*%Y #get beta estimates
  yHat = X%*% betaHat #predicted values
  n = length(Y)
  p = ncol(X)
  if(n>p){ # if there are more parameters than observations, cannot estimate SE
    sigmaHat = 1/(n-p)* t(Y-yHat)%*% (Y-yHat)
    stdError = sqrt(diag(as.numeric(sigmaHat) * solve(t(X)%*%X)))
    tVal = betaHat/stdError #calculate t-statistic
    posBeta = which(betaHat>0) #get the indices for which we need to flip p-value
    pVal = 2*pt(abs(tVal), n-1, lower.tail = FALSE) #calculate p-value

    #format data
    frame = cbind.data.frame(betaHat, stdError, tVal, pVal)
    colnames(frame) = c("Estimate", "Std. Error", "t-value", "p-value")
    rownames(frame) = c(paste0("beta", 0:(p-1)))
    return(frame)
  }else{
    #return error if cannot estimate SE
    return("model singularities")
  }

}



