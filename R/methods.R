#' BIC.bHP
#'
#' Extract the path of BIC value of each iterated BIC HP-filter conduction for
#' class \code{bHP}. As \code{nonstop} type of bHP also keeps BIC for each iteration time till
#' the \code{Max_iter}, BIC.bHP method returns BIC value for it as well.
#'
#' @param x an object of class \code{bHP}
#'
#' @return a vector recording BIC after each iteration of bHP.
#'
#' @export
#'
#' @examples
#' lam <- 100 # tuning parameter for the annual data
#'
#' data(IRE) # load the data 'IRE'
#'
#' # by BIC
#'
#' bx_BIC <- BoostedHP(IRE, lambda = lam, iter= TRUE, stopping = "BIC")
#'
#' BIC(bx_BIC)
#'
#'
#' bx_none <- BoostedHP(IRE, lambda = lam, iter= TRUE, stopping = "nonstop")
#'
#' BIC(bx_none)
#'
#'
#'
#' ### If the test type is not "adf", Pvalue.bHP will return error
#'
#' # raw HP filter
#'
#' bx_HP <- BoostedHP(IRE, lambda = lam, iter= FALSE)
#'
#' BIC(bx_HP)
#'
#' # Error in BIC.bHP(bx_HP) :
#' # The stationary test type is none-iter, not BIC or none.
#'
#'
#' # by ADF
#' bx_ADF <- BoostedHP(IRE, lambda = lam, iter= TRUE, stopping = "adf")
#'
#' BIC(bx_ADF)
#'
#' #Error in BIC.bHP(bx_ADF) :
#' #The stationary test type is adf, not BIC or none.
#'
BIC.bHP <- function(x){

  if(x$stopping == "BIC" | x$stopping == "nonstop"){

    message("Retrun the BIC path of ", x$stopping, ".")
    message("Number of iterations: ",x$iter_num)
    message("Keep the path of BIC till iterated ", (x$iter_num+1), " times.")

    return(x$BIC_hist)
  }
  else {
    message("The stopping criterion is not BIC.")
  }
}

#####################################################################

#' Predict.bHP
#'
#' Extract the final trend component for class \code{bHP}.
#'
#' @param x an object of class \code{bHP}
#'
#' @return the estimated trend component
#'
#' @export
#'
#' @examples
#' lam <- 100 # tuning parameter for the annual data
#'
#' data(IRE) # load the data 'IRE'
#'
#' # raw HP filter
#' bx_HP <- BoostedHP(IRE, lambda = lam, iter = FALSE)
#'
#' # by ADF
#' bx_ADF <- BoostedHP(IRE, lambda = lam, iter = TRUE, stopping = "adf", sig_p = 0.050)
#'
#' # return the final trend component
#'
#' predict(bx_HP)
#'
#' predict(bx_ADF)


predict.bHP <- function(x) {
  message("Retrun the trend component of ", x$stopping, " criterion.")
  message("Number of iterations: ", length(x$trend_hist[1, ]))

  return(x$trend)
}

####################################################
#' Residuals.bHP
#'
#' Extract the final cycle component for class \code{bHP}.
#'
#' @param x an object of class \code{bHP}
#'
#' @return the estimated cycle component
#'
#' @export
#'
#' @examples
#' lam <- 100 # tuning parameter for the annual data
#'
#' data(IRE) # load the data 'IRE'
#'
#' # raw HP filter
#' bx_HP <- BoostedHP(IRE, lambda = lam, iter = FALSE)
#'
#' # by ADF
#' bx_ADF <- BoostedHP(IRE, lambda = lam, iter = TRUE, stopping = "adf", sig_p = 0.050)
#'
#' # return the final trend component
#'
#' residuals(bx_HP)
#'
#' residuals(bx_ADF)

residuals.bHP <- function(x) {
  message("Retrun the trend component of ", x$stopping, " criterion.")
  message("Number of iterations: ", length(x$trend_hist[1, ]))

  return(x$cycle)
}
####################################################
