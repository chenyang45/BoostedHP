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
