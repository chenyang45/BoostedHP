#' Generic Pvalue
#'
#' Generic function for extractting the path of p-value of R objects.
#'
#' @export  UseMethod("Pvalue",x)
#'


Pvalue <- function(x, ...) UseMethod("Pvalue", x)


#' Pvalue.bHP
#'
#' Extract the path of p-value of each iterated ADF HP-filter conduction for class 'bHP'.
#'
#' @param x an object of class "bHP"
#'
#' @return a vector recording p-value after each iterated HP-filter coonduction.
#' @export
#'
#' @examples
#'
#' lam <- 100 # tuning parameter for the annaul data
#'
#' data(IRE) # laod the data 'IRE'
#'
#' # by ADF
#'
#' bx_ADF <- BoostedHP(IRE, lambda = lam, iter = TRUE, test_type = "adf")
#' Pvalue(bx_ADF)
#'
#' # Retrun the value path of adf.
#' # Iterated number of HP filter: 19
#' # [1] 0.26932206 0.16154351 0.10943027 0.09301570 0.08624282 0.08172733 0.07880462 0.07692725
#' # [9] 0.07561611 0.07449014 0.07326910 0.07175650 0.06981805 0.06736339 0.06433257 0.06068690
#' # [17] 0.05640284 0.05146806 0.04785197
#'
#'
#' ### If the test type is not "adf", Pvalue.bHP will return error
#'
#' # raw HP filter
#'
#' bx_HP <- BoostedHP(IRE, lambda = lam, iter = FALSE)
#' Pvalue(bx_HP)
#'
#' # Error in Pvalue.bHP(bx_HP) :
#' # The stationary test type is none-iter, not ADF.
#'
#' # by BIC
#'
#' bx_BIC <- BoostedHP(IRE, lambda = lam, iter = TRUE, test_type = "BIC")
#' Pvalue(bx_BIC)
#'
#' # Error in Pvalue.bHP(bx_BIC) : The stationary test type is BIC, not ADF.
#'
#' # by none test type
#' # Iterated HP filter until Max_Iter and keep the path of BIC.
#'
#' bx_none <- BoostedHP(IRE, lambda = lam, iter = TRUE, test_type = "none")
#' Pvalue(bx_none)
#'
#' # Error in Pvalue.bHP(bx_none) : The stationary test type is none, not ADF.
Pvalue.bHP <- function(x) {
  if (x$test_type == "adf") {
    message("Retrun the value path of ", x$test_type, ".")
    message("Iterated number of HP filter: ", x$iter_num)

    return(x$adf_p_hist)
  }
  else {
    stop("The stopping criterion is ", x$test_type, ", not ADF.")
  }
}
