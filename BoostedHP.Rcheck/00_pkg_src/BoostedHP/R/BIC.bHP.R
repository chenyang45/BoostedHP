#' BIC.bHP
#'
#' Extract the path of BIC value of each iterated BIC HP-filter conduction for class 'bHP'.
#' As 'none' type of bHP also keep BIC for each iteration time till the max_iter, BIC.bHP method also
#' return BIC value for 'none' bHP.
#'
#' @param x an object of class "bHP"
#'
#' @return a vector recording BIC after each iterated HP-filter coonduction.
#' @export
#'
#' @examples
#'
#' lam <- 100 # tuning parameter for the annaul data
#'
#' data(IRE) # laod the data 'IRE'
#'
#' # by BIC
#'
#' bx_BIC <- BoostedHP(IRE, lambda = lam, iter= TRUE, test_type = "BIC")
#'
#' BIC(bx_BIC)
#'
#' #Retrun the value path of BIC.
#' #Iterated number of HP filter: 5
#' #Keep the path of BIC till iterated 6 times to show the tuning point.
#' #[1] 1.586255 1.366335 1.293931 1.264323 1.254397 1.254620
#'
#' bx_none <- BoostedHP(IRE, lambda = lam, iter= TRUE, test_type = "none")
#'
#' BIC(bx_none)
#'
#' #Retrun the BIC path of none.
#' #Iterated number of HP filter: 99
#' #Keep the path of BIC till iterated 100 times to show the tuning point.
#' #[1] 1.586255 1.366335 1.293931 1.264323 1.254397 1.254620 1.260345 1.269139 1.279670 1.291179
#' #[11] 1.303223 ...
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
#' bx_ADF <- BoostedHP(IRE, lambda = lam, iter= TRUE, test_type = "adf")
#'
#' BIC(bx_ADF)
#'
#' #Error in BIC.bHP(bx_ADF) :
#' #The stationary test type is adf, not BIC or none.
#'
#'
#'

BIC.bHP <- function(x){

  if(x$test_type == "BIC" | x$test_type == "none"){

  message("Retrun the BIC path of ", x$test_type, ".")
  message("Iterated number of HP filter: ",x$iter_num)
  message("Keep the path of BIC till iterated ", (x$iter_num+1), " times to show the tuning point.")

  #print(x$BIC_hist)

  return(x$BIC_hist)

  }
  else {
    stop("The stationary test type is ",x$test_type, ", not BIC or none.")
    }
}


