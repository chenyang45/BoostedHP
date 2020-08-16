#' Summary method for class \code{bHP}
#'
#' tables that summarize a \code{bHP} object.
#'
#' @param x an object of class \code{bHP}
#' @param digit controls the number of significant digits to print when printing
#'   numeric values. It is a suggestion only. Valid values are 1...22 with
#'   default 8. See the note in 'print.default' about values greater than 15.
#'
#'
#' @return summary of raw data, trend component, the number of iterations, p-value, etc.
#' @export
#'
#' @examples lam <- 100 # tuning parameter for the annual data
#'
#' data(IRE) # load the data 'IRE'
#'
#' # raw HP filter
#' bx_HP <- BoostedHP(IRE, lambda = lam, iter= FALSE)
#'
#' # by BIC
#' bx_BIC <- BoostedHP(IRE, lambda = lam, iter= TRUE, stopping = "BIC")
#'
#' # by ADF
#' bx_ADF <- BoostedHP(IRE, lambda = lam, iter= TRUE, stopping = "adf")
#'
#' # by nonstop test type
#' # Iterated HP filter until Max_Iter and keep the path of BIC.
#'
#' bx_nonstop <- BoostedHP(IRE, lambda = lam, iter= TRUE, stopping = "nonstop")
#'
#' #--------- start to summary the content of bHP -----------------
#'
#' summary(bx_ADF)
#' summary(bx_BIC)
#' summary(bx_nonstop)
#' summary(bx_HP)
#'

summary.bHP <- function(x, digit = 8){

  options(digits=digit)

  message("This is a summary table of 'bHP' class (",x$stopping,").")

  cat("
==============================================================================================================
                                ",
      paste0("Summary Table of '",x$stopping,"' bHP"),
"
==============================================================================================================
",

"Length of the Data:",length(x$raw_data),
if(length(x$iter_num)>0){paste0("; Iterated Number: ", x$iter_num)},
if(x$stopping == "nonstop"){paste0("; Iterated Number: ", (length(x$BIC_hist)-1))},
if(x$stopping == "nonstop-iter"){"; Only Conduct HP-Filter once"},
"; Stopping Criterion: ", x$stopping,"\n", if(x$stopping == "adf"){paste0("P-value (last iteration): ",
round(x$adf_p_hist[x$iter_num],4))}, if(x$stopping == "BIC"){paste0("BIC value (last iteration): ",
round(x$BIC_hist[x$iter_num],4))},"\n",
"------------------------------------------------------------------------------------------------------------","\n",
"\n",
"Raw Data","\n",
"---------","\n",
#"Min. ", "1st Qu.", "Median", "Mean ", "3rd Qu.", "Max. ", "\n",
summary(matrix(x$raw_data)),"\n",


"\n",
"Final Trend Component","\n",
"----------------------","\n",
#"Min. ", "1st Qu.", "Median", "Mean ", "3rd Qu.", "Max. ", "\n",
summary(matrix(x$trend)),"\n",


#----------- start with the path of p-value or BIC -----------------
if(x$stopping == "adf" | x$stopping == "BIC"){"\n"},
if(x$stopping == "adf" | x$stopping == "BIC"){
  "------------------------------------------------------------------------------------------------------------"
},

if(x$stopping == "adf" | x$stopping == "BIC"){"\n"},

# the path of ADF
 if(x$stopping == "adf"){
    "Path of P-value (head):"},
 if(x$stopping == "adf"){
 head(round(x$adf_p_hist,4))},

# the path of BIC
if(x$stopping == "BIC"){
  "Path of BIC (head):"},
if(x$stopping == "BIC"){
  head(round(x$BIC_hist[1:(x$iter_num)],4))},

if(x$stopping == "adf" | x$stopping == "BIC"){"\n"},


# the path of ADF
if(x$stopping == "adf"){
  "Path of P-value (tail):"},
if(x$stopping == "adf"){
  tail(round(x$adf_p_hist,4))},

if(x$stopping == "BIC"){
  "Path of BIC (tail):"},
if(x$stopping == "BIC"){
  tail(round(x$BIC_hist[1:(x$iter_num)],4))},
"
==============================================================================================================
")

}


