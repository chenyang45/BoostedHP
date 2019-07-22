#' Summary method for class 'bHP'
#'
#' table of the summary special for class 'bHP'.
#'
#' @param x an object of class "bHP"
#' @param digit controls the number of significant digits to print when printing numeric values. It is a suggestion only. Valid values are 1...22 with default 8. See the note in 'print.default' about values greater than 15.
#'
#'
#' @return summary of raw dta, trend component, iterated number, p-value ...
#' @export
#'
#' @examples lam <- 100 # tuning parameter for the annaul data
#'
#' data(IRE) # laod the data 'IRE'
#'
#' # raw HP filter
#' bx_HP <- BoostedHP(x, lambda = lam, iter= FALSE)
#'
#' # by BIC
#' bx_BIC <- BoostedHP(IRE, lambda = lam, iter= TRUE, test_type = "BIC")
#'
#' # by ADF
#' bx_ADF <- BoostedHP(IRE, lambda = lam, iter= TRUE, test_type = "adf")
#'
#' # by none test type
#' # Iterated HP filter until Max_Iter and keep the path of BIC.
#'
#' bx_none <- BoostedHP(IRE, lambda = lam, iter= TRUE, test_type = "none")
#'
#' #--------- start to summary the content of bHP -----------------
#'
#' summary(bx_ADF)
#' summary(bx_BIC)
#' summary(bx_none)
#' summary(bx_HP)
#'

summary.bHP <- function(x, digit = 8){

  options(digits=digit)

  message("This is a summary table of 'bHP' class (",x$test_type,").")

  cat("
==============================================================================================================
                                ",
      paste0("Summary Table of '",x$test_type,"' bHP"),
"
==============================================================================================================
",

"Length of the Data:",length(x$raw_data),
if(length(x$iter_num)>0){paste0("; Iterated Number: ", x$iter_num)},
if(x$test_type == "none"){paste0("; Iterated Number: ", (length(x$BIC_hist)-1))},
if(x$test_type == "none-iter"){"; Only Conduct HP-Filter once"},
"; Stopping Criterion: ", x$test_type,"\n", if(x$test_type == "adf"){paste0("P-value (last iteration): ",
round(x$adf_p_hist[x$iter_num],4))}, if(x$test_type == "BIC"){paste0("BIC value (last iteration): ",
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
if(x$test_type == "adf" | x$test_type == "BIC"){"\n"},
if(x$test_type == "adf" | x$test_type == "BIC"){
  "------------------------------------------------------------------------------------------------------------"
},

if(x$test_type == "adf" | x$test_type == "BIC"){"\n"},

# the path of ADF
 if(x$test_type == "adf"){
    "Path of P-value (head):"},
 if(x$test_type == "adf"){
 head(round(x$adf_p_hist,4))},

# the path of BIC
if(x$test_type == "BIC"){
  "Path of BIC (head):"},
if(x$test_type == "BIC"){
  head(round(x$BIC_hist[1:(x$iter_num)],4))},

if(x$test_type == "adf" | x$test_type == "BIC"){"\n"},

#if(x$test_type == "adf" | x$test_type == "BIC"){
#  "------------------------------------------------------------------------------------------------------------"
#},



# the path of ADF
if(x$test_type == "adf"){
  "Path of P-value (tail):"},
if(x$test_type == "adf"){
  tail(round(x$adf_p_hist,4))},

if(x$test_type == "BIC"){
  "Path of BIC (tail):"},
if(x$test_type == "BIC"){
  tail(round(x$BIC_hist[1:(x$iter_num)],4))},

#if(x$test_type == "adf" | x$test_type == "BIC"){"\n"},
"
==============================================================================================================
")

}


