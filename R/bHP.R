#' bHP: Package for the boosted HP filter
#'
#' The boosted HP filter by Phillips and Shi (2019)
#'
#' \code{BoostedHP} is the main function. It generates a \code{bHP} object which can work with
#' generic methods.
#'
#' @references
#'
#' Phillips, Peter CB, and Zhentao Shi.
#' "Boosting: Why you can use the hp filter."
#' arXiv: 1905.00175, Cowles Foundation Discussion Paper No.2192, (2019).
#'
#' @docType package
#' @name bHP
NULL



#' Ireland Annual GDP
#'
#' @usage data(IRE)
#'

#' @format
#' \itemize{
#'  \item\strong{Release:} {Gross Domestic Product}
#'  \item\strong{Frequency:} {Annual}
#'  \item\strong{Date Range:} {1981--2016}
#' }
#'
#' @section Descripton:
#' This dataset is described in Section 4.1 of Philips and Shi (2019).
#' Also See Okun, Ball, Leigh, and Loungani (2017).
#'
#'
#' @source OECD Stat \url{https://stats.oecd.org/}
#'
#' @references
#'
#' Phillips, Peter CB, and Zhentao Shi.
#' "Boosting: Why you can use the hp filter."
#' arXiv: 1905.00175, Cowles Foundation Discussion Paper No.2192, (2019).
#'
#' Ball, Laurence, Daniel Leigh, and Prakash Loungani.
#'   "Okun's law: Fit at 50?."
#'   Journal of Money, Credit and Banking 49, no. 7 (2017): 1413-1441.
#'
#'
#' @docType data
"IRE"
