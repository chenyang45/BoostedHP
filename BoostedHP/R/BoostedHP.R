# Boosting the Hodrick-Prescott Filter
# by Peter Phillips and Zhentao Shi (2019)
#
#================================================================
# R version 3.4.3 (2017-11-30) -- "Kite-Eating Tree"
# Copyright (C) 2017 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Date: 2018-04-22
# by Zhentao Shi, zhentao.shi@cuhk.edu.hk
#    Chen Yang,   chen_yang@link.cuhk.edu.hk
#
# ===============================================================
#
# Version 0.0.5
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



#' Boosting the Hodrick-Prescott Filter
#'
#' all in one function of conducting iterated HP-filter for types: none-iter, adf, BIC, none.
#'
#' @param x is a time series to be filtered.
#' @param lambda the turning parameter, default value is 1600, as recommended by Hodrick-Prescott for quarterly data.
#' @param iter logical parameter, TRUE (default) is to conduct iterated HP-filter, FALSE does not iterated so is the same as the original HP filter.
#' @param test_type stopping criterion. "adf", or "BIC" (default), "none".
#' @param sig_p a threshold of the p-value below which the iteration will stop. default value is 0.050. only effective when test_type = adf
#' @param Max_Iter maximal number of iterations. The default value is 100.
#'
#' @return The function returns a list containing the following items
#' \item{cycle}{The cyclical component in the final iteration.}
#' \item{trend}{The trend component in the final iteration.}
#' \item{trend_hist}{The estimated trend in each iteration.}
#' \item{iter_num}{The total number of iterations when it stops.}
#' \item{IC_hist}{The path of the BIC through the iterations.}
#' \item{adf_p_hist}{The path of the ADF test p-value through the iterations}

#' @details
#' Given time series data \eqn{ x_{t}:t=1,\ldots,n }
#' the HP method decomposes the series into
#' two additive components --- a trend component \eqn{ f_{t} }
#' and a residual or cyclical component \eqn{   c_{t}  }, estimated as
#' \deqn{(\hat{f}_{t}^{HP} )
#' =\arg\min_{ (f_{t} )}  \{ \sum_{t=1}^{n} (x_{t}-f_{t} )^{2}
#' +\lambda\sum_{t=2}^{n} (\Delta^ 2 f_{t}  )^{2} \},}
#' and
#' \deqn{ (\hat{c}_{t}^{HP} )=( x_t-\hat{f}_{t}^{HP}) }
#' where \eqn{\Delta f_{t}=f_{t}-f_{t-1}},
#' and \eqn{\Delta^2 f_{t}= \Delta f_{t}- \Delta f_{t-1} = f_{t}- 2 f_{t-1} + f_{t-2}},
#' and \eqn{\lambda\geq 0}
#' is a tuning parameter that controls the extent of the penalty.
#'
#'
#' @export
#'
#'
#' @examples
#' library(tseries)
#'
#' data(IRE) # load the data 'IRE'
#' lam <- 100 # tuning parameter for the annaul data
#'
#' # raw HP filter
#' bx_HP <- BoostedHP(IRE, lambda = lam, iter= FALSE)
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


BoostedHP <- function(x, lambda = 1600, iter= TRUE, test_type = "BIC", sig_p = 0.050, Max_Iter = 100) {


  # Require Package: tseries, expm


  if (!is.numeric(x) || anyNA(x) ) {
    stop("Argument is not numeric or containing NAs: returning NA")
    return(NA_real_)
  }



  # POSIXct (date/time) index

  ## generating trend operator matrix "S"
  raw_x <- x # save the raw data before HP
  n <- length(x) # data size

  I_n <-  diag(n)
  D_temp <- rbind(matrix(0, 1, n), diag(1, n - 1, n))
  D_temp <- (I_n - D_temp) %*% (I_n - D_temp)
  D <- t( D_temp[3:n, ] )

  # Equation 4 in PJ
  S <- solve( I_n + lambda * D %*% t(D) )
  mS = diag(n) - S



  ## the simple HP-filter

  if(iter==FALSE){

    message("Conducted the simple HP filter.")

    # get the trend and cycle
    x_f <- S %*% x
    x_c <- x - x_f
    result <- list(cycle = x_c, trend_hist = x_f,test_type = "none-iter", trend = x - x_c, raw_data = raw_x)

  }

  ####################################################

  ## the boosted HP filter


  if(iter==TRUE) {


    if (test_type == "adf"){
      message("Iterated HP filter with ADF test criterion.")
    } else if ( test_type == "BIC"){
      message( "Iterated HP filter with BIC criterion.")
      message( "Save the path of BIC till iter+1 times to show the 'turning point' feature of choosen iteration time in BIC history.")
    }  else if ( test_type == "none" ) {
      message( "Iterated HP filter until Max_Iter and keep the path of BIC.")
    }



    ### ADF test as the stopping criterion
    if (test_type =="adf"  ) {

      r <- 1
      stationary <- FALSE
      x_c <- x

      x_f <- matrix(0, n, Max_Iter)
      adf_p <- rep(0, Max_Iter)

      while( (r <= Max_Iter) & (stationary == FALSE)){

        x_c <- ( diag(n) - S ) %*% x_c # update
        x_f[, r] <- x - x_c

        adf_p_r <- (tseries::adf.test(x_c, alternative = "stationary"))$p.value
        # x_c is the residual after the mean and linear trend being removed by HP filter
        # we use the critical value for the ADF distribution with
        # the intercept and linear trend specification

        adf_p[r] <- adf_p_r

        sig_p = sig_p # + 0.001 # due to the way that R reports the p-value
        if(test_type == "adf")   stationary <- (adf_p_r <= sig_p)


        # Truncate the storage matrix and vectors
        if(stationary == TRUE){
          R <- r
          x_f <- x_f[, 1:R]
          adf_p <- adf_p[1:R]
          break
        }

        r <- r + 1
      } # end the while loop

      if( r > Max_Iter ){
        R <- Max_Iter
        warning("The number of iterations exceeds the limit.
                The residual cycle remains non-stationary.")
      }

      result <- list(cycle = x_c, trend_hist = x_f,  test_type = test_type,
                     signif_p = sig_p, adf_p_hist= adf_p, iter_num = R,
                     trend  = x - x_c, raw_data = raw_x)
      } else  {

        # assignment
        r <- 0
        x_c_r <- x
        x_f <- matrix(0, n, Max_Iter)
        IC <- rep(0, Max_Iter)
        IC_decrease = TRUE


        I_S_0 = diag(n) - S
        c_HP = I_S_0 %*% x
        I_S_r = I_S_0


        while( r < Max_Iter ) {
          r <- r + 1

          x_c_r = I_S_r %*% x  # this is the cyclical component after m iterations
          x_f[, r] = x - x_c_r
          B_r <- diag(n) -  I_S_r
          IC[r] =   var (x_c_r ) / var( c_HP ) +  log( n )/ (n - sum(diag (S) ) ) * sum( diag( B_r ) )

          I_S_r = I_S_0 %*% I_S_r # update for the next round

          if  ( (r >= 2) & (  test_type == "BIC") )  {
            if (  IC[r-1] < IC[r] )   { break  }
          }

        } # end of the while loop

        # the message



        # final assignment
        R = r - 1;
        x_f <- as.matrix(x_f[, 1:R])
        x_c <- x - x_f[,R]
        # browser()


        if(test_type == "BIC"){
          # save the path of BIC till iter+1 times to keep the "turning point" of BIC history.
          result <- list(cycle = x_c, trend_hist = x_f,  test_type = test_type,
                         BIC_hist = IC[1:(R+1)], iter_num = R, trend =  x- x_c, raw_data = raw_x)

        }

        if(test_type == "none"){

        result <- list(cycle = x_c, trend_hist = x_f,  test_type = test_type,
                       BIC_hist = IC,iter_num = Max_Iter-1, trend =  x- x_c, raw_data = raw_x)

        }



      }

  } # end the boosted HP

  attr(result,'class')<-'bHP'

  return(result)
}


