# Boosting the Hodrick-Prescott Filter
# by Peter Phillips and Zhentao Shi (2018)
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
#' @param x an object of class "bHP"
#' @param lambda the turning parameter, default value is 1600.
#' @param iter logical parameter, TRUE (default) is to conduct iterated HP-filter, FALSE is not.
#' @param test_type the type for creterion: none-iter, adf, BIC, none (default).
#' @param sig_p significant p-value, default value is 0.050.
#' @param Max_Iter maximum iterated time, default value is 100.
#'
#' @return cycle component, trend component, raw data, iterated number, p-value or BIC.
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
#'


BoostedHP <- function(x, lambda = 1600, iter= TRUE, test_type = "none", sig_p = 0.050, Max_Iter = 100) {


  # Require Package: tseries, expm
  #--------------------------------------------------------------------------------------
  # Inputs
  #   x: a univariate time series
  #   lambda: the tuning parameter in the HP filter (base learner). Default is 1600.
  #   iter: logical.
  #       If iter = FALSE, the function returns the simple HP filter (fit only once).
  #       If iter = TRUE, the boosted HP filter.
  #   test_type (stopping criterion):
  #       If ="adf" or "BIC", the two stopping criteria elaborated in the paper.
  #       If = "none", iterated until Max_Iter
  #   sig_p: the significance level of the ADF test as the stopping criterion.
  #           It is useful only when test_type == "adf".
  #   Max_Iter: the maximum number of iterations.
  #--------------------------------------------------------------------------------------
  # Outputs
  #   $cycle: the cyclical components in the final round
  #   $trend: the trend component in the final round
  #   $trend_hist: the estimated trend in each iteration
  #   $iter_num: the total number of iterations
  #   $IC_hist: the path of the information criterion along the iterations
  #   $adf_p_hist: the path of the ADF test p-value along the iterations





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


