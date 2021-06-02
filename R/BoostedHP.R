#' Boosting the Hodrick-Prescott Filter
#'
#' All in one function of conducting the boosted HP-filter.
#'
#' @param x is a raw time series to be filtered.
#' @param lambda the turning parameter, default value is 1600,
#'   as recommended by Hodrick and Prescott (1997) for quarterly data.
#' @param iter logical, \code{TRUE} (default) to conduct the boosted HP filter.
#'   FALSE does not iterated, which is exactly the original HP filter.
#' @param stopping stopping criterion. \code{"BIC"} (default), or \code{"adf"}, or \code{"nonstop"} means keeping
#'    iteration until the maximum number of iteration, specified by \code{Max_Iter} is reached.
#' @param sig_p a threshold of the p-value for the ADF test, with default value 0.050.
#'    Only effective when \code{stopping = "adf"}.
#' @param Max_Iter maximal number of iterations. The default is 100.
#'
#' @return The function returns a list containing the following items:
#' \item{cycle}{The cyclical component in the final iteration.}
#' \item{trend}{The trend component in the final iteration.}
#' \item{trend_hist}{The estimated trend in each iteration.}
#' \item{iter_num}{The total number of iterations when it stops.}
#' \item{IC_hist}{The path of the BIC up to the final iterations.}
#' \item{adf_p_hist}{The path of the ADF test p-value up to the final iteration.}

#' @details
#'
#' This is the main function of implementing the boosted HP filter (Phillisp and
#' Shi, 2021). The arguments accommendate the orginal HP filter (\code{iter =
#' FALSE}), the boosted HP filter with the BIC stopping criterion (\code{stopping =
#' "BIC"}),
#' or ADF test stopping criterion
#' (\code{stopping = "adf"}),  or keep going until the maximum number of iterations is reached
#' (\code{stopping = "nonstop"}).
#'
#' Either the original HP filter or the bHP filter requires \code{lambda} to
#' control the strength of the weak learner for in-sample fitting. The default
#' is \code{lambda = 1600}, which is recommended by Hodrick and Prescott (1997)
#' for quarterly data. \code{lambda} should be adjusted for different
#' frequencies. For example, \code{lambda = 129600} for monthly data and
#' \code{lambda = 6.25} for annual data.
#'
#' See the vignette with a brief introduction of the idea of bHP.
#'
#' @references
#'
#' Peter Phillips and Zhentao Shi, 2021: "Boosting: Why You Can Use the HP Filter," International Economic Review, 62(2), 521-570
#'
#'
#' @export
#'
#'
#' @examples
#'
#' data(IRE) # load the data 'IRE'
#' lam <- 100 # tuning parameter for the annual data
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
#' # If stopping = "nonstop",
#' # Iterated HP filter until Max_Iter and keep the path of BIC.
#'
#' bx_nonstop <- BoostedHP(IRE, lambda = lam, iter= TRUE, stopping = "nonstop")


BoostedHP <- function(x, lambda = 1600, iter= TRUE, stopping = "BIC", sig_p = 0.050, Max_Iter = 100) {

  if (!is.numeric(x) || anyNA(x) ) {
    stop("The raw time series is not numeric or it contains NAs: returning NA")
    return(NA_real_)
  }

  ## generating trend operator matrix "S"
  raw_x <- x # save the raw data before HP
  n <- length(x) # data size

  I_n <-  diag(n)
  D_temp <- rbind(matrix(0, 1, n), diag(1, n - 1, n))
  D_temp <- (I_n - D_temp) %*% (I_n - D_temp)
  D <- t( D_temp[3:n, ] )

  S <- solve( I_n + lambda * D %*% t(D) ) # Equation 4 in PJ
  mS = diag(n) - S



  ## the simple HP-filter
  if(iter==FALSE){
    # message("Original HP filter.")

    # get the trend and cycle
    x_f <- S %*% x
    x_c <- x - x_f
    result <- list(cycle = x_c, trend_hist = x_f,
                   stopping = "nonstop", trend = x - x_c, raw_data = raw_x)

  }

  ####################################################

  ## the boosted HP filter
  if(iter==TRUE) {

    # if (stopping == "adf"){
    #   # message("bHP with ADF stopping criterion.")
    # } else if ( stopping == "BIC"){
    #   # message( "bHP with BIC stopping criterion.")
    #   # message( "Save the path of BIC till iter+1 times to show the 'turning point' feature of choosen iteration time in BIC history.")
    # }  else if ( stopping == "nonstop" ) {
    #   # message( "bHP filter until Max_Iter and keep the path of BIC.")
    # }

    ### ADF test as the stopping criterion
    if (stopping =="adf") {

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

        if(stopping == "adf")   stationary <- (adf_p_r <= sig_p)
        
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
        warning("The number of iterations exceeds Max_Iter.
                The residual cycle remains non-stationary.")
      }

      result <- list(cycle = x_c, trend_hist = x_f,  stopping = stopping,
                     signif_p = sig_p, adf_p_hist= adf_p, iter_num = R,
                     trend  = x - x_c, raw_data = raw_x)
    } else { # either BIC or nonstopping

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

        if  ( (r >= 2) & (  stopping == "BIC") )  {
          if (  IC[r-1] < IC[r] )   { break  }
        }

      } # end of the while loop

      # final assignment
      R = r - 1;
      x_f <- as.matrix(x_f[, 1:R])
      x_c <- x - x_f[,R]

      if(stopping == "BIC"){
        # save the path of BIC till iter+1 times to keep the "turning point" of BIC history.
        result <- list(cycle = x_c, trend_hist = x_f,  stopping = stopping,
                       BIC_hist = IC[1:(R+1)], iter_num = R, trend =  x- x_c, raw_data = raw_x)
      }

      if(stopping == "nonstop"){

        result <- list(cycle = x_c, trend_hist = x_f,  stopping = stopping,
                       BIC_hist = IC,iter_num = Max_Iter-1, trend =  x- x_c, raw_data = raw_x)
      }
    }
  } # end the boosted HP

  attr(result,'class')<-'bHP' # assign the class
  return(result)
}


