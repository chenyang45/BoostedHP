% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{BIC.bHP}
\alias{BIC.bHP}
\title{BIC.bHP}
\usage{
\method{BIC}{bHP}(x)
}
\arguments{
\item{x}{an object of class \code{bHP}}
}
\value{
a vector recording BIC after each iteration of bHP.
}
\description{
Extract the path of BIC value of each iterated BIC HP-filter conduction for
class \code{bHP}. As \code{nonstop} type of bHP also keeps BIC for each iteration time till
the \code{Max_iter}, BIC.bHP method returns BIC value for it as well.
}
\examples{
lam <- 100 # tuning parameter for the annual data

data(IRE) # load the data 'IRE'

# by BIC

bx_BIC <- BoostedHP(IRE, lambda = lam, iter= TRUE, stopping = "BIC")

BIC(bx_BIC)


bx_none <- BoostedHP(IRE, lambda = lam, iter= TRUE, stopping = "nonstop")

BIC(bx_none)



### If the test type is not "adf", Pvalue.bHP will return error

# raw HP filter

bx_HP <- BoostedHP(IRE, lambda = lam, iter= FALSE)

BIC(bx_HP)

# Error in BIC.bHP(bx_HP) :
# The stationary test type is none-iter, not BIC or none.


# by ADF
bx_ADF <- BoostedHP(IRE, lambda = lam, iter= TRUE, stopping = "adf")

BIC(bx_ADF)

#Error in BIC.bHP(bx_ADF) :
#The stationary test type is adf, not BIC or none.

}
