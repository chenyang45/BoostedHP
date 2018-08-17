library(stats)
## Extends the example for 'switch'
center <- function(x, type = c("mean", "median", "trimmed")) {
  type <- match.arg(type)
  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = .1))
}
x <- rcauchy(10)
center(x, "t")       # Works
center(x, "med")     # Works
try(center(x, "m"))  # Error
stopifnot(identical(center(x),       center(x, "mean")),
          identical(center(x, NULL), center(x, "mean")) )



match.arg <- function (arg, choices, several.ok = FALSE) 
{
  if (missing(choices)) {
    formal.args <- formals(sys.function(sysP <- sys.parent()))
    choices <- eval(formal.args[[as.character(substitute(arg))]], 
                    envir = sys.frame(sysP))
  }
  if (is.null(arg)) 
    return(choices[1L])
  else if (!is.character(arg)) 
    stop("'arg' must be NULL or a character vector")
  if (!several.ok) {
    if (identical(arg, choices)) 
      return(arg[1L])
    if (length(arg) > 1L) 
      stop("'arg' must be of length 1")
  }
  else if (length(arg) == 0L) 
    stop("'arg' must be of length >= 1")
  i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
  if (all(i == 0L)) 
    stop(gettextf("'arg' should be one of %s", paste(dQuote(choices), 
                                                     collapse = ", ")), domain = NA)
  i <- i[i > 0L]
  if (!several.ok && length(i) > 1) 
    stop("there is more than one match in 'match.arg'")
  choices[i]
}











## Allowing more than one match:
match.arg(c("gauss", "rect", "ep"),
          c("gaussian", "epanechnikov", "rectangular", "triangular"),
          several.ok = TRUE)


