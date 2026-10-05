#' A Robust Version of \code{seas} that \emph{Always} Works (Experimental)
#'
#' \code{robust.seas} has exactly the same usage as \code{\link{seas}}, the main
#' function of seasonal, but will always work. If \code{seas} is failing,
#' \code{robust.seas} tries an an alternative specification, and returns a
#' message. \code{robust.seas} currently works with all 3000+ series of the M3
#' forecast competition.
#'
#' If \code{seas} fails, \code{robust.seas} tries the following:
#'
#' \enumerate{
#' \item adjusting invalid start year
#' \item increasing \code{maxiter} to 10000 (if suggested in the error)
#' \item switch to X-11
#' \item fix model to (0 1 1)(0 1 1)
#' \item turn off AIC testing
#' \item turn off outlier detection
#' }
#'
#' If \code{robust.seas} is applied to annual series (in which case you do not
#' need seasonal adjustment), it will proceed slightly different:
#'
#' \enumerate{
#' \item adjusting invalid start year
#' \item ensure SEATS, X11 and AIC testing are off
#' \item turn off outlier detection
#' \item fix model to (1 1 0)
#' }
#'
#' If it still fails, it will return an error, along with the suggestion to post the example on:
#' \url{https://github.com/christophsax/seasonal/wiki/Breaking-Examples-(and-Possible-Solutions)}.
#'
#' @return returns an object of class \code{"seas"}.
#' @seealso \code{\link{seas}} for the main function of seasonal.
#' @param ... arguments passed on to \code{\link{seas}}.
#' @export
#' @examples
#' \dontrun{
#'
#' x <- ts(1:40) # an annual time series with an invalid start year
#'
#' # not working
#' seas(x)
#'
#' # working
#' robust.seas(x)
#'
#'
#' ### X-13 in the  IJF-M3 forecast competition
#'
#' # Original analysis by Peter Ellis
#' # http://ellisp.github.io/blog/2015/12/21/m3-and-x13/
#'
#' library(Mcomp) # IJF-M3 forecast competition data
#' library(parallel) # part of R base, but needs to be loaded
#'
#' # using cluster parallelization, which also works on Windows
#' # (on Linux and Mac, you could simply use mclapply)
#'
#' # a) set up cluster
#' cl <- makeCluster(detectCores())
#'
#' # b) load 'seasonal' and 'Mcomp' for each node
#' clusterEvalQ(cl, {
#'   library(seasonal)
#'   library(Mcomp)
#' })
#'
#' # c) run in parallel (60 sec on an older Macbook Pro with 8 cores)
#' ff <- function(e) {
#'   m <- robust.seas(e$x, forecast.save = "fct", forecast.maxlead = 18, seats = NULL)
#'   series(m, "forecast.forecasts")[, 1]
#' }
#' lx13 <- parLapply(cl, M3, ff)
#'
#' # d) stop the cluster
#' stopCluster(cl)
#' }
robust.seas <- function(...) {
  rcl <- match.call(definition = seas)
  cl <- as.list(rcl)
  cl[[1]] <- as.name("seas")
  cenv <- parent.frame()

  x <- eval(cl[["x"]], envir = cenv)

  retry <- function(...) {
    # overrides replace existing args in cl, so fallbacks accumulate and win
    # over user-supplied values
    o <- list(...)
    cl[names(o)] <<- o
    z <- try(eval(as.call(cl), envir = cenv), silent = TRUE)
    if (!inherits(z, "try-error")) {
      z$call <- rcl
    }
    z
  }

  z <- retry()
  if (!inherits(z, "try-error")) {
    return(z)
  }

  tt <- tsp(x)
  if (!is.null(tt) && tt[1] < 1000) {
    message("adjusting invalid start year (+ 2000 years)")
    tsp(x)[1:2] <- tsp(x)[1:2] + 2000
    cl[["x"]] <- x # substitute the fixed series into the call
    z <- retry(regression.aictest = NULL)
    if (!inherits(z, "try-error")) {
      return(z)
    }
  } else if (is.null(tt)) {
    # not a time series: nothing we can fix, rethrow the seas error
    stop(z)
  }

  if (frequency(x) == 1) {
    # a bunch of tweaks for annual series
    message("annual series: ensure SEATS, X11 and AIC testing are off")
    z <- retry(seats = NULL, x11 = NULL, regression.aictest = NULL)
    if (!inherits(z, "try-error")) {
      return(z)
    }

    message("annual series: turn off outlier detection")
    z <- retry(outlier = NULL)
    if (!inherits(z, "try-error")) {
      return(z)
    }

    message("annual series: fix model to (110)")
    z <- retry(arima.model = c(1, 1, 0))
    if (!inherits(z, "try-error")) {
      return(z)
    }
  }

  # only do maxiter fix if suggested in the error; it stays active for all
  # subsequent retries. older x13 said 'maxiter', current says
  # 'maximum iterations reached'
  if (grepl("maxiter|maximum iterations", z, ignore.case = TRUE)) {
    message("increasing maxiter")
    z <- retry(estimate.maxiter = 10000)
    if (!inherits(z, "try-error")) {
      return(z)
    }
  }

  message("use X-11")
  z <- retry(x11 = "")
  if (!inherits(z, "try-error")) {
    return(z)
  }

  message("fix model to (0 1 1)(0 1 1)")
  z <- retry(arima.model = c(0, 1, 1, 0, 1, 1))
  if (!inherits(z, "try-error")) {
    return(z)
  }

  message("turn off aic testing")
  z <- retry(regression.aictest = NULL)
  if (!inherits(z, "try-error")) {
    return(z)
  }

  message("turn off outlier detection")
  z <- retry(outlier = NULL)
  if (!inherits(z, "try-error")) {
    return(z)
  }

  message(
    "Unfortunatly, this call still returns an error. If you
want to help out, please post your call and the data on
the Github Wiki:\n
https://github.com/christophsax/seasonal/wiki/Breaking-Examples-(and-Possible-Solutions)\n
You can use the dput() function to copy/paste your time
series data. Thanks.\n"
  )

  stop(z)
}
