#' Lag of a Time Series
#'
#' \code{L} shifts the values in an \code{xts} time series by the specified
#' number of lags. The function extends the \code{lag} function by allowing
#' users to specify a frequency with which to lag the variable.
#'
#' @param x `xts` time series variable
#' @param lag numeric or character value indicating the number of periods to
#'   lag x. Defaults to one lag. Character inputs indicate the type of time
#'   period to lag. \code{lag} accepts 'y' years, 'q' quarters, 'm' months,
#'   'w' weeks, and 'd' days. Number and character values can be combined.
#'
#' @return new 'xts' object shifted by the \code{lag} number of periods.
#' @import xts
#' @export
#'
#' @examples
#' Y <- pwt(country = 'USA', series = 'gdp')
#' l_Y <- L(Y)       # GDP lagged by 1 period (quarter)
#' y_Y <- L(Y, 'y')  # GDP lagged by 1 year
#' m_Y <- L(Y, '6m') # GDP lagged by 6 months
#' #Table(Y, l_y, y_Y, m_Y)
L <- function(x, lag=1){
  if (is.numeric(lag)){
    k <- lag
    if (k == 1){nam <- ""
    } else {nam <- k}
  } else {
    nlag <- nchar(lag)
    freq <- substr(periodicity(x)$scale, 1, 1)
    if (nlag == 1){
      num <- 1
      dfreq <- lag
      nam <- lag
    } else if (nlag > 1){
      num <- as.numeric(substr(lag, 1, nlag-1))
      dfreq <- substr(lag, nlag, nlag)
      if (num == 1){nam <- dfreq
      } else {nam <- lag}
    }
    freqNumber_ <- function(x){
      z <- switch(x,
                  'y' = 1,
                  'q' = 4,
                  'm' = 12,
                  'w' = 52,
                  'd' = 365)
      z
    }
    numer <- freqNumber_(freq)
    denom <- freqNumber_(dfreq)
    k <- round(num*(numer/denom), digits = 0)

  }
  z <- lag(x, k=k)
  colnames(z) <- paste0('L', nam, '.', colnames(x))
  z
}
#' Difference of a Time Series
#'
#' \code{D} calculates the difference between an \code{xts} time series and
#' its lag.
#'
#' @param x `xts` time series variable
#' @param lag numeric or character value indicating the number of periods to
#'   lag x. Defaults to one lag. Character inputs indicate the type of time
#'   period to lag. \code{lag} accepts 'y' years, 'q' quarters, 'm' months,
#'   'w' weeks, and 'd' days. Number and character values can be combined.
#' @param diff number of times the \code{x} is differenced
#'
#' @return differenced 'xts' object.
#' @export
#'
#' @examples
#' Y <- pwt(country = 'USA', series = 'gdp')
#' d_Y <- D(Y)           # GDP differenced by 1 period (quarter)
#' y_Y <- D(Y, 'y')      # GDP differenced by 1 year
#' D_Y <- D(Y, diff = 2) # GDP differenced twice
#' #Table(Y, d_y, y_Y, D_Y)
D <- function(x, lag=1, diff=1){
  z <- x
  for (i in 1:diff){
    z <- z - L(z, lag)
  }
  if (lag == 1){lNam <- NULL
  } else {lNam <- lag}
  if (diff == 1){dNam <- NULL
  } else {dNam <- paste0("(", diff, ")")}
  colnames(z) <- paste0('D', dNam, lNam, ".", colnames(x))
  z
}
#' Growth Rate of a Time Series
#'
#' \code{G} calculates the growth rate of an 'xts' time series.
#'
#' @param x `xts` time series variable
#' @param lag numeric or character value indicating the number of periods
#'   between which the time series grew
#'
#' @return 'xts' object of growth rates.
#' @export
#'
#' @examples
#' Y <- pwt(country = 'USA', series = 'gdp')
#' g_Y <- G(Y)      # 1 period growth rate of GDP
#' y_Y <- G(Y, 'y') # yearly growth rate of GDP
#' m_Y <- G(Y, '6m') # 6 month growth rate of GDP
#' #Table(Y, g_y, y_Y, m_Y)
G <- function(x, lag=1){
  z <- D(x, lag=lag, diff=1)/L(x,lag=lag)*100
  if (lag == 1){gNam <- NULL
  } else {gNam <- lag}
  colnames(z) <- paste0('G',gNam,'.', colnames(x))
  if (is.null(attr(x,'units')) || tolower(attr(x,'units')) != 'percent'){
    attr(z,"units") <- "Percent"
  } else {attr(z,"units") <- "%^2"}
  z
}
#' Log Difference of a Time Series
#'
#' \code{lnD} calculates the log-difference of an 'xts' time series.
#'
#' @param x `xts` time series variable
#' @param lag numeric or character value indicating the number of periods to
#'   lag x. Defaults to one lag. Character inputs indicate the type of time
#'   period to lag. \code{lag} accepts 'y' years, 'q' quarters, 'm' months,
#'   'w' weeks, and 'd' days. Number and character values can be combined.
#' @param diff number of times the \code{x} is differenced
#'
#' @return log-differenced 'xts' object.
#' @export
#'
#' @examples
#' Y <- pwt(country = 'USA', series = 'gdp')
#' d_Y <- lnD(Y)           # GDP differenced by 1 period (quarter)
#' y_Y <- lnD(Y, 'y')      # GDP differenced by 1 year
#' D_Y <- lnD(Y, diff = 2) # GDP differenced twice
#' #Table(Y, d_y, y_Y, D_Y)
lnD <- function(x,lag=1,diff=1){
  z <- log(x)
  for (i in 1:diff){
    z <- z - L(z, lag=lag)
  }
  if (lag == 1){lNam <- NULL
  } else {lNam <- lag}
  if (diff == 1){dNam <- NULL
  } else {dNam <- paste0("(", diff, ")")}
  colnames(z) <- paste0('lnD', dNam, lNam, ".", colnames(x))
  if (!is.null(attr(x,'units'))){
    attr(z,'units') <- paste0('ln.',attr(x,'units'))
  }
  z
}
