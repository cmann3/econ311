#' Repeat Character in String
#'
#' \code{repChar} repeats a character for a single value.
#'
#' @param x characters to repeats
#' @param n number of times to repeat
#' @param nl logical determining whether a newline should be added
#
#' @return character of length \code{nchar(x)*n}
#' @export
#'
#' @examples
#' repChar('=', 10)
repChar <- function(x, n, nl = FALSE){
  z <- paste(rep(x, n), collapse="")
  if (nl == TRUE){z <- paste0(z,'\n')}
  z
}
#' Pretty Print Dates
#'
#' \code{printDate} truncates and cleans dates values according to frequency.
#'
#' @param x character or Date value
#' @param freq frequency to print. Accepts 'y', 'q', 'm', or 'd'
#' @param format if \code{x} is a character, specifies the date format
#
#' @return character value
#' @importFrom lubridate year month
#' @export
#'
#' @examples
#' printDate('1947-01-01', freq='q')
#' printDate('1965-05-30', freq='m')
printDate <- function(x, freq, format=NULL){
  if (inherits(x, "Date")){
    if (is.null(format)){x <- as.Date(x)
    } else {x <- as.Date(x, format = format)}
  }
  if (nchar(freq) > 1){
    freq <- tolower(substr(freq, 1, 1))
  }
  if (freq == 'y'){z <- year(x)
  } else if (freq == 'm'){
    y <- year(x)
    m <- month(x)
    if (m < 10){m <- paste0('0',m)}
    z <- paste0(y,'-',m)
  } else if (freq == 'q'){
    y <- year(x)
    m <- month(x)
    if (m == 1){q <- 1
    } else if (m == 4){q <- 2
    } else if (m == 7){q <- 3
    } else if (m == 10){q <- 4}
    z <- paste0(y,'-Q',q)
  } else {z <- x}
  z
}
#' Convert to Specific Frequency
#'
#' \code{freq} converts an 'xts' time series object to a different time
#' frequency.
#'
#' @param x 'xts' object
#' @param freq frequency which to convert. Accepts 'y', 'q', 'm', 'w',
#'   or 'd' for dates. Also accepts 'h', '30m', '15m', '10m', '5m', '3m',
#'   and 'mn' for times.
#' @param OHLC logical describing whether to keep the Open, High, Low, and
#'   Close of data after conversion. Defaults to FALSE.
#
#' @return 'xts' object
#' @import xts zoo lubridate
#' @export
#'
#' @examples
#' urate <- fred('UNRATE') # monthly Civilian Unemployment Rate data
#' freq(urate, 'y')        # yearly Unemployment Rate
freq <- function(x, freq='q', OHLC=FALSE){
  if (freq == 'y' | freq == 'a'){z = to.yearly(x,OHLC=OHLC)
  } else if (freq == 'q'){z = to.quarterly(x,OHLC=OHLC)
  } else if (freq == 'm'){z = to.monthly(x,OHLC=OHLC)
  } else if (freq == 'w'){z = to.weekly(x,OHLC=OHLC)
  } else if (freq == 'd'){z = to.daily(x,OHLC=OHLC)
  } else if (freq == 'h'){z = to.hourly(x,OHLC=OHLC)
  } else if (freq == '30m'){z = to.minutes30(x,OHLC=OHLC)
  } else if (freq == '15m'){z = to.minutes15(x,OHLC=OHLC)
  } else if (freq == '10m'){z = to.minutes10(x,OHLC=OHLC)
  } else if (freq == '5m'){z = to.minutes5(x,OHLC=OHLC)
  } else if (freq == '3m'){z = to.minutes3(x,OHLC=OHLC)
  } else if (freq == 'mn'){z = to.minutes(x,OHLC=OHLC)}
  z
}
#' Transform Data
#'
#' \code{trans} transforms an object (an 'xts' for all options) by a
#' specific formula.
#'
#' @param x 'xts' object
#' @param type type of transformation to perform.
#'   \itemize{
#'     \item \code{log} log transformation
#'     \item \code{l} lag of time series
#'     \item \code{g} growth rate of time series
#'     \item \code{gy} yearly growth rate
#'     \item \code{d} difference of time series
#'     \item \code{dy} yearly difference
#'     \item \code{dv} difference between \code{x} and \code{opt}
#'     \item \code{pdv} percent diff. between \code{x} and \code{opt}
#'     \item \code{ldv} log diff. \code{x} and \code{opt}
#'     \item \code{i} \code{x} is indexed to \code{opt}=100
#'     \item \code{r} transformed to Real by \code{opt}, defaults to CPI
#'     \item \code{s} share of \code{opt}, defaults to nominal GDP
#'   }
#' @param opt additional options passed to the transformation
#
#' @return transformed 'xts' object
#' @export
#'
#' @examples
#' GDP <- fred('GDP')  # Nominal GDP
#' rGDP <- trans('GDP', 'r', opt = fred('GDPDEF'))
#' GDPgap <- trans(rGDP, 'pdv', opt = fred('GDPPOT'))
#'
#' Treas <- fred('GS10')         # 10 year Treasury Yield
#' rTreas <- trans(Treas, 'r')   # Real interest rate
#' trans(rTreas, 'i', '2011-01') # indexed to Jan 2011 = 100
#'
trans <- function(x, type=NULL, opt=NULL){
  if (type == 'log' | type == 'ln'){
    x = log(x)
    attr(x,"transform") = 'log'
  } else if (type == 'l' | type == 'lag'){
    if (is.null(opt)) {opt=1}
    x = L(x, lag = opt)
  } else if (type == 'g' | type == 'grow' | type == 'pchange'){
    if (is.null(opt)) {opt=1}
    x = G(x, lag = opt)
  } else if (type == 'gy' | type == 'gyear' | type == 'pyear'){
    if (is.null(opt)) {opt=1}
    x = G(x, lag = 'y')
  } else if (type == 'd' | type == 'diff' | type == 'change'){
    if (is.null(opt)) {opt=1}
    x = D(x, lag = opt)
  } else if (type == 'dy' | type == 'diffyear' | type == 'changeyear'){
    if (is.null(opt)) {opt=1}
    x = D(x, lag = 'y')
  } else if (type == 'dv'){
    x = x - opt
  } else if (type == 'pdv'){
    x = (x - opt)/opt * 100
    attr(x, 'units') <- 'Percent'
  } else if (type == 'ldv'){
    x = log(x) - log(opt)
  } else if (type == 'i' | type == 'index'){
    x = suppressWarnings(x/as.numeric(x[opt])*100)
    attr(x, 'units') <- paste0('Index (',opt,'=100)')
  } else if (type == 'r' | type == 'real'){
    if (is.null(opt)){opt = fred('CPIAUCSL')}
    if (attr(x,"units") == 'percent'){x = ((1+x)/(1+G(opt)))-1
    } else {x = x/opt*100}
  } else if (type == 's' | type == 'share'){
    if (is.null(opt)){opt = fred('GDP')}
    x = x/opt*100
    attr(x, 'units') <- 'Percent'
  }
  x
}
