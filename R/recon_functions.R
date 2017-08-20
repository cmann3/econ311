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
  z <- lag(x, k=k) # change to xts::lag or something. doesn't work with dplyr (due to lag/lead)
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
#' @importFrom zoo as.yearqtr
#' @import xts
#' @import zoo
#' @export
#'
#' @examples
#' urate <- fred('UNRATE', as_xts=TRUE) # monthly Civilian Unemployment Rate data
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
#' GDP <- fred('GDP', as_xts=TRUE)  # Nominal GDP
#' rGDP <- trans('GDP', 'r', opt = fred('GDPDEF'))
#' GDPgap <- trans(rGDP, 'pdv', opt = fred('GDPPOT'))
#'
#' Treas <- fred('GS10', as_xts=TRUE)         # 10 year Treasury Yield
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

alignLeft_ <- function(x, n){
  extraSpace <- n - nchar(x)
  if (extraSpace > 0){x <- paste0(x, repChar(" ", extraSpace))}
  x
}
alignRight_ <- function(x, n){
  extraSpace <- n - nchar(x)
  if (extraSpace > 0){x <- paste0(repChar(" ", extraSpace), x)}
  x
}
alignCenter_ <- function(x, n){
  extraSpace <- n - nchar(x)
  extraLeft <- floor(extraSpace/2)
  extraRight <- ceiling(extraSpace/2)
  if (extraLeft > 0){leftSpace <- repChar(" ", extraLeft)
  } else {leftSpace <- ""}
  if (extraRight > 0){rightSpace <- repChar(" ", extraRight)
  } else {rightSpace <- ""}
  x <- paste0(leftSpace, x, rightSpace)
  x
}
alignDecimal_ <- function(x, n){
  #if (!grepl(".", x, fixed = TRUE)){right <- right + 1}
  z <- strsplit(x, ".", fixed = TRUE)
  if (is.na(z[[1]][1])){extraLeft <- n[1]
  } else {extraLeft <- n[1] - nchar(z[[1]][1])}
  if (length(z[[1]]) == 1){
    if (is.na(z[[1]][1]) || z[[1]][1] == ""){extraRight <- n[2]
    } else {extraRight <- n[2] + 1}
  } else if (length(z[[1]]) == 2){extraRight <- n[2] - nchar(z[[1]][2])
  } else if (length(z[[1]]) == 0){extraRight <- n[2]}
  if (extraLeft > 0){leftSpace <- repChar(" ", extraLeft)
  } else {leftSpace <- ""}
  if (extraRight > 0){rightSpace <- repChar(" ", extraRight)
  } else {rightSpace <- ""}
  x <- paste0(leftSpace, x, rightSpace)
  x
}
alignTrunc_ <- function(x, n, trim = 'r'){
  nCut <- nchar(x) - n
  if (nCut > 0){
    if (trim == 'r'){
      x <- substr(x, 1, n)
    } else if (trim == 'l'){
      x <- substr(x, nCut, nchar(x))
    }
  } else if (nCut < 0){
    x <- paste0(x, repChar(" ", abs(nCut)))
  }
  x
}
maxCharDec_ <- function(x, rows){
  z <- strsplit(x, ".", fixed = TRUE)
  beforeDec <- vector('numeric', length = rows)
  afterDec  <- vector('numeric', length = rows)
  for (i in 1:rows){
    if (is.na(z[[i]][1])){
      beforeDec[i] <- 0
      afterDec[i] <- 0
    } else {
      nSplit <- length(z[[i]])
      if (nSplit == 2){
        beforeDec[i] <- nchar(z[[i]][1])
        afterDec[i]  <- nchar(z[[i]][2])
      } else if (nSplit == 1){
        firstLetter <- substr(x[i], 1, 1)
        if (firstLetter == "."){
          beforeDec[i] <- 0
          afterDec[i]  <- nchar(z[[i]][1])
        } else {
          beforeDec[i] <- nchar(z[[i]][1])
          afterDec[i]  <- 0
        }
      } else {stop('Too many decimal points in each value.')}
    }
  }
  n <- vector('numeric', length = 2)
  n[1] <- max(beforeDec)
  n[2] <- max(afterDec)
  n
}
alignCol_ <- function(x, align, nameWidth = NULL, rows = NULL, width = NULL, ...){
  if (is.null(nameWidth)){nameWidth <- 0}
  if (!is.character(x)){x <- as.character(x)}
  if (is.null(rows)){rows <- length(x)}
  x <- sapply(x, function(y){
    if (is.na(y)){y <- '.'}
    y
  })
  if (align == 'd'){
    n <- maxCharDec_(x, rows)
    maxWidth <- n[1]+n[2]+1
    if (maxWidth < nameWidth){
      extraSpace <- nameWidth - maxWidth
      n[1] <- n[1] + ceiling(extraSpace/2)
      n[2] <- n[2] + floor(extraSpace/2)
      maxWidth <- n[1]+n[2]+1
    }
  } else if (align == 'm'){
    n <- min(nchar(x))
    if (n < nameWidth){n <- nameWidth}
  } else if (align == 't'){
    if (is.null(width)){width = ceiling(getOption('width')/2)}
    n <- width
  } else {
    n <- max(nchar(x))
    if (is.na(n)){n <- 1}
    if (n < nameWidth){n <- nameWidth}
  }
  maxN  <- ifelse(align == 'd', maxWidth+1, n+1)
  align <- switch(align,
                  'l' = 'alignLeft_',
                  'r' = 'alignRight_',
                  'c' = 'alignCenter_',
                  'd' = 'alignDecimal_',
                  'm' = 'alignTrunc_',
                  't' = 'alignTrunc_')
  for (i in 1:rows){
    if (is.na(x[i])){x[i] = '.'}
    x[i] <- do.call(align, list(x[i], n, ...))
  }
  Z = list(dat = x, max = maxN)
  Z
}
alignColName_ <- function(x, align, colChar, ncols, ...){
  for (i in 1:ncols){
    alignCall <- switch(align[i],
                        'l' = 'alignLeft_',
                        'r' = 'alignRight_',
                        'c' = 'alignCenter_',
                        'd' = 'alignCenter_',
                        'm' = 'alignTrunc_',
                        't' = 'alignTrunc_')
    if (is.na(x[i]) || is.null(x[i])){x[i] = ''}
    x[i] <- do.call(alignCall, list(x[i], colChar[i+1]-1, ...))
  }
  if (colChar[1] > 0){
    rowcol <- repChar(" ", colChar[1]-1)
    x <- c(rowcol, x)
  }
  x
}
getColType_ <- function(x, nCol){
  for (i in 1:nCol){
    tx <- typeof(x[,i])
    if (!is.null(attr(x[,i], 'Date'))){z <- substr(toupper(attr(x[,i], 'Date')), 1, 1)
    } else if (tx == 'double' && !is.null(attr(x[,i], 'units'))){
      atr <- tolower(attr(x[,i], 'units'))
      if (grepl('percent', atr)){z <- '%'
      } else if (grepl('rate', atr)){z <- 'Rate'
      } else if (grepl('ratio', atr)){z <- 'Ratio'
      } else if (grepl('bil', atr)){z <- 'Bill'
      } else if (grepl('mil', atr)){z <- 'Mill'
      } else if (grepl('thou', atr)){z <- 'Thou'
      } else if (grepl('unit', atr)){z <- 'Unit'
      } else if (grepl('person', atr)){z <- 'Pers'
      } else if (grepl('index', atr)){z <- 'Indx'
      } else {z <- substr(atr,1,3)}
    } else {
      z <- switch(tx,
                  'double' = 'dbl',
                  'integer' = 'int',
                  'character' = 'chr',
                  'factor' = 'fctr',
                  'date' = 'date')
    }
    if (i == 1){Z <- paste0("<",z,">")
    } else {Z <- c(Z, paste0("<",z,">"))}
  }
  Z
}
alignFullDF_ <- function(x, align='auto', digits = 3, trunc = 'long', rows=TRUE, sub=NULL, sep=NULL, ...){
  nRow <- nrow(x)
  nCol <- ncol(x)
  colNames <- colnames(x)
  # Get sub if needed
  if (!is.null(sub) && sub == 'type'){sub <- getColType_(x, nCol)}
  # Get Alignment of Columns
  if ('auto' %in% align){
    Align    <- rep('c', length = nCol)
  } else if (length(align) == 1){
    Align <- rep(align, length = nCol)
  } else if (length(align) < nCol){
    diffAlign  <- nCol - length(align)
    extraAlign <- rep('l', length = diffAlign)
    Align      <- c(align, extraAlign)
  } else {Align <- align}
  # By column, round or turn to string
  for (i in 1:nCol){
    if (is.numeric(x[,i])){
      if ('auto' %in% align){Align[i] <- 'd'}
      x[,i] <- round(x[,i], digits = digits)
      x[,i] <- as.character(x[,i])
    } else if (!is.character(x[,i])){x[,i] <- as.character(x[,i])}
  }
  # Align DF
  colChar <- vector('numeric', length = nCol+1)
  if (is.numeric(rows)){
    rowCol      <- alignCol_(as.character(c(1:6,rows:(rows+5))), 'l', rows = nRow)
    alignedDF   <- rowCol[['dat']]
    colChar[1]  <- rowCol[['max']]
  } else if (rows == TRUE){
    rowCol      <- alignCol_(as.character(1:nRow), 'l', rows = nRow)
    alignedDF   <- rowCol[['dat']]
    colChar[1]  <- rowCol[['max']]
  } else {colChar[1] <- 0}
  for (i in 1:nCol){
    z <- alignCol_(x[,i], Align[i], nchar(colNames[i]), nRow) # add ... back
    colChar[i+1] <- z[['max']]
    if (exists('alignedDF')){alignedDF <- cbind(alignedDF, z[['dat']])
    } else {alignedDF <- z[['dat']]}
  }
  z <- alignedDF
  # Align Column Names (and any sub header)
  colNames <- alignColName_(colNames, Align, colChar, nCol)
  if (!is.null(sub)){sub <- alignColName_(sub, Align, colChar, nCol)}
  if (!is.null(sep)){
    Sep <- rep(sep, nCol)
    Sep <- alignColName_(Sep, Align, colChar, nCol)
    nsep <- nchar(sep)
    extraSpace <- colChar[1] - 1 - nsep
    if (extraSpace > 0){Sep[1] <- paste0(sep, repChar(" ", extraSpace))
    } else {Sep[1] <- sep}
  }
  if (rows){rowcol <- repChar(" ", colChar[1]-1)} else{rowcol <- ""}
  # Truncate DF if needed
  width <- getOption('width')
  sumChar <- sum(colChar)
  if (width <= sumChar){
    if (trunc == 'col'){
      cumChar <- cumsum(colChar)
      lastCol <- which(cumChar > (width - 1))[1] - 1
      z <- z[,1:lastCol]
      colNames <- colNames[1:(lastCol)]
      if (!is.null(sub)){sub <- sub[1:(lastCol)]}
      if (!is.null(sep)){Sep <- Sep[1:(lastCol)]}
      maxWidth <- cumChar[lastCol]
    } else {
      if (trunc == 'long'){trunc <- which(colChar == max(colChar))[1]
      } else if (is.numeric(trunc)){trunc <- trunc + 1
      } else if (trunc %in% colnames(x)){trunc <- which(colnames(x) == trunc) + 1}
      toCut <- sumChar - width + 1
      newWidth <- colChar[trunc] - toCut
      for (i in 1:nRow){
        z[i,trunc] <- alignTrunc_(z[i,trunc], newWidth)
      }
      colNames[trunc] <- alignTrunc_(colNames[trunc], newWidth)
      maxWidth <- width - 1
    }
  } else {maxWidth <- sumChar}
  # Print Results
  x_table <- paste0(" ", repChar("=", maxWidth),"\n")
  x_table <- c(x_table, colNames, '\n')
  if (!is.null(sub)){x_table <- c(x_table, sub,'\n')}
  x_table <- c(x_table, paste0(repChar("-", maxWidth),"\n"))

  if (is.null(sep)){
    for (i in 1:nRow){
      x_table <- c(x_table, paste(z[i,]), '\n')
    }
  } else {
    for (i in 1:sep){
      x_table <- c(x_table, paste(z[i,]), '\n')
    }
    x_table <- c(x_table, paste0(repChar(".", maxWidth),"\n"))
    for (i in (sep+1):nrow(z)){
      x_table <- c(x_table, paste(z[i,]), '\n')
    }
  }
  x_table <- c(x_table, paste0(repChar("-", maxWidth),"\n"))
  x_table
}
#' Pretty Print Data
#'
#' \code{Table} formats and prints data. Rows and columns are truncated to
#' fit on screen and columns are aligned.
#'
#' @param x object to be printed
#' @param n maximum number of rows to print. If 'ht', head and tail are printed
#' @param align vector of characters describing column alignment. Accepts
#'   'l' for left, 'r' for right, 'c' for center, 'd' for aligning on
#'   decimal place, 't' for left alignment and truncation, 'm' for truncating
#'   to the shortest character. 'auto' aligns center for characters and on
#'   the decimal place for numeric values.
#' @param digits number of digits to which numeric values are rounded
#' @param trunc character or numeric value. Character values of 'long' or
#'   'col' are accepted. 'long' truncates the widest column if necessary,
#'   'col' removes necessary columns. Numeric values specify which columns to
#'   truncate.
#' @param rows logical. Are row numbers printed?
#' @param sub vector to be printed under column names. If 'type', the type of
#'   each column is printed
#' @param start starting row number of the data to print
#' @param sep defaults to NULL. Row number after which to draw a line
#
#' @return prints truncated table of data
#' @export
#'
#' @examples
#' GDP <- fred('GDP')
#' Table(GDP)
#'
#' data(pwt9)
#' Table(pwt9, sub = 'type', trunc = 'col')
Table <- function(x, n=30, align='auto', digits = 3, trunc = 'long',
                  rows=TRUE, sub=NULL, start = 1, sep = NULL){
  UseMethod('Table')
}
#' @export
Table.data.frame <- function(x, n=30, align='auto', digits = 3, trunc = 'long', rows=TRUE, sub=NULL, start = 1, sep = NULL){
  nRow <- nrow(x)
  if (is.numeric(n)){
    if (nRow < n){n <- nRow}
    clnam <- colnames(x)
    x <- x[start:(start+n-1),]
    colnames(x) <- clnam
  } else if (n == 'ht'){
    if (nRow > 20){
      x <- rbind(x[1:6,], x[(nRow-5):nRow,])
      sep <- 6
      rows <- nRow-5
    }
  }
  cat(alignFullDF_(x, align, digits, trunc, rows, sub, sep))
}
#' @export
Table.matrix <- function(x, n=30, align='auto', digits = 3, trunc = 'long', rows=TRUE, sub=NULL, start = 1, sep = NULL){
  nRow <- nrow(x)
  if (is.numeric(n)){
    if (nRow < n){n <- nRow}
    clnam <- colnames(x)
    x <- as.matrix(x[start:(start+n-1),])
    colnames(x) <- clnam
  } else if (n == 'ht'){
    if (nRow > 20){
      x <- rbind(x[1:6,], x[(nRow-5):nRow,])
      sep <- 6
      rows <- nRow-5
    }
  }
  cat(alignFullDF_(x, align, digits, trunc, rows, sub, sep))
}
#' @export
Table.xts <- function(x, n='ht', align='auto', digits = 3, trunc = 'long', rows=TRUE, sub='type', start = 1, sep = NULL){
  nRow <- nrow(x)
  dte <- as.Date(index(x))
  freq <- attr(x, 'frequency')
  Date <- vector('character', length = nRow)
  for (i in 1:nRow){
    Date[i] <- printDate(dte[i], substr(tolower(freq),1,1))
  }
  X <- data.frame(Date,x)
  if (is.numeric(n)){
    if (nRow < n){n <- nRow}
    X <- X[start:(start+n-1),]
  } else if (n == 'ht'){
    if (nRow > 20){
      X <- rbind(X[1:6,], X[(nRow-5):nRow,])
      sep <- 6
      rows <- nRow-5
    }
  }
  for (i in 1:ncol(x)){
    if (!is.null(attr(x[,i], 'units'))){
      attr(X[,i+1], 'units') <- attr(x[,i], 'units')
    }
  }
  attr(X[,1], 'Date') <- freq
  cat(alignFullDF_(X, align, digits, trunc, rows, sub, sep))
}
