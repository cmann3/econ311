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
#' @import xts
#' @importFrom lubridate year month
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
#' @import xts
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
