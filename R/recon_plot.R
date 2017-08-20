#' Scatter Plot Time Series
#'
#' \code{splot} plots two economic time series against each other, taking into
#' consideration object attributes such as 'units' and 'title'. splot works
#' best with data collecting functions, such as \code{fred} in the the
#' \code{recon} package.
#'
#' @param x 'xts' object to be plotted on the x-axis
#' @param y 'xts' object to be plotted on the y-axis
#' @param data data object containing \code{x} and \code{x}
#' @param by object of factors to be plotted. Only 'cycle' supported for now,
#'   which plots by the business cycle
#' @param time time period to restrict plot. Uses 'xts' convention, e.g.,
#'   '1970-01/1972-06-03'.
#' @param theme graphical theme of the plot. Only 'std' is supported.
#' @param leg vector of character names to use in the legend. 'auto' uses
#'   object attributes
#' @param title character title of plot
#' @param reg logical or character. Should a regression line by printed? If
#'   'by', then a regression for each category.
#'
#' @section Graphical Parameters:
#' If \code{NULL}, then value is chosen either by \code{theme} or with object
#' attributes.
#' @param type 'p' for points, 'l' for lines, 'b' for both
#' @param pch type of point to use
#' @param xlab character label of x-axis
#' @param ylab character label of y-axis
#' @param xlim Date vector of limits on the x-axis
#' @param ylim numeric vector of limits on the y-axis
#' @param col character vector of color names to use for lines and points
#' @param colBG background color
#' @param colGrid color of grids
#' @param colPlot color of plot background
#' @param colAxes color of plot axes
#' @param lty line type. examples: 'solid', 'dotted', 'dashed'
#' @param lwd line width
#' @param las tick label orientation on axis
#' @param cex size of objects in plot
#' @param cex.lab size of labels
#' @param cex.axis size of axis
#' @param mar inner margins outside of plot. c(bottom,left,top,right)
#' @param bg background color of points
#' @param rlwd regression line width
#'
#' @return plot of time-series
#' @importFrom grDevices adjustcolor
#' @import graphics
#' @import scales
#' @export
#'
#' @examples
#' GDP <- fred('GDPC1', as_xts=TRUE)
#' GDPpot <- fred('GDPPOT', as_xts=TRUE)
#' GDPgap <- (GDP-GDPpot)/GDPpot*100
#'
#' urate <- fred('UNRATE', as_xts=TRUE)
#' un <- fred('NROU', as_xts=TRUE)
#' cycU <- urate-un
#'
#' splot(GDPgap, cycU, reg=T)
#' splot(GDPgap, cycU, by='cycle')

sPlot <- function(x, y, by=NULL, data=NULL,
                  title = NULL, xlab = NULL, ylab = NULL,
                  time = NULL, ylim = NULL, xlim = NULL,
                  type = 'p', col = NULL, lwd = 1, lty = NULL, colAxes = NULL,
                  pch = NULL, cex = 1, colBG = NULL, bg = colBG,
                  colPlot = NULL, colGrid = NULL, theme = 'std',
                  reg = NULL, rlwd = lwd+1, mar = c(4, 4, 2, 1),
                  cex.lab = 1, cex.axis = 0.75, las = 0, leg = NULL){
  # Themes ====================================================================
  if (theme == 'std'){
    if (is.null(colBG)){colBG <- 'white'}
    if (is.null(colGrid)){colGrid <- 'lightgrey'}
    if (is.null(colAxes)){colAxes <- 'black'}
    if (is.null(colPlot)){colPlot <- 'white'}
    if (is.null(col)){col <- c('black', brewer_pal('qual', palette = 6)(8))}
    if (is.null(lty)){lty <- rep('solid', 8)}
    if (is.null(pch)){pch <- shape_pal()(6)}
  }
  # Get Data ==================================================================
  xTmp <- with(data, x)
  yTmp <- with(data, y)
  if (is.null(xlab)){
    if (!is.null(attr(xTmp, 'title'))){xlab <- attr(xTmp, 'title')
    } else {xlab <- deparse(substitute(x))}
  }
  if (is.null(ylab)){
    if (!is.null(attr(yTmp, 'title'))){ylab <- attr(yTmp, 'title')
    } else {ylab <- deparse(substitute(y))}
  }
  z <- na.omit(merge(xTmp, yTmp))
  if (!is.null(time)){z <- z[time]}
  if (is.null(xlim)){
    xMin <- min(na.omit(xTmp))
    xMax <- max(na.omit(xTmp))
    xlim <- c(xMin, xMax)
  }
  if (is.null(ylim)){
    yMin <- min(na.omit(yTmp))
    yMax <- max(na.omit(yTmp))
    ylim <- c(yMin, yMax)
  }
  # Set Par ===================================================================
  if (!is.null(leg) || !is.null(by)){mar[4] <- mar[4] + 6}
  par(bg = colBG, cex.lab = cex.lab, cex.axis = cex.axis,
      las = las, mar = mar)
  # Plot no By ================================================================
  if (is.null(by)){
    z <- as.matrix(na.omit(z))
    # Plot
    plot(1, type = 'n', xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab)
    grid(nx=NULL, ny=NULL, col=colGrid)
    axis(1)
    axis(2)
    points(z[,1], z[,2], type=type, col=col[1], lwd=lwd, lty=lty[1],
           pch = pch[1], cex = cex, bg = bg)
    if (!is.null(reg)){
      abline(lm(z[,2]~z[,1]), lty=lty[1], col=col[2], lwd=rlwd)
    }
  }
  # Do By =====================================================================
  if (!is.null(by)){
    if (by == 'cycle'){
      cfreq <- substr(periodicity(index(z))[6], 1, 1)
      if (cfreq == 'q'){cycles <- fred('USRECQ')
      } else if (cfreq == 'y'){
        cycles <- fred('USRECQ')
        cycles <- freq(cycles, 'y')
      } else if (cfreq == 'm'){cycles <- fred('USREC')
      } else if (cfreq == 'd'){cycles <- fred('USRECD')
      } else {
        cycles <- fred('USRECD')
        cycles <- freq(cycles, cfreq)
      }
      z <- as.matrix(na.omit(merge(z, cycles)))
      Z <- list()
      Z[[1]] <- subset(z, z[,3] == 1)
      Z[[2]] <- subset(z, z[,3] == 0)
      # Plot
      plot(1, type = 'n', xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab)
      grid(nx=NULL, ny=NULL, col=colGrid)
      axis(1)
      axis(2)
      points(Z[[1]][,1], Z[[1]][,2], type=type, col=col[2], lwd=lwd, lty=lty[1],
             pch = pch[1], cex = cex, bg = bg)
      points(Z[[2]][,1], Z[[2]][,2], type=type, col=col[3], lwd=lwd, lty=lty[1],
             pch = pch[2], cex = cex, bg = bg)
      if (!is.null(reg) && reg != 'by'){
        abline(lm(z[,2]~z[,1]), lty=lty[1], col=col[1], lwd=rlwd)
      } else if (!is.null(reg) && reg == 'by'){
        abline(lm(Z[[1]][,2]~Z[[1]][,1]), lty=lty[1], col=col[2], lwd=rlwd)
        abline(lm(Z[[2]][,2]~Z[[2]][,1]), lty=lty[1], col=col[3], lwd=rlwd)
      }
      if (is.null(leg)){leg <- c('Recession', 'Expansion')}
      legend('left', inset=c(1.05,0), legend=leg, pch=pch[1], col=col[2:3],
             xpd=TRUE, seg.len=1.25, cex=0.75, lwd=1)
    }
    byTmp <- with(data, by)
    # do more stuff!
  }
  # Title =====================================================================
  if (!is.null(title)){title(main = title)}
  # Reset Par ===============================================================
  par(bg = 'white', cex.lab = 1, cex.axis = 1,
      las = 0, mar = c(5,4,4,2)+0.1)
}
#' Plot Time Series
#'
#' \code{tplot} plots economic time series data, taking into consideration
#' object attributes such as 'units' and 'title'. tplot works best with data
#' collecting functions, such as \code{fred} in the the \code{recon} package.
#'
#' @param ... 'xts' objects to be plotted
#' @param right 'xts' object to be plotted on the right axis
#' @param data data object containing \code{...} and \code{right}
#' @param time time period to restrict plot. Uses 'xts' convention, e.g.,
#'   '1970-01/1972-06-03'.
#' @param bars object containing Peaks and Troughs to plot. 'cycle' (default)
#'   plots business cycle bars
#' @param theme graphical theme of the plot. Only 'std' is supported.
#' @param leg vector of character names to use in the legend. 'auto' uses
#'   object attributes
#' @param title character title of plot
#'
#' @section Graphical Parameters:
#' If \code{NULL}, then value is chosen either by \code{theme} or with object
#' attributes.
#' @param xlab character label of x-axis
#' @param ylab character label of left y-axis
#' @param yRlab character label of right y-axis
#' @param xlim Date vector of limits on the x-axis
#' @param ylim numeric vector of limits on the left y-axis
#' @param yRlim numeric vector of limits on the right y-axis
#' @param ticks numeric vector of the desired number of ticks on (x,y) axis
#' @param col character vector of color names to use for lines
#' @param colAxes color of axes
#' @param colBG background color
#' @param colGrid color of grids
#' @param colPlot color of plot background
#' @param colBar color of bars (business cycle)
#' @param lty line type. examples: 'solid', 'dotted', 'dashed'
#' @param lwd line width
#' @param las tick label orientation on axis
#' @param cex size of objects in plot
#' @param cex.lab size of labels
#' @param cex.axis size of axis
#' @param ltyAxis line type of axis
#' @param lwdAxis line width of axis
#' @param lwdTicks line width of ticks
#' @param mar inner margins outside of plot. c(bottom,left,top,right)
#'
#' @return plot of time-series
#' @importFrom grDevices adjustcolor
#' @import graphics
#' @import scales
#' @export
#'
#' @examples
#' GDP <- fred('GDPC1', as_xts=TRUE)
#' GDPpot <- fred('GDPPOT', as_xts=TRUE)
#' GDPgap <- (GDP-GDPpot)/GDPpot*100
#'
#' urate <- fred('UNRATE', as_xts=TRUE)
#' un <- fred('NROU', as_xts=TRUE)
#' cycU <- urate-un
#'
#' tplot(GDPgap, cycU)
#' tplot(GDPgap, right = GDP)
tPlot = function(..., right=NULL, data=NULL, time=NULL,
                 xlim = NULL, ylim = NULL, yRlim = NULL, ticks = c(8,5),
                 theme = 'std', col = NULL, colBG = NULL, colGrid = NULL,
                 colAxes = NULL, colPlot = NULL, colBar = NULL,
                 mar = c(3.5, 3.5, 2, 1), bars = 'cycle', las = 0,
                 cex = 1, cex.lab = 1, cex.axis = 0.75, lty = NULL, lwd = 1,
                 xlab = 'Date', ylab = NULL, yRlab = NULL, title = NULL,
                 ltyAxis = 'solid', lwdAxis = 1, lwdTicks = lwdAxis,
                 leg = 'auto'){
  # Get Data ================================================================
  yNam <- deparse(substitute(c(...)))
  yNam <- unlist(strsplit(gsub(" ","", gsub(".*\\((.*)\\).*", "\\1", yNam)),","))
  tmp <- with(data, list(...))
  nleft <- length(tmp)
  nlines <- nleft
  if (!is.null(right)){nlines <- nlines + 1}
  z   <- tmp[[1]]
  if ('auto' %in% leg){
    autoLeg <- TRUE
    Leg <- vector('character', length = nlines)
  } else {autoLeg <- FALSE}
  if (autoLeg){
    if (!is.null(attr(tmp[[1]], 'title'))){Leg[1] <- attr(tmp[[1]], 'title')
    } else {Leg[1] <- yNam[1]}
  }
  if (is.null(ylab) && !is.null(attr(tmp[[1]], 'units'))){
    yLab <- attr(tmp[[1]], 'units')
  }
  if (nleft > 1){for(i in 2:nleft){
    z <- merge(z, tmp[[i]])
    if (is.null(ylab) && !is.null(attr(tmp[[i]], 'units'))){
      tmpLab <- attr(tmp[[i]], 'units')
      if (exists('yLab')){yLab <- c(yLab, tmpLab)
      } else {yLab <- tmpLab}
    }
    if (autoLeg){
      if (!is.null(attr(tmp[[i]], 'title'))){Leg[i] <- attr(tmp[[i]], 'title')
      } else {Leg[i] <- yNam[i]}
    }
  }}
  if (!is.null(right)){
    zRight <- with(data, right)
    z <- merge(z, zRight)
    axisRight <- TRUE
    if (is.null(yRlab)){
      if (!is.null(attr(zRight, 'units'))){yRlab <- attr(zRight, 'units')
      } else {yRlab <- ""}}
    if (autoLeg){
      if (!is.null(attr(zRight, 'title'))){Leg[nlines] <- attr(zRight, 'title')
      } else {Leg[nlines] <- deparse(substitute(right))}
    }
  } else {axisRight <- FALSE}
  if (autoLeg){leg <- Leg}
  if (is.null(ylab)){
    if (exists('yLab')){
      yLab <- unique(yLab)
      if (length(yLab) == 1){ylab <- yLab
      } else {
        yLab <- unique(sub("([A-Za-z]+).*", "\\1", yLab))
        ylab <- paste(yLab, collapse = ", ")
      }
    } else {ylab <- ""}
  }
  if (!is.null(time)){z <- z[time]}
  z <- na.omit(z)
  Dates <- as.Date(index(z))
  z <- as.matrix(z)
  if (axisRight){
    zRight <- z[,nleft+1]
    z <- z[,1:nleft]
    yRTick <- pretty(zRight, n = ticks[2])
  }
  # Limits ==================================================================
  if (is.null(xlim)){xlim <- c(min(Dates), max(Dates))}
  if (is.null(ylim)){ylim <- c(min(na.omit(z)), max(na.omit(z)))}
  if (axisRight && is.null(yRlim)){
    yRlim <- c(min(na.omit(zRight)), max(na.omit(zRight)))}
  # Ticks ===================================================================
  xTick <- pretty(Dates, n = ticks[1])
  yTick <- pretty(z, n = ticks[2])
  # Maybe minor ticks
  # Scale Right =============================================================
  if (axisRight){
    nyTick <- length(yTick)
    nrTick <- length(yRTick)
    transB <- (yTick[nyTick] - yTick[1]) / (yRTick[nrTick] - yRTick[1])
    transA <- yTick[1] - yRTick[1]*transB
    zR <- transA + transB*zRight
    #yRtick needs to be transformed
  }
  # Themes ==================================================================
  if (theme == 'std'){
    if (is.null(colBG)){colBG <- 'white'}
    if (is.null(colGrid)){colGrid <- 'lightgrey'}
    if (is.null(colAxes)){colAxes <- 'black'}
    if (is.null(colPlot)){colPlot <- 'white'}
    if (is.null(colBar)){colBar <- adjustcolor('lightsteelblue', 0.5)}
    if (is.null(col)){col <- c('black', brewer_pal('qual', palette = 6)(8))}
    if (is.null(lty)){lty <- rep('solid', 8)}
  }
  # Par =====================================================================
  if (axisRight){mar[4] <- mar[4] + 2.5}
  if (!is.null(leg)){mar[3] <- mar[3] + 0.5}
  if (!is.null(title)){oma <- c(0,0,1.75,0)
  } else {oma <- c(0,0,0.25,0)}
  par(bg = colBG, cex.lab = cex.lab, cex.axis = cex.axis,
      las = las, mar = mar, oma = oma)
  # Plot Axes ===============================================================
  plot(1, type = 'n', ylim = ylim, xlim = xlim,
       axes = FALSE, xlab = xlab, ylab = ylab, mgp=c(2.5,0.5,0))
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = colPlot)
  abline(v=xTick, h=yTick, col=colGrid, lty='dotted')
  axis.Date(1, at=xTick, labels = TRUE, tck = -0.03)
  axis(2, labels = TRUE, tck = -0.03)
  if (axisRight){
    if (axisRight){
      axis(4, at =yTick[2:(nyTick-1)], labels = yRTick[2:(nyTick-1)],
           tck = -0.02, mgp=c(2.5,0.5,0), las=0)
      mtext(side=4, line=2, yRlab, las=0)
    }
    mtext(side=4, line=2, yRlab, las=0)
  }
  # Plot Bars ===============================================================
  if (!is.null(bars)){
    if (bars == 'cycle'){
      Peak <- c('1899-06-01', '1902-09-01', '1907-05-01', '1910-01-01',
                '1913-01-01', '1918-08-01', '1920-01-01', '1923-05-01', '1926-10-01',
                '1929-08-01', '1937-05-01', '1945-02-01', '1948-11-01', '1953-07-01',
                '1957-08-01', '1960-04-01', '1969-12-01', '1973-11-01', '1980-01-01',
                '1981-07-01', '1990-07-01', '2001-03-01', '2007-12-01')
      Trough <- c('1900-12-31', '1904-08-31', '1908-06-30', '1912-01-31',
                  '1914-12-31', '1919-03-31', '1921-07-31', '1924-07-31', '1927-11-30',
                  '1933-03-31', '1938-06-30', '1945-10-31', '1949-10-31', '1954-05-31',
                  '1958-04-30', '1961-02-28', '1970-11-30', '1975-03-31', '1980-07-31',
                  '1982-11-30', '1991-03-31', '2001-11-30', '2009-06-30')
      Peak <- as.Date(Peak)
      Trough <- as.Date(Trough)
    } else {
      Peak <- as.Date(bars[1])
      Trough <- as.Date(bars[2])
    }
    rect(Peak,ylim[1]-abs(ylim[1]),Trough,ylim[2]+abs(ylim[2]), col=colBar, border=NA)
  }
  # Plot Bands ==============================================================
  # Plot Lines ==============================================================
  if (is.null(ncol(z))){
    lines(Dates,z,type='l',col=col[1], lwd=lwd, lty=lty[1])
  } else {
    for (i in 1:nleft){
      lines(Dates, z[,i], type='l',col=col[i], lwd=lwd, lty=lty[i])
    }
  }
  if (axisRight){
    lines(Dates, zR, type='l', col=col[nlines], lwd=lwd, lty=lty[nlines])
  }
  # Add Legend ==============================================================
  if (!is.null(leg)){
    legend('top', inset=c(0,-0.1), legend=leg, lty=lty, col=col,
           xpd=TRUE, seg.len=1.25, cex=0.75, horiz=TRUE, lwd=2)
  }
  # Add Title ===============================================================
  if (!is.null(title)){
    title(main = title, outer = TRUE)
  }
  # Reset Par ===============================================================
  par(bg = 'white', cex.lab = 1, cex.axis = 1,
      las = 0, mar = c(5,4,4,2)+0.1, oma = c(0,0,0,0))
}

