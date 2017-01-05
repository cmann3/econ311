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
#' @importFrom zoo index
#' @import graphics
#' @import scales
#' @import xts
#' @export
#'
#' @examples
#' GDPgap <- fred('gap')
#' cycU <- fred('cyclical')
#' splot(GDPgap, cycU, reg=T)
#' splot(GDPgap, cycU, by='cycle')

splot <- function(x, y, by=NULL, data=NULL,
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
