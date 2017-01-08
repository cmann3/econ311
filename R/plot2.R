#' Scatter Plot Time Series
#'
#' \code{sPlot} plots two economic time series against each other, taking into
#' consideration object attributes such as 'units' and 'title'. sPlot works
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
#' sPlot(GDPgap, cycU, reg=T)
#' sPlot(GDPgap, cycU, by='cycle')
sPlot <- function(x, y, by=NULL, data=NULL,
                  title = NULL, xlab = NULL, ylab = NULL,
                  time = NULL, ylim = NULL, xlim = NULL,
                  type = 'p', col = NULL, lwd = 1, lty = NULL, colAxes = NULL,
                  pch = NULL, cex = 1, colBG = NULL, bg = colBG,
                  colPlot = NULL, colGrid = NULL, theme = 'std',
                  reg = NULL, rlwd = lwd+1, mar = c(4, 4, 2, 1),
                  cex.lab = 1, cex.axis = 0.75, las = 0, leg = NULL){
  splot(x, y, by, data, title, xlab, ylab, time, ylim, xlim, type, col, lwd, lty,
        colAxes, pch, cex, colBG, bg, colPlot, colGrid, theme, reg, rlwd, mar,
        cex.lab, cex.axis, las, leg)
}
#' Plot Time Series
#'
#' \code{tPlot} plots economic time series data, taking into consideration
#' object attributes such as 'units' and 'title'. tPlot works best with data
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
#' @importFrom zoo index
#' @import graphics
#' @import scales
#' @import xts
#' @export
#'
#' @examples
#' GDPgap <- fred('gap')
#' cycU <- fred('cyclical')
#' GDP <- fred('GDPC1')
#' tPlot(GDPgap, cycU)
#' tPlot(GDPgap, right = GDP)
tPlot = function(..., right=NULL, data=NULL, time=NULL,
                 xlim = NULL, ylim = NULL, yRlim = NULL, ticks = c(8,5),
                 theme = 'std', col = NULL, colBG = NULL, colGrid = NULL,
                 colAxes = NULL, colPlot = NULL, colBar = NULL,
                 mar = c(3.5, 3.5, 2, 1), bars = 'cycle', las = 0,
                 cex = 1, cex.lab = 1, cex.axis = 0.75, lty = NULL, lwd = 1,
                 xlab = 'Date', ylab = NULL, yRlab = NULL, title = NULL,
                 ltyAxis = 'solid', lwdAxis = 1, lwdTicks = lwdAxis,
                 leg = 'auto'){
  tplot(..., right, data, time, xlim, ylim, yRlim, ticks, theme, col, colBG, colGrid,
        colAxes, colPlot, colBar, mar, bars, las, cex, cex.lab, cex.axis, lty, lwd, xlab,
        ylab, yRlab, title, ltyAxis, lwdAxis, lwdTicks, leg)
}


