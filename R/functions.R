#' Download Federal Reserve Data
#'
#' \code{fred} downloads data from the Federal Reserve Economic Database
#' (FRED).
#'
#' @param ... vector of FRED codes as character strings (use quotations).
#' @param names vector of characters to replace column names in final dataframe. The length must be equal to the number of codes.
#' @param rm logical value expressing whether or not to remove time periods that contain an NA value. Defaults to TRUE.
#' @param log character vector describing which variables to transform using the natural logarithm.
#' @param as_xts logical value describing whether the output should be 'xts' type.
#'
#' @return dataframe
#' @import httr
#' @import jsonlite
#' @import broom
#' @export
#'
#' @examples
#' y <- fred('GDP')
#' macro <- fred('GDP', 'CPIAUCSL', 'UNRATE', names = c('y', 'pi', 'u'))
#'
fred <- function(..., names=NULL, rm=TRUE, log=NULL, as_xts=FALSE){
  series    <- list()
  info      <- list()
  id        <- toupper(unlist(list(...)))
  api_tail  <- '&api_key=04dc6777a721d81414a460093933e0ae&file_type=json'
  for (i in 1:length(id)){
    obs_request <- paste0('https://api.stlouisfed.org/fred/series/observations?series_id=',
                          id[i], api_tail)
    id_request  <- paste0('https://api.stlouisfed.org/fred/series?series_id=',
                          id[i], api_tail)
    cat_request <- paste0('https://api.stlouisfed.org/fred/series/categories?series_id=',
                          id[i], api_tail)
    GET(obs_request)  %>% content('text') %>% fromJSON -> obs_response
    GET(id_request)   %>% content('text') %>% fromJSON -> id_response
    GET(cat_request)  %>% content('text') %>% fromJSON -> cat_response
    series[[id[i]]] <-suppressWarnings(as.xts(as.numeric(obs_response$observations$value),
                             order.by = as.Date(obs_response$observations$date)))
    colnames(series[[id[i]]]) <- id[i]
    info[[id[i]]]         <- list()
    info[[id[i]]][['source']]     <- 'FRED'
    info[[id[i]]][['id']]         <- id_response$seriess$id
    info[[id[i]]][['realtime']]   <- as.Date(id_response$seriess$realtime_end)
    info[[id[i]]][['title']]      <- id_response$seriess$title
    #attr(z, 'frequency') <- id_response$seriess$frequency
    info[[id[i]]][['units']]      <- id_response$seriess$units
    info[[id[i]]][['s-adj']]      <- id_response$seriess$seasonal_adjustment
    info[[id[i]]][['updated']]    <- id_response$seriess$last_updated
    info[[id[i]]][['notes']]      <- id_response$seriess$notes
    info[[id[i]]][['cat-id']]     <- cat_response$categories$id
    info[[id[i]]][['parent-id']]  <- cat_response$categories$parent_id
    if (id[i] %in% toupper(log)){
      series[[id[i]]] <- log(series[[id[i]]])
      info[[id[i]]][['transform']] <- 'log'
    }
  }
  x <- series[[id[1]]]
  if (length(id)>1){
    for (j in 2:length(id)){
      x <- merge(x, series[[id[j]]])
    }
  }
  if (rm){x <- na.omit(x)}
  if (!as_xts){
    x_names <- c('Date', colnames(x))
    x <- spread(tidy(x), series, value)
    colnames(x)[1] <- 'Date'
    x <- as_tibble(select_(x, .dots=x_names))
  }
  for (k in 1:length(id)){
    attr(x[,k], 'info') <- info[[id[k]]]
  }
  if (!is.null(names)){colnames(x) <- c('Date', names)}
  x
}
#' Plot Economic Time Series
#'
#' \code{tplot} converts a time-based dataframe into a format more readily useable by \code{ggplot}, then plots the specified variables.
#'
#' @param x variable plotted across the x-axis (e.g. date, year, etc.).
#' @param ... list of y variables to be plotted .
#' @param data dataframe containing variables to be plotted.
#' @param nber logical value that determines whether vertical bars representing US recessions should be plotted.
#' @param xlim two values for the lower and upper bounds on x.
#' @param ylim two values for the lower and upper bounds on y.
#' @param xlab character label for the x-axis. Defaults to the name of the x variable.
#' @param ylab character label for the y-axis. Defaults to 'auto' which uses the name of the y variable if only one y variable is plotted; otherwise it uses 'Value'.
#' @param main character label for the title of the plot.
#' @param sub character label for the subtitle of the plot.
#' @param nber_color character describing the color of the nber bars.
#' @param size width of the lines to be plotted.
#' @param colors a character vector that describes the color of the lines to be used. Accepts name or hexadecimal value.
#' @param legend a character vector of length equal to number of y variables describing names of legend entries
#'
#' @return a 'gg' object
#' @import shiny
#' @import scales
#' @export
#'
#' @examples
#' data(macro)
#' tplot(Date, GDP, data=macro, nber=TRUE)
#'
tplot <- function(x, ..., data, nber=FALSE, xlim=c(NA,NA), ylim=c(NA,NA),
                  xlab=deparse(substitute(x)), ylab='auto',
                  main=NULL, sub=NULL, nber_color='pink',
                  size=1, colors=NULL, iplot=FALSE, legend='auto'){
  y <- eval(substitute(alist(...)))
  y_names <- paste(y)
  x_name <- deparse(substitute(x))
  min_x <- min(data[[x_name]])
  max_x <- max(data[[x_name]])
  X <- data[,c(x_name,y_names)]
  X <- suppressWarnings(gather(X, "Variable", "Value", eval(y_names)))
  colnames(X)[which(colnames(X)==x_name)] <- 'Date'
  recessions <- read.table(textConnection(
    "Peak, Trough
    1857-06-01, 1858-12-01
    1860-10-01, 1861-06-01
    1865-04-01, 1867-12-01
    1869-06-01, 1870-12-01
    1873-10-01, 1879-03-01
    1882-03-01, 1885-05-01
    1887-03-01, 1888-04-01
    1890-07-01, 1891-05-01
    1893-01-01, 1894-06-01
    1895-12-01, 1897-06-01
    1899-06-01, 1900-12-01
    1902-09-01, 1904-08-01
    1907-05-01, 1908-06-01
    1910-01-01, 1912-01-01
    1913-01-01, 1914-12-01
    1918-08-01, 1919-03-01
    1920-01-01, 1921-07-01
    1923-05-01, 1924-07-01
    1926-10-01, 1927-11-01
    1929-08-01, 1933-03-01
    1937-05-01, 1938-06-01
    1945-02-01, 1945-10-01
    1948-11-01, 1949-10-01
    1953-07-01, 1954-05-01
    1957-08-01, 1958-04-01
    1960-04-01, 1961-02-01
    1969-12-01, 1970-11-01
    1973-11-01, 1975-03-01
    1980-01-01, 1980-07-01
    1981-07-01, 1982-11-01
    1990-07-01, 1991-03-01
    2001-03-01, 2001-11-01
    2007-12-01, 2009-06-01"), sep=',',
    colClasses=c('Date', 'Date'), header=TRUE)
  if (nber){
    rec <- recessions[which(recessions[,1] >= min_x & recessions[,1] <= max_x),]
    nber <- geom_rect(data=rec,
                      aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=Inf),
                      fill=nber_color, alpha=0.5)
  } else {nber <- NULL}
  if (ylab == 'auto'){
    if (length(y) > 1){ylab <- 'Value'
    } else {ylab <- y_names[1]}
  }
  if (legend[1] == 'auto'){
    legend <- sort(y_names)
  } else {
    leg1 <- sort(y_names)
    leg2 <- character(length(y_names))
    for (i in 1:length(y_names)){
      leg2[which(leg1==y_names[i])] <- legend[i]
    }
    legend <- leg2
  }
  if (!is.null(colors)){
    col <- scale_color_manual(labels=legend, values=colors)
  } else {col <- scale_color_manual(labels=legend, values=hue_pal()(length(y_names)))}
  if (iplot){
    Z <- shinyApp(
      ui <- fluidPage(
        div(
          plotOutput('timeplot',
                     hover = hoverOpts("plot_hover", delay = 50, delayType = 'debounce')),
          uiOutput('hover_info'),
          plotOutput('selectplot',
                     height = '75px',
                     brush = brushOpts(id = 'plot_brush',
                                       resetOnNew = TRUE,
                                       direction = 'x'))
        )
      ),
      server <- function(input, output) {
        ranges <- reactiveValues(x = NULL, y = NULL)
        Z <- reactiveValues(X = NULL)
        output$selectplot <- renderPlot({
          ggplot(X, aes(x = Date, y = Value, color=Variable)) + geom_line() +
            scale_x_date(breaks=pretty_breaks(n=10)) +
            theme(legend.position = 'none') +
            theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

        })
        output$timeplot <- renderPlot({
          ggplot(Z$X)+nber+geom_line(aes(x=Date, y=Value, color=Variable), size=size)+coord_cartesian(xlim=ranges$x) +
            col+xlab(xlab)+ylab(ylab)+ggtitle(main, subtitle=sub)+theme(legend.position="top", legend.title=element_blank())
        })
        observe({
          brush <- input$plot_brush
          if (!is.null(brush)){
            ranges$x <- c(as.Date(brush$xmin, origin="1970-01-01"), as.Date(brush$xmax, origin="1970-01-01"))
            ranges$y <- c(brush$ymin, brush$ymax)
            Z$X <- filter(X, Date >= ranges$x[1] & Date <= ranges$x[2])
          } else {
            ranges$x <- NULL
            ranges$y <- NULL
            Z$X <- X
          }
        })
        output$hover_info <- renderUI({
          hover <- input$plot_hover
          point <- nearPoints(X, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
          if (nrow(point) == 0) return(NULL)

          left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
          top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

          left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
          top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

          style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                          "left:", left_px + 2, "px; top:", top_px + 2, "px;")
          wellPanel(
            style = style,
            p(HTML(paste0("<b> ",x_name, ": </b>", point$Date, "<br/>",
                          "<b> Value: </b>", round(point$Value, 2), "<br/>",
                          "<b> Series: </b>", point$Variable, "<br/>")))
          )
        })
      },
      options = list(height = 600)
    )
  } else {
    p <- ggplot(X) + nber+geom_line(aes(x=eval(parse(text=x_name)), y=Value, color=Variable), size=size)
    if (!is.na(ylim[1])){p <- p+ylim(ylim[1], ylim[2])}
    if (!is.na(xlim[1])){z <- p+xlim(xlim[1], xlim[2])
    } else {z <- p+xlim(min_x, max_x)}
    Z <- p+col+xlab(xlab)+ylab(ylab)+ggtitle(main, subtitle=sub)
    if (length(y)==1){Z <- Z+ theme(legend.position = 'none')}
  }
  Z
}
#' Obtain Information About Time Series
#'
#' \code{info} lists the information about a time series obtained via FRED.
#'
#' @param x FRED variable
#'
#' @return list
#' @export
#'
#' @examples
#' data(macro)
#' info(macro$GDP)
#'
info <- function(x){
  if (typeof(x) == 'list'){
    n <- length(x)
    names <- names(x)
    title <- NULL
    for (i in 1:n){
      if (!is.null(attributes(x[[i]])[['info']]$title)){
        title <- c(title, attributes(x[[i]])[['info']]$title)
      } else {title <- c(title, names[i])}
    }
    z <- cbind(names, title)
  } else {
    if (!is.null(attributes(x)[['info']])){
      z <- attributes(x)[['info']]
    } else {z <- "info not found"}
  }
  z
}
