trunc_time_ <- function(x, freq){
  if(!is.Date(x)){x <- as.Date(x)}
  if (freq == 'y'){z <- year(x)
  } else if (freq == 'm'){
    q_y <- year(x)
    q_m <- month(x)
    if (q_m < 10){q_m <- paste0('0',q_m)}
    z <- paste(q_y, q_m, sep="-")
  } else if (freq == 'q'){
    q_y <- year(x)
    q_m <- month(x)
    if (q_m == 1){q_q <- 1
    } else if (q_m == 4){q_q <- 2
    } else if (q_m == 7){q_q <- 3
    } else {q_q <- 4}
    z <- paste0(q_y, "-Q", q_q)
  } else {z <- as.character(x)}
  z
}
trunc_freq_ <- function(x){
  x <- tolower(x)
  years <- c('annual', 'yearly', 'year', 'years', 'y', 'a')
  quarters <- c('quarterly', 'quarter', 'quarters', 'q')
  months <- c('monthly', 'month', 'months', 'm')
  weeks <- c('weekly', 'week', 'weeks', 'w')

  if (x %in% years){z = 'y'
  } else if (x %in% quarters){z = 'q'
  } else if (x %in% months){z = 'm'
  } else if (x %in% weeks){z = 'w'
  } else {z = 'd'}
  z
}
fred_ <- function(x, type = 'series'){
  api_tail <- '&api_key=04dc6777a721d81414a460093933e0ae&file_type=json'
  if (type == 'series'){
    obs_request <- paste0('https://api.stlouisfed.org/fred/series/observations?series_id=',
                          x, api_tail)
    id_request <- paste0('https://api.stlouisfed.org/fred/series?series_id=',
                         x, api_tail)
    cat_request <- paste0('https://api.stlouisfed.org/fred/series/categories?series_id=',
                          x, api_tail)
    GET(obs_request) %>% content('text') %>% fromJSON -> obs_response
    GET(id_request) %>% content('text') %>% fromJSON -> id_response
    GET(cat_request) %>% content('text') %>% fromJSON -> cat_response
    z <-as.xts(as.numeric(obs_response$observations$value),
               order.by = as.Date(obs_response$observations$date))
    colnames(z) <- x
    attr(z, 'source') <- 'FRED'
    attr(z, 'id') <- id_response$seriess$id
    attr(z, 'realtime') <- as.Date(id_response$seriess$realtime_end)
    attr(z, 'title') <- id_response$seriess$title
    attr(z, 'frequency') <- id_response$seriess$frequency
    attr(z, 'units') <- id_response$seriess$units
    attr(z, 's-adj') <- id_response$seriess$seasonal_adjustment
    attr(z, 'updated') <- id_response$seriess$last_updated
    attr(z, 'notes') <- id_response$seriess$notes
    attr(z, 'cat-id') <- cat_response$categories$id
    attr(z, 'parent-id') <- cat_response$categories$parent_id
  } else if (type == 'search'){
    x <- gsub(" ", "+",x)
    search_request <- paste0('https://api.stlouisfed.org/fred/series/search?search_text=',
                             x, api_tail)
    GET(search_request) %>% content('text') %>% fromJSON -> search_response
    z <- search_response$seriess
    z <- z[,c('id', 'title', 'observation_start', 'observation_end', 'frequency_short',
              'units_short', 'seasonal_adjustment_short')]
  } else if (type == 'cat'){
    cat_request <- paste0('https://api.stlouisfed.org/fred/category/children?category_id=',
                          x, api_tail)
    GET(cat_request) %>% content('text') %>% fromJSON -> cat_response
    z <- cat_response$categories
  } else if (type == 'catseries'){
    series_request <- paste0('https://api.stlouisfed.org/fred/category/series?category_id=',
                             x, api_tail)
    GET(series_request) %>% content('text') %>% fromJSON -> series_response
    z <- series_response$seriess
    z <- z[,c('id', 'title', 'observation_start', 'observation_end', 'frequency_short',
              'units_short', 'seasonal_adjustment_short')]
  }
  z
}
#' Download Federal Reserve Data
#'
#' \code{fred} downloads data from the Federal Reserve Economic Database
#' (FRED) and formats it to an 'xts' object.
#'
#' @param ... vector of FRED codes as strings. If string begins with '?',
#'   \code{fredSearch} is called to search for the series. The following
#'   codes are also accepted: 'gap', 'cyclical', 'inflation', 'Y', 'I', 'G',
#'   and 'NX'.
#' @param trans vector of characters used to transform the corresponding
#'   series. Calls the \code{trans} function. Accepts 'log', 'l', 'g', 'gy',
#'   'd', 'dy', 'dv', 'pdv', 'ldv', 'i', 'r', and 's'.
#' @param opt additional options passed to \code{trans}
#' @param form if specified, formula used to combine the FRED series.
#'   Similar to the 'formula' field on the FRED website. Each series in ...
#'   is assigned a letter (a)-(z) sequentially to be used in the formula.
#' @param freq time frequency to convert with which to convert the series.
#'   Accepts 'y', 'q', 'm', 'w', or 'd'.
#' @param date starting and ending period for the series. Uses 'xts'
#'   conventions, e.g. \emph{'1970-01/1972-10'}
#' @param name vector specifying the resulting column names. If \code{NULL},
#'   the FRED code is used.
#' @param units units of the resulting object. Appended to the attributes.
#'   If \code{NULL}, the units from the FRED website are used.
#'
#'
#' @return 'xts' object
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @import magrittr
#' @import xts
#' @export
#'
#' @examples
#' Y <- fred('GDPC1', trans = 'gy') # yearly growth rate of Real GDP
#' gap <- fred('GDPC1', 'GDPPOT', form = (a-b)/b*100, units = 'Percent')
#' urate <- fred('?unemploy rate')  # evokes search
#'
fred <- function(..., trans = NULL, form = NULL, freq = NULL,
                 date = NULL, opt = NULL, name = NULL, units = NULL){
  id <- as.vector(...)
  special <- c('gap','cyclical','inflation')
  nicknames <- c('Y','C','I','G','NX')
  for (i in 1:length(id)){
    if (id[i] %in% special){
      if (id[i] == 'gap'){
        y <- fred_('GDPC1', type = 'series')
        ypot <- fred_('GDPPOT', type = 'series')
        z <- (y - ypot)/ypot*100
        attr(z, 'units') <- 'Percent'
        attr(z, 'title') <- 'GDP Gap'
        colnames(z) <- 'GDP_Gap'
      } else if (id[i] == 'cyclical'){
        u <- fred_('UNRATE', type = 'series')
        un <- fred_('NROU', type = 'series')
        z <- u - un
        attr(z, 'units') <- 'Percent'
        attr(z, 'title') <- 'Cyclical Unemployment Rate'
        colnames(z) <- 'Cyc_U'
      } else if (id[i] == 'inflation'){
        p <- fred_('CPIAUCSL', type = 'series')
        z <- G(p, 'y')
        attr(z, 'units') <- 'Percent'
        attr(z, 'title') <- 'Inflation Rate (CPI)'
        colnames(z) <- 'Pi'
      }
    } else {
      if (id[i] %in% nicknames){
        if (id[i] == 'Y'){id[i] <- 'GDPC1'
        } else if (id[i] == 'C'){id[i] <- 'PCECC96'
        } else if (id[i] == 'I'){id[i] <- 'GPDIC1'
        } else if (id[i] == 'G'){id[i] <- 'GCEC1'
        } else if (id[i] == 'NX'){id[i] <- 'NETEXC'}
      } else if (substr(id[i], 1, 1) == '?'){
        searchTerms <- substr(id[i], 2, nchar(id[i]))
        id[i] <- fredSearch(searchTerms, ret='id')
      }
      id[i] <- toupper(id[i])
      z <- fred_(id[i], type = 'series')
      if (!is.null(trans)){
        if (is.null(opt)){z <- trans(z, type = trans)
        } else {z <- trans(z, type = trans, opt = opt)}
      }
    }
    if (exists('frd_dt')){frd_dt <- merge(frd_dt, z)
    } else {frd_dt <- z}
  }
  if (!is.null(form)){
    n <- ncol(frd_dt)
    letname <- letters[seq(from=1,to=n)]
    for (i in 1:n){assign(paste0(letname[i]),frd_dt[,i])}
    frd_dt <- eval(parse(text=form))
  }
  if (!is.null(freq)){frd_dt <- freq(frd_dt,freq)}
  if (!is.null(date)){frd_dt <- frd_dt[date]}
  if (!is.null(units)){attr(frd_dt, 'units') <- units}
  frd_dt <- na.omit(frd_dt)
  if (!is.null(name)){colnames(frd_dt) <- name}
  frd_dt
}
#' Search Federal Reserve Data
#'
#' \code{fredSearch} searches and downloads data from the Federal Reserve
#' Economic Database.
#'
#' @param x character value to search
#' @param ret either 'obs' or 'id' to return either the data observations
#'   or the FRED id code
#' @param n the maximum number of results to show per page
#
#' @return 'xts' object or a FRED code
#' @importFrom lubridate is.Date
#' @export
#'
#' @examples
#' urate <- fredSearch('unemp rate')
#' medPrice <- fredSearch('medical care price')
fredSearch <- function(x, ret='obs', n=30){
  x <- fred_(x, 'search')
  max_page <- floor(1000/n)
  searching = 1
  while (searching > 0){
    page_num <- (searching - 1)/ n + 1
    x.temp <- x[searching:(searching+n),]
    z <- matrix(, n, 5)
    colnames(z) = c('Title', 'Dates', 'Fq', 'Unit', 'SA')
    id <- x.temp[,'id']
    for (i in 1:n){
      z[i,1] <- x.temp$title[i]
      z[i,4] <- unlist(strsplit(x.temp$units_short[i],' '))[1]
      if (z[i,4] == 'Percent' || z[i,4] == 'Percentage'){z[i,4] <- '%'}
      z[i,5] <- x.temp$seasonal_adjustment_short[i]
      z[i,3] <- trunc_freq_(x.temp$frequency_short[i])
      start <- trunc_time_(x.temp$observation_start[i], z[i,3])
      end <- trunc_time_(x.temp$observation_end[i], z[i,3])
      z[i,2] <- paste0(start,':',end)
    }
    cat(paste0('Search Results (page ',page_num,' of ',max_page,'):\n'))
    Table(z, trunc = 1, align='l', n=n)
    display_choice <- TRUE
    while (display_choice){
      if (searching < 31){zz <- readline(paste0("Choice [1-",n, " p1-p",max_page,", >]:"))
      } else if (searching > (max_page-1)*n){zz <- readline(paste0("Choice [1-",n, " p1-p",max_page,", <]:"))
      } else {zz <- readline(paste0("Choice [1-",n, " p1-p",max_page,", <, >]:"))}

      if (zz == '>'){
        if (searching > (max_page-1)*n){cat('No next results to display. Please choose again.')
        } else {
          searching <- searching + n
          display_choice <- FALSE
        }
      } else if (zz == '<'){
        if (searching < n+1){cat('No previous results to display. Please choose again.')
        } else {
          searching <- searching - n
          display_choice <- FALSE
        }
      } else if (zz == ""){
      } else if (substr(zz, 1, 1) == 'p'){
        pgnum <- as.numeric(substr(zz, 2, nchar(zz)))
        if (pgnum > max_page || pgnum < 1){cat('Page number not found. Please choose again.')
        } else {
          searching <- (pgnum - 1)*n + 1
          display_choice <- FALSE
        }
      } else {
        #zz <- as.numeric(zz) + searching - 1
        zz <- id[as.numeric(zz)]
        if (ret == 'obs'){zz <- fred_(zz, 'series')}
        display_choice <- FALSE
        searching <- 0
      }
    }
  }
  zz
}
