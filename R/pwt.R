#' Penn World Tables 9.0
#'
#' A dataset containing data from the Penn World Tables version 9.0. It
#' includes data from 178 countries. This is not the complete PWT. Observations
#' are only included if they contain data on Real GDP, capital stock, and
#' employment. Furthermore, only 16 variables are included. More info about the
#' variables are included in the attributes.
#'
#' @format A data frame with 8244 rows and 16 variables:
#' \describe{
#'   \item{code}{3-letter ISO country code}
#'   \item{year}{Year}
#'   \item{pop}{Population}
#'   \item{emp}{Labor Force}
#'   \item{hours}{Average Annual Hours of Labor Force}
#'   \item{hc}{Human Capital Index}
#'   \item{gdp}{Real GDP at Constant 2011 National Prices}
#'   \item{k}{Capital Stock at Constant 2011 National Prices}
#'   \item{tfp}{TFP at Constant National Prices (2011=1)}
#'   \item{labsh}{Share of Labour Compensation in GDP at Current Nat. Prices}
#'   \item{delta}{Average Depreciation Rate of the Capital Stock}
#'   \item{shC}{Share of Household Consumption at current PPPs}
#'   \item{shI}{Share of Gross Capital Formation at current PPPs}
#'   \item{shG}{Share of Government Consumption at current PPPs}
#'   \item{shX}{Share of Merchandise Exports at current PPPs}
#'   \item{shM}{Share of Merchandise Imports at current PPPs}
#' }
#' @source \url{http://www.rug.nl/ggdc/productivity/pwt/}
"pwt9"
#' Penn World Tables 9.0 Countries
#'
#' A dataset containing the countries in the PennWorld Table.
#'
#' @format A data frame with 178 rows and 3 variables:
#' \describe{
#'   \item{Code}{3-letter ISO country code}
#'   \item{Country}{Country name}
#'   \item{Years}{Years available in the dataset}
#' }
#' @source \url{http://www.rug.nl/ggdc/productivity/pwt/}
"pwtcountries"
#' Penn World Tables 9.0 Variables
#'
#' A dataset containing the variables in the PennWorld Table.
#'
#' @format A data frame with 16 rows and 3 variables:
#' \describe{
#'   \item{Series}{Variable code for use in \code{pwt}}
#'   \item{Title}{Variable name}
#'   \item{Units}{Units of the variable}
#' }
#' @source \url{http://www.rug.nl/ggdc/productivity/pwt/}
"pwtseries"
#' Extract PWT 9.0 Data
#'
#' \code{pwt} extracts and formats specific data from the Penn World Tables as
#' 'xts' time series objects.
#'
#' @param country 3-digit ISO country code
#' @param series variable name to load
#
#' @return 'xts' object
#' @importFrom utils data
#' @import xts
#' @export
#'
#' @examples
#' Y <- pwt(country = 'USA', series = 'gdp')
#' tPlot(Y)
pwt <- function(country, series){
  data(pwt9, envir = environment())
  data(pwtcountries, envir = environment())
  cName <- toupper(country)
  sName  <- tolower(series)
  info <- subset(pwtcountries, Code == cName)
  dat  <- pwt9[, c('code', 'year', sName)]
  dat  <- subset(dat, code == cName)
  year <- as.Date(paste0(dat[,'year'],'-01-01'))
  x    <- as.xts(dat[,sName], order.by = year)
  colnames(x) <- sName
  attr(x, 'id') <- sName
  attr(x, 'units') <- attr(pwt9[, sName], 'units')
  attr(x, 'title') <- attr(pwt9[, sName], 'title')
  attr(x, 'frequency') <- 'Yearly'
  attr(x, 'source') <- 'Penn World Tables 9.0'
  attr(x, 'country') <- as.character(info[1,2])
  x
}
#' PWT 9.0 Series Names
#'
#' \code{pwtSeries} lists available series for \code{pwt} function and info
#
#' @return data.frame
#' @importFrom utils data
#' @export
#'
#' @examples
#' pwtSeries()
pwtSeries <- function(){
  data(pwtseries, envir = environment())
  Table(pwtseries, n=180, align=c('l','l','l'))
}
#' PWT 9.0 Country Names
#'
#' \code{pwtCountry} lists available country codes for \code{pwt} function and
#' the years available
#'
#' @param search character string to search for within country names. If NULL
#'   lists all countries
#
#' @return data.frame
#' @importFrom utils data
#' @export
#'
#' @examples
#' pwtCountry()
#' pwtCountry('swed')
#' pwtCountry('ir')
pwtCountry <- function(search = NULL){
  data(pwtcountries, envir = environment())
  if (!is.null(search)){
    resRows <- which(grepl(search, pwtcountries, ignore.case = TRUE) |
                       grepl(search, pwtcountries$Code, ignore.case = TRUE))
    results <- pwtcountries[resRows,]
    Table(results, n = 180, align=c('c','l','c'))
  } else {
    Table(pwtcountries, n=180, align=c('c','l','c'))
  }
}

