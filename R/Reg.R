#' Regression
#'
#' \code{Reg} performs a variety of regressions within a single function (only
#' OLS is supported at the moment). Standard test are automatically performed.
#' Multiple types of standard error calculations are available.
#'
#' @param form regression formula, e.g. \emph{y~x1+x2}
#' @param data object that contains data in formula
#
#' @return 'RegM' object containing regression information, including
#'   coefficients, se, tests, and others
#' @importFrom stats lm na.exclude na.omit pt terms
#' @import lmtest
#' @import sandwich
#' @import magrittr
#' @export
#'
#' @examples
#' GDPgap <- fred('gap')
#' cycU <- fred('cyclical')
#' Reg(GDPgap, cycU) # Okun's Law
Reg <- function(y,..., data=NULL, time=NULL, test=TRUE){
  name.y <- deparse(substitute(y))
  name.x <- deparse(eval(substitute(alist(...))))


  gsub("[\\(\\)]", "", regmatches(name.x, gregexpr("\\(.*?\\)",name.x))[[1]]) %>%
    strsplit(",") %>% unlist %>% gsub(" ", "", .) -> name.x
  # Merge Data into a DF is not already
  if (is.null(data)){
    X <- list(...)
    for (i in 1:length(X)){
      y <- merge(y, X[[i]])
    }
    data <- y
    colnames(data) <- c(name.y, name.x)
  }
  data <- na.omit(data)
  if (!is.null(time)){data <- data[time]}
  data <- as.data.frame(data)
  # Create Formula
  form <- paste0(name.y,'~',paste(name.x,collapse='+'))
  # Perform Regression
  z <- lm(eval(parse(text=form)), data=data, na.action=na.exclude)
  z$vcov <- vcovHC(z, type = 'HC1')
  z$se <- sqrt(diag(z$vcov))
  if (test){z$test <- Reg.test(z)}
  class(z) <- c("RegM", "lm")
  z
}
#' @export
print.RegM <- function(x, digits=3){
  if (is.null(x$first)){
    xSum <- summary(x)
    namV <- names(x$coeff)
    nVar <- length(namV)
    COEFF <- x$coeff
    SE <- x$se
    tStat <- COEFF / SE
    pVal <- 2*pt(-abs(tStat), df=x$df.residual)
    star <- vector('character', length = nVar)
    for (i in 1:nVar){
      if (pVal[i] <= 0.01){star[i] <- '***'
      } else if (pVal[i] <= 0.05){star[i] <- '**'
      } else if (pVal[i] <= 0.1){star[i] <- '*'
      } else {star[i] <- ''}
    }
    COEFF <- as.character(round(COEFF, digits=digits))
    SE <- as.character(round(SE, digits=digits))
    pVal <- as.character(round(pVal, digits=digits))
    for (i in 1:nVar){
      if (COEFF[i] == '0'){COEFF[i] <- '<0.001'}
      if (SE[i] == '0'){SE[i] <- '<0.01'}
      if (pVal[i] == '0'){pVal[i] <- '<0.01'}
      SE[i] <- paste0('(',SE[i],')')
    }
    z <- cbind(namV,COEFF,SE,pVal,star)
    colnames(z) <- c('Variable', 'Estimate', 'St.Err.', 'P-Value', '')
    obs <- c('Observations', length(x$residuals), '', '', '')
    r2 <- c('R2', round(xSum$r.squared, digits=digits), '', '', '')
    adjr2 <- c('Adj. R2', round(xSum$adj.r.squared, digits=digits), '', '', '')
    z <- rbind(z, obs, r2, adjr2)
    cat(paste0(' Dependent Variable: ', xSum$terms[[2]], '\n'))
    Table(z, align=c('l','r','l','r','l'), rows=FALSE, sep = nVar)
  } else {
  }
}
#' @export
print.RegTest <- function(x){
  Table(x)
}
Reg.test <- function(x){
  z = matrix(,7,5)
  colnames(z) = c("Test Name", "H0", "Statistic", "P-Value", " ")
  # Breusch-Godfrey Test
  test = bgtest(x)
  row = 1
  z[row,1] = "Breusch-Godfrey Test"
  z[row,2] = "No Serial Corr."
  z[row,3] = round(as.numeric(test[[1]]),3)
  if (round(as.numeric(test[[4]]),3) == 0){
    z[row,4] = "<0.01"
    z[row,5] = "***"
  } else {
    z[row,4] = round(as.numeric(test[[4]]),3)
    if (as.numeric(test[[4]]) < 0.01){z[row,5] = "***"
    } else if (as.numeric(test[[4]]) < 0.05) {z[row,5] = "**"
    } else if (as.numeric(test[[4]]) < 0.1) {z[row,5] = "*"
    } else {z[row,5] = " "}
  }
  # Durbin-Watson Test
  test = dwtest(x)
  row = 2
  z[row,1] = "Durbin-Watson Test"
  z[row,2] = "No Autocorr."
  z[row,3] = round(as.numeric(test[[1]]),3)
  if (round(as.numeric(test[[4]]),3) == 0){
    z[row,4] = "<0.01"
    z[row,5] = "***"
  } else {
    z[row,4] = round(as.numeric(test[[4]]),3)
    if (as.numeric(test[[4]]) < 0.01){z[row,5] = "***"
    } else if (as.numeric(test[[4]]) < 0.05) {z[row,5] = "**"
    } else if (as.numeric(test[[4]]) < 0.1) {z[row,5] = "*"
    } else {z[row,5] = " "}
  }
  # Breusch-Pagan Test
  test = bptest(x)
  row = 3
  z[row,1] = "Breusch-Pagan Test"
  z[row,2] = "Homoskedastic"
  z[row,3] = round(as.numeric(test[[1]]),3)
  if (round(as.numeric(test[[4]]),3) == 0){
    z[row,4] = "<0.01"
    z[row,5] = "***"
  } else {
    z[row,4] = round(as.numeric(test[[4]]),3)
    if (as.numeric(test[[4]]) < 0.01){z[row,5] = "***"
    } else if (as.numeric(test[[4]]) < 0.05) {z[row,5] = "**"
    } else if (as.numeric(test[[4]]) < 0.1) {z[row,5] = "*"
    } else {z[row,5] = " "}
  }
  # Goldfeld-Quandt Test
  test = gqtest(x)
  row = 4
  z[row,1] = "Goldfeld-Quandt Test"
  z[row,2] = "Homoskedastic"
  z[row,3] = round(as.numeric(test[[1]]),3)
  if (round(as.numeric(test[[4]]),3) == 0){
    z[row,4] = "<0.01"
    z[row,5] = "***"
  } else {
    z[row,4] = round(as.numeric(test[[4]]),3)
    if (as.numeric(test[[4]]) < 0.01){z[row,5] = "***"
    } else if (as.numeric(test[[4]]) < 0.05) {z[row,5] = "**"
    } else if (as.numeric(test[[4]]) < 0.1) {z[row,5] = "*"
    } else {z[row,5] = " "}
  }
  # Harvey-Collier Test
  test = harvtest(x)
  row = 5
  z[row,1] = "Harvey-Collier Test"
  z[row,2] = "Linear"
  z[row,3] = round(as.numeric(test[[1]]),3)
  if (round(as.numeric(test[[4]]),3) == 0){
    z[row,4] = "<0.01"
    z[row,5] = "***"
  } else {
    z[row,4] = round(as.numeric(test[[4]]),3)
    if (as.numeric(test[[4]]) < 0.01){z[row,5] = "***"
    } else if (as.numeric(test[[4]]) < 0.05) {z[row,5] = "**"
    } else if (as.numeric(test[[4]]) < 0.1) {z[row,5] = "*"
    } else {z[row,5] = " "}
  }
  # Rainbow Test
  test = raintest(x)
  row = 6
  z[row,1] = "Rainbow Test"
  z[row,2] = "Linear"
  z[row,3] = round(as.numeric(test[[1]]),3)
  if (round(as.numeric(test[[4]]),3) == 0){
    z[row,4] = "<0.01"
    z[row,5] = "***"
  } else {
    z[row,4] = round(as.numeric(test[[4]]),3)
    if (as.numeric(test[[4]]) < 0.01){z[row,5] = "***"
    } else if (as.numeric(test[[4]]) < 0.05) {z[row,5] = "**"
    } else if (as.numeric(test[[4]]) < 0.1) {z[row,5] = "*"
    } else {z[row,5] = " "}
  }
  # RESET Test
  test = resettest(x)
  row = 7
  z[row,1] = "Ramsey's RESET Test"
  z[row,2] = "Fun. Form"
  z[row,3] = round(as.numeric(test[[1]]),3)
  if (round(as.numeric(test[[4]]),3) == 0){
    z[row,4] = "<0.01"
    z[row,5] = "***"
  } else {
    z[row,4] = round(as.numeric(test[[4]]),3)
    if (as.numeric(test[[4]]) < 0.01){z[row,5] = "***"
    } else if (as.numeric(test[[4]]) < 0.05) {z[row,5] = "**"
    } else if (as.numeric(test[[4]]) < 0.1) {z[row,5] = "*"
    } else {z[row,5] = " "}
  }
  class(z) = c("RegTest", "matrix")
  return(z)
}
