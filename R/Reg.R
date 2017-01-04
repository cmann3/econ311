#' Regression
#'
#' \code{Reg} performs a variety of regressions within a single function (only
#' OLS is supported at the moment). Standard test are automatically performed.
#' Multiple types of standard error calculations are available.
#'
#' @param form regression formula, e.g. \emph{y~x1+x2}
#' @param data object that contains data in formula
#' @param se type of standard error to use. For OLS regressions, defaults to
#'   'HC1' (robust option in STATA).
#' @param type regression type to use. Only 'ols' supported for now
#' @param na.action how are NAs handled. Defaults to na.exclude
#' @param table logical. Should results be printed
#
#' @return 'RegM' object containing regression information, including
#'   coefficients, se, tests, and others
#' @importFrom stats lm na.exclude na.omit pt terms
#' @import lmtest
#' @import sandwich
#' @export
#'
#' @examples
#' GDPgap <- fred('gap')
#' cycU <- fred('cyclical')
#' Reg(GDPgap ~ cycU) # Okun's Law
Reg <- function(form, data=NULL, se='auto', type='ols',
                na.action=na.exclude,table=TRUE){
  # Merge Data into a DF is not already
  if (is.null(data)){
    if(inherits(form,"formula")==TRUE){
      xterm = attr(terms(form), "variables")
      data = eval(xterm[[2]])
      for (i in 3:length(xterm)){
        data = merge(data, eval(xterm[[i]]))
      }
      colnames(data) = as.character(xterm)[2:length(xterm)]
    } else {
      data = form
      colnames(data) = all.vars(expression(form)) # get the inputs to form
    }
  }
  data = na.omit(data)
  # Perform Regression
  if (type=='ols'){
    z = Reg.ols(form, data=data, se=se, na.action=na.action,table=table)
  } else {stop('Type not recognized. Please choose "ols".')}
  #return(z)
}
Reg.ols <- function(form,data,se,table=table,digits=3, weights=NULL,
                    offset=NULL, na.action=na.exclude){
  z = lm(form, data=data, na.action=na.action)
  if (se == 'auto' | se == 'hc1' | se == 'r' | se == 'robust'){
    z$vcov = vcovHC(z, type = 'HC1')
  } else if (se == 'neweywest' | se == 'nw'){z$vcov = NeweyWest(z)
  } else if (se == 'sand'){z$vcov = sandwich(z)
  } else if (se == 'hc'){z$vcov = vcovHC(z, type = 'HC')
  } else if (se == 'hc0'){z$vcov = vcovHC(z, type = 'HC0')
  } else if (se == 'hc2'){z$vcov = vcovHC(z, type = 'HC2')
  } else if (se == 'hc3'){z$vcov = vcovHC(z, type = 'HC3')
  } else if (se == 'hc4'){z$vcov = vcovHC(z, type = 'HC4')
  } else if (se == 'hc4m'){z$vcov = vcovHC(z, type = 'HC4m')
  } else if (se == 'hc5'){z$vcov = vcovHC(z, type = 'HC5')
  } else if (se == 'hac'){z$vcov = vcovHAC(z)
  } else if (se == 'opg'){z$vcov = vcovOPG(z)
  } else if (se == 'andrews'){z$vcov = weightsAndrews(z)
  } else if (se == 'lumley'){z$vcov = weightsLumley(z)
  } else {z$vcov = vcovHC(z, type = 'const')}
  z$se = sqrt(diag(z$vcov))
  z$test = Reg.ols.test(z)
  class(z) = c("RegM", "lm")
  if (table==TRUE){
    print(z)
  }
  return(z)
}
Reg.ols.test <- function(x){
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
print.RegTest <- function(x){
  Table(x)
}
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
    colnames(z) <- c('Variable', 'Estimate', 'St.Err.', 'P-Vale', '')
    obs <- c('Observations', length(x$residuals), '', '', '')
    r2 <- c('R2', round(xSum$r.squared, digits=digits), '', '', '')
    adjr2 <- c('Adj. R2', round(xSum$adj.r.squared, digits=digits), '', '', '')
    z <- rbind(z, obs, r2, adjr2)
    cat(paste0(' Dependent Variable: ', xSum$terms[[2]], '\n'))
    Table(z, align=c('l','r','l','r','l'), rows=FALSE, sep = nVar)
  } else {
  }
}
