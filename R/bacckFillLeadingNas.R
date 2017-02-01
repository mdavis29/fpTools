
#'  Takes a vector of fiscal or calendar periods and returns a vector of dates, the first date of each month

#' @param  datain  a time series matrix with lags as each column, contained leading NAs
#' @param f frequency of the time series
#' @return a data frame with the leading NAs filled in
#' @author Zeng, Qichen 
#' @details this is too clean up lagged regressors for a xreg argument of an arima model 
#' @export


bacckFillLeadingNas <- function(datain, f = 12){
  mydata<-as.data.frame(datain[,1:(ncol(datain))])

  for (i in 1:(ncol(mydata))){
    x<-mydata[,i]
    leadingNaCount <- function(x) { sum(cumprod(is.na(x))) }
    nacounter <- leadingNaCount(x)
    x<-na.omit(x)
    revx <- ts(rev(x), frequency=12)
    
    if(nacounter > 0){
      fc <- forecast(stlm(revx, s.window=f, method = 'ar'), nacounter)
      f.val<-as.numeric(fc$mean)
      mydata[1:(nacounter),i]<-f.val
    }
  }
  return(mydata)
}
