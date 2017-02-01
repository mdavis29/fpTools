

#'  Takes a vector of fiscal or calendar periods and returns a vector of dates, the first date of each month

#' @param  mydata an integer vector of fiscal period of the form yyyymm
#' @param n number of months difference between the fiscal year and the calendar year
#' @param fiscal Logical, whether the mydata consisits of calendar or fiscal periods
#' @return a vector of dates, where the day is the first day of the month
#' @author Matthew Davis
#' @details A fiscal period 201701 starting in July 1st, 2016 would have a n=6 (
#' This is 'new fiscal year starts july 1st' scenario.  201701 should return as.Date('2016-01-07')
#' @export
#' @import lubridate
fptoDate<-function(mydata, n=6, fiscal=TRUE){ ##converts a fiscal period to the first day of the calender date's month

  mydata<-as.character(mydata)

  makeDate<-function(dat){
    dat<-strptime(paste(as.numeric(strtrim(dat,4)), as.numeric(substr(dat, 5, 6)), '01', sep="/"),  format="%Y/%m/%d" )
    return(dat)
  }
  out<-c(rep(NA, length(mydata)))
  for (i in 1:(length(mydata))){
    out[i]<-as.character(makeDate(mydata[i]))

  }
  out<-as.Date(out)
  if (fiscal==TRUE){ out<-out+months(n)}

  return(out)
}


