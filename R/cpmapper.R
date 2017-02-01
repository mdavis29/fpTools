

#'  This function creates a vector of length n of calendar periods

#' @param  st The start of the calendar periods, an integer fiscal period of the form yyyymm
#' @param n The integer number of fiscal periods in the map
#' @param useFirst Logical where or not to include the 'st' variable a in the output vector
#' @return A vector of integet fiscal periods
#' @author Matthew Davis
#' @details  The output is of length n, reguardless of where the 'st' period is included
#' @export
#' @import lubridate
calendar.period.mapper<-function(st, n, useFirst=TRUE){  ##creates a fiscal period vector output

  styear<-as.numeric(substr(st,1,4))
  stmonth<-as.numeric(substr(st,5,6))
  y<- as.Date(paste(styear,stmonth, 01, sep="-"))
  if (useFirst==FALSE){y<-y+months(1:(n))}
  if (useFirst==TRUE){y<-y+months(0:(n-1))}
  y<-paste(strftime(y, format="%Y"), strftime(y, format="%m"), sep="")
  y<-as.numeric(y)
  return(y)
}

 