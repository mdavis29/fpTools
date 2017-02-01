
#'  Takes a vector of fiscal or calendar periods and returns a vector of dates, the first date of each month
#' @param  myTs a univariate time series object of frequeny = 12 (monthly)
#' @param fiscal Logical, whether to return fiscal or calendar metrics (FY start July 1st)
#' @return a dataframe of growth metrics
#' @author Matthew Davis
#' @details Year over Year 'yoy' is the percent change over the pervious year, 
#' @export
growthMetric<-function(myTs, fiscal = TRUE){
  ytime<-time(myTs)
  stYear<-min(round(time(myTs),0))
  endYear<-max(trunc(time(myTs)))
  ny<-endYear-stYear 
  yearTotal<-c()
  yName<-c()
  numMonths<-c()
  yoy<-c()
  yoy.ave<-c()
  yoy.aveChange<-c()	
  f<-ifelse(fiscal  == TRUE, .5,  0) 
  f1<-ifelse(fiscal == TRUE, 1,0)
  f2<-ifelse(fiscal  == TRUE, 'FY', 'CY') 
  for (i in 0:ny){
    keep<- ytime >= stYear+i+f & ytime < stYear+1+i+f
    nm<-sum(keep,na.rm = TRUE)
    if(nm<12){print(paste('Missing',12 - nm, 'Months in',f2, stYear+i+f1)) }
    yearTotal<-append(yearTotal, sum(c(myTs)[keep]))
    yoy<-append(yoy, round((yearTotal[i+1]-yearTotal[i])/yearTotal[i],3))
    yName<-append(yName, paste(f2, stYear+i+f1, sep = ''))
    numMonths<-append(numMonths, nm)
    yoy.ave<-append(yoy.ave, mean(c(myTs)[keep], na.rm = TRUE))
    yoy.aveChange<-append(yoy.aveChange,round((yoy.ave[i+1]-yoy.ave[i])/yoy.ave[i],3))
  }
  outputFY<-data.frame(YearTotal = yearTotal, 
                       YoYGrowth = c(NA, yoy), 
                       numMonths = numMonths, 
                       YoYAverage =  yoy.ave,
                       YoYAveChange = c(NA, yoy.aveChange)
                       
                       
  )
  rownames(outputFY)<-yName				
  outputFY[sapply(outputFY, is.nan)]<-NA
  outputFY[sapply(outputFY, is.infinite)]<-NA
  print(outputFY)
}

