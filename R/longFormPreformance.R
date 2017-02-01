#'  Takes a vector of fiscal or calendar periods and returns a vector of dates, the first date of each month
#' @param mydata data frame with actual and forecasted data in long form
#' @param nameKey column name that is the primary key for the forecast (ie bottom level)
#' @param valueCol column names that has the forecats and actuals vales
#' @param timeCol column with the time reference (intersection become the validation set)
#' @param forcastInicatorCol logical, TRUE ==  is a forecasted value, FALSE = is actyual
#' @return a dataframe of preformance metrics
#' @author Matthew Davis
#' @export
longFormPreformance<-function(mydata, 
                              nameKey = 'FinancialSubdivisionName',
                              valueCol = 'PaymentAmount' , 
                              timeCol = 'CalendarPeriod', 
                              forcastInicatorCol ='forecastIncator' ){
              outputMetric<-data.frame()
              
              nameList<-unique(mydata[,nameKey])
                for (k in 1:(length(nameList))){
                  mydata.temp<-droplevels(mydata[mydata[,nameKey] == nameList[k],])
                  mydata.temp.fcst<- mydata.temp[mydata.temp[,forcastInicatorCol]==TRUE,]
                  mydata.temp.actual<- mydata.temp[mydata.temp[,forcastInicatorCol]==FALSE,]
                  temp.timeOverlap<-intersect( mydata.temp.fcst[,timeCol],
                                              mydata.temp.actual[,timeCol])  
                    if(length( temp.timeOverlap)>2){
                            f<-mydata.temp.fcst[mydata.temp.fcst[, timeCol] %in% temp.timeOverlap,valueCol]
                            a<-mydata.temp.actual[mydata.temp.actual[, timeCol]  %in% temp.timeOverlap,valueCol]
                            a[is.na(a)]<-0
                            f[is.na(f)]<-0
                            temp.metrics<-modelPreformance(a =a , p= f)
                                                    }
                    if(length( temp.timeOverlap)<=2){temp.metrics<-modelPreformance(a =c(NA,NA) , p= c(NA,NA))}
                
                  outputMetric.temp<-cbind(data.frame(t(temp.metrics)), nm = nameList[k])
                  print(outputMetric.temp)
                  outputMetric<-rbind(outputMetric, outputMetric.temp)
                                              }
              colnames(outputMetric)[9] <-nameKey
              outputMetric$valueCol = as.factor(valueCol)
          return(outputMetric)
          }
