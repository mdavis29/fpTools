
#' @title Calculate Forecast Model Preformance
#' @param fcastData univariate ts with the forecasted data (must overlap trainData for preformance metric calcs)
#' @param trainData univariate ts with the training data
#' @param seriesName name ofthe time series
#' @description plots a forecast and training data from time series objects 
#' @return a plot of preformance from the time series
#' @author Matthew Davis
#' @details This is intended to plot and look and anualized time series forecast and  estimate preformance
#' @export


plotForecastPreformance<-function(fcastData, trainData, seriesName = '') {
    print('click to plot')
    readline()
  val<-window(trainData, start = start(fcastData), end = end(trainData))
  pred<-window(fcastData, start = start(fcastData), end = end(trainData))
  combined<-ts(append(trainData, window(fcastData, start = end(trainData)+c(0,1))),
               start = start(trainData),  frequency = 12 )
  mp<-modelPreformance(val, pred)
  g<-growthMetric(combined, fiscal = FALSE)
  
    par(mfrow = c(2,1))
  plot(combined, las = 1, ylab = NULL,xlab = 'Calendar Year',  main =seriesName, mar = c(1,1,1,1) )
    lines(fcastData , col = 'red')
    lines(stl(combined,s.window=12)$time.series[,2],  col = 'lightblue', lty= 'dotdash' )
if(sd(fcastData)>0){
  bp<-barplot(g$YearTotal, names.arg = rownames(g), las = 1, mar = c(2,6,1,1))
      text(x = bp, y = g$YearTotal*.7, labels = paste(100*g$YoYGrowth,'%', sep = ''),cex=.7,pos=3)
      text(x = bp, y = g$YearTotal*.5, labels = round(g$YearTotal,0),cex=.7,pos=3)
        legend('topleft', legend =paste( colnames(mp) , mp[1,]), cex = .7)
              }
            }



