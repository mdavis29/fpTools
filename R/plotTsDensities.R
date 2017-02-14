#' @title plot time series density before and after
#' @param myTs a univariate time series object
#' @param timeCut numeric date date to cut test before and after ie 2016.5
#' @param plotName name of the plot to be put in the title
#' @return density plot of the data before and after the cut off
#' @author Matthew Davis
#' @details This creates a ts matrix of lag columns from a ts object 
#' @export
plotTsDenisities<-function(myTs, timeCut = c(2016), plotName =NULL ){
  before<-myTs[time(myTs)<timeCut]
  after<-myTs[time(myTs)>=timeCut]
  plot(density(before, na.rm = TRUE), 
     main = paste(plotName,
                  paste('Before and After', timeCut)), 
     col = 'blue',
     sub = plotName,
     cex.main = .8)

  abline(v = mean(before)+2*sd(before))
  abline(v = mean(before)-2*sd(before))
  abline(v = mean(before), col = 'blue')
  abline(v = mean(after), col = 'darkorange')
  tt<-t.test(before, after)
  legend('topright',fill = c('blue', 'darkorange','white'), cex = .8,
       legend = c(paste(paste('Mean Before', timeCut), round(mean(before),2), sep = ': '),
                  paste(paste('After/During', timeCut), round(mean(after),2), sep = ': '),
                  paste('Pvalue',round(tt$p.value,4))))
  d<-ifelse(tt$p.value > 0.05, 
          'not a significant differnence',
          'significantly different')
  cc<-ifelse(tt$p.value > 0.05, 
           'gray',
           'red')
  points(density(after, na.rm = TRUE), cex = .01, col = 'darkorange' )
  legend('bottom', cex = .8, legend =  d,text.col  = cc, bg = 'white')
}