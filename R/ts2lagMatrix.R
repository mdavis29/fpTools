#' @title Build Time Series Lag Matrix
#' @param myTs a univariate time series object
#' @param nlag number of lags
#' @return time series object where each column is a lag of the original data
#' @author Matthew Davis
#' @details This creates a ts matrix of lag columns from a ts object 
#' @export
ts2lagMatrix<-function(myTs, nlag = 1, pastLags = TRUE){
 output<-NULL
 l<-length(myTs) 
  if(pastLags == TRUE){
   for ( i in 1:nlag){
    temp<-c(rep(NA, i),head(myTs, l-i))
    output<-cbind(output, temp)
        }}
 if(pastLags ==FALSE){
  for ( i in 1:nlag){
    temp<-c(tail(myTs, l-i),  rep(NA, i))
    output<-cbind(output, temp)
         }}
 if(class(myTs) == 'ts'){
   output<-ts(output, start = start(myTs), frequency = frequency(myTs))
        }
    colnames(output)<-paste('lag', 1:nlag, sep = '' )
  return(output)
      }
