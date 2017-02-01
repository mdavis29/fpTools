
#'  Takes a vector of fiscal or calendar periods and returns a vector of dates, the first date of each month
#' @param  rd.ts Time Series Matrix with raw data
#' @param fd.ts Time Series Matrix of 
#' @return a dataframe of growth metrics
#' @author Matthew Davis
#' @details Year over Year 'yoy' is the percent change over the pervious year, 
#' @export

meanAdjustForecast<-function(rd.ts, fd.ts){
  nms<-intersect(colnames(rd.ts), colnames(fd.ts))
  for (i in 1:(length(nms))){
    p<-1
    nm.temp<-nms[i]
    rd.temp<-rd.ts[, nm.temp]
    fd.temp<-fd.ts[, nm.temp]
    s = start(fd.temp)
    e = end(rd.temp)
    vs<-window(rd.temp, start =s, end =e)
    fs<-window(fd.temp, start = s, end =e)
    
    test1<-all(length(vs)>1,length(fs)>1)  
    
    if(test1==TRUE){
      test2<-all(sum(vs == 0)<3, 
                 sum(fs == 0)<3,
                 sd(vs)!=0,
                 sd(fs)!= 0)
      if(test2 == TRUE){      
        p<-t.test(vs, fs)$p.value  
        print(paste(nm.temp,p))
        if(p<.05){  
          fd.ts[, nms[i]]<-fd.temp + (mean(vs) - mean(fs))/2
          print(paste(nm.temp, 'adjusted'))
        }}}}
  return(fd.ts)}
