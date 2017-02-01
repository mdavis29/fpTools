
#'  calculated a bunch of model preformance metrics.
#' @param a vector of actuals
#' @param p vector of predictions, same length as actuals
#' @return a dataframe of preformance measurs
#' @author Matthew Davis
#' @details The following are the returned metrics
#' \itemize{
#' \item cor = pairwise complete,
#' \item  r2 = cor^2 ie percent of variance explained
#' \item rmse = root mean squared error
#' \item mse = mean squared error
#' \item pvalue =  from paired T Test
#' \item  meanA =  Actuals na.rm
#' \item meanP = Prediction na.rm
#' \item normalResids = p value from shapiro test, to test whether resids are normally distrubuted
#' }
#' @export
#'
modelPreformance<-function(a,p){
        cors <- NA
        r2 <- NA
        pval <- NA
        meanA<-NA
        meanP<-NA
        mape<-NA 
          
         if(all(c(length(a) == length(p),
                  length(a) > 1,
                  length(p)>1, 
                  sum(is.na(a))!=length(a),
                  sum(is.na(p))!=length(p)
                        ))){
           if(sd(a-p, na.rm = TRUE)>0){
                    cors = round(cor(a,p, use= "pairwise.complete.obs"),3)
                    r2 = round(cor(a,p, use= "pairwise.complete.obs")^2,3)
                    pval = round(t.test(a,p,  paired = TRUE)$p.value,3)
                        }}
      
        ma<-mean(a, na.rm = TRUE)
        mp<-mean(p, na.rm = TRUE)

        output<-c(  cors = cors, 
                    r2 =r2,
                    pval = pval,
                    rmse=trunc(rmse(a,p)),
                    mse=trunc(mse(a,p)),
                    meanA=round(ma,0),
                    meanP=round(mp,0))
            if(is.na(ma) == FALSE){
                if( ma!=0){mape<-round(100*(mp-ma)/ma,1)}}
       
     output<-c(output, mape = mape )
return(output)
}
