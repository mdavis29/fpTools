
#' Converts a Fiscal Period to to a Caendar Period to a Fiscal Period

#' @param  fp an integer vector of fiscal period of the form yyyymm
#' @param n number of months difference between the fiscal year and the calendar year
#' @param reverse True or FALSE reverses the function to go from CP to FP
#' @return A vector of calendar periods  of the form yyyymm
#' @author Matthew Davis
#' @details A fiscal period of 201701 starting in July CY 2016 would have a n=6 (
#' This is 'new fiscal year starts july 1st' scenario
#' @export
#'
#'
#'
FPtoCP<-function(fp,n=6, reverse=FALSE){
f<-function(fp1, reverse1=FALSE, n1=6){  ##Converts a Fiscal Period to
  y<-as.numeric(substr(as.character(fp1),1,4))
  m<-as.numeric(substr(as.character(fp1),5,6))
  if( reverse1==TRUE){
    if(m > n1)y<-y+1
    m<-(m+n1)%%12}
  if(reverse1==FALSE){
    if(m <= n1) y<-y-1
    m<-(m-n1)%%12}
  if (m==0)m<-12
  if(m<10)m<-paste('0', m, sep='')
  as.numeric(paste(y, m, sep=''))
      }
sapply(fp,FUN=f, n1=n, reverse1=reverse)}

