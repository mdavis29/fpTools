#' @title  FPTools a package for manipulationg fiscal periods
#' @description The package provides:
#'  \itemize{
#'    \item calendar.period.mapper: (creates a vector of calendar or fiscal periods)
#'    \item FP2CP: maps a fiscal period to calendar period and visversa
#'    \item fptoDate: takes a fiscal period and returns the first day of that month
#'    \item CPtoCalendar returns an aggrettge bizdays, holidays and mondays-sundays for each fiscal period
#'    \item xtsTainTestSplitter takes a quantmod xts object returns a list with train and test sets
#'    \item growthMetric takes a ts and returns year over year growth metrics
#'    \item modelPreformance generic calucations of model preformance
#'    \item plotForecastPreformance plot model preformance for time series data
#'    \item meanAdjustForecast mean adjusts a forecast based overlapping validation set
#'    \item longFormPreformance forecast preformance on long for data
#'    \item ts2lagMatrix create a time series matrix from a ts
#'    \item plotTsDenisities plot denisty of a times series before and after a date
#'   }
#' @examples
#'  FPtoCP(c(201201,20126)) #fiscal year starting July 1 st of each year
#'  calendar.period.mapper(201201, 24)
#'  fptoDate(201201)
#'  CPtoCalendar(c(201201,201212))
#'  CPtoCalendar(c(201201,201212),fiscal = FALSE)
#'  CPtoCalendar(c(201201,201212),fillgap = FALSE)
#' @docType package
#' @name fpTools
NULL

