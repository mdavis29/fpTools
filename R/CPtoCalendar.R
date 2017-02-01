#'  Takes a vector of fiscal or calendar periods and returns a vector of dates, the first date of each month
#' @param mydata an integer vector of calendar periods of the form yyyymm (defaults to fiscal)
#' @param fillgap whether to fill in any missing fiscal periods, or only use the list given
#' @param calendarname Name for timdate package to deterimine holidays na buisness days
#' @param fiscal whether the calendar periods are a fiscal or calendar period
#' @return a data frame of sums of weekdays, bizdays and holidays by the fiscal periods entered
#' @author Qichen Zeng, Matthew Davis
#' @details This package is intended to get montly summaries of number holidays and weekdays for a particular calenar or fiscal month. 
#' @export

CPtoCalendar <- function(mydata, fillgap = TRUE, fiscal = TRUE , calendarname = holidayNYSE(2000:2020)){
  ifelse(fiscal == TRUE, cp<-fpTools::FPtoCP(mydata), cp <-mydata)

  ##Find the max and min calendar time in the given Fiscal Period
  maxtime <- strptime(paste(max(cp),01, sep=""), format = "%Y%m%d")+months(1)-days(1)
  mintime <- strptime(paste(min(cp),01, sep=""), format = "%Y%m%d")
  
  ##Make Calendar using timeSequence from min to max period
  calendertime <- timeSequence(from = mintime, to = maxtime, by='day',format = "%Y-%m-%d")
  calendertimeDate <- format(calendertime, format = "%Y-%m-%d")
  weekdays <- dayOfWeek(calendertime)
  
  ##Set the week days to true or false to individual weekdays
  Monday <- ifelse(weekdays == 'Mon', 1, 0)
  Tuesday <- ifelse(weekdays == 'Tue', 1, 0)
  Wednesday <- ifelse(weekdays == 'Wed', 1, 0)
  Thursday <- ifelse(weekdays == 'Thu', 1, 0)
  Friday <- ifelse(weekdays == 'Fri', 1, 0)
  Saturday <- ifelse(weekdays == 'Sat', 1, 0)
  Sunday <- ifelse(weekdays == 'Sun', 1, 0)
  
  ##Find the working and off days
  Businessdays <- isBizday(calendertime, calendarname)
  Business_days <- ifelse(Businessdays == 'TRUE', 1, 0)
  Holiday <- isHoliday(calendertime, calendarname)
  Holidays <- ifelse(Holiday == 'TRUE', 1, 0)
  years_dates <- substr(as.character(calendertimeDate),1,4)
  month_dates <- substr(as.character(calendertimeDate),6,7)
  CalendarPeriod <- paste(years_dates,month_dates, sep = "")
  
  ##Convert CalendarPeriod back to FiscalPeriod
  FiscalPeriod <- FPtoCP(CalendarPeriod, reverse = TRUE)
  testcalender <- data.frame(FiscalPeriod,calendertimeDate, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, Business_days, Holidays)
  
  ##Print out the summary for work days and off days
  summarycalendar <- plyr::ddply(testcalender, c('FiscalPeriod'), plyr::summarize,
                           Monday = sum(Monday),
                           Tuesday = sum(Tuesday),
                           Wednesday = sum(Wednesday),
                           Thursday = sum(Thursday),
                           Friday = sum(Friday),
                           Saturday = sum(Saturday),
                           Sunday = sum(Sunday),
                           Business_day = sum(Business_days),
                           Holidaysandweekends = sum(Holidays))
  
  
  ##Subset the FiscalPeriod as index
  rownames(summarycalendar) <- summarycalendar[,1]
  summarycalendar[,1] <- NULL
  
  ##Control for output as a whole list or individual FP inputs
  if (fiscal==FALSE){rownames(summarycalendar)<- FPtoCP(rownames(summarycalendar))}
  
  if (fillgap==FALSE){ summarycalendar <-summarycalendar[(rownames(summarycalendar)) %in% as.character(mydata),]}
  
  
  return(summarycalendar)} #end function
