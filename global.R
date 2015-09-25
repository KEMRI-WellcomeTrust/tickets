library("plyr")
library("dplyr")
library(RODBC) #connect to the database sudo apt-get install libiodbc2-dev
library(scales)  
library(shinyBS)

#tickets <- read.csv("data/servicedesk data.csv")
#4169E1 -royal blue
#8A2BE2 - blueviolet
cbPalette <- c( "#0033ff" ,"#009900")

integer_breaks <- function(x){
  seq.int(0, ceiling(max(x)) ,by = 100) 
}

#connect to the database
dbase <-odbcConnect(dsn = "sql03" ,pwd = "test" , uid = "test")

ticket_data <- sqlFetch(dbase, 'open_ticket_vw')
tickets <- sqlQuery(dbase, "select * FROM open_ticket_vw",as.is = TRUE)
close(dbase)


#tickets <- read.csv("tickets_al.csv")
head(tickets)
table(tickets$NAME)

tickets$REPORT_DATE <- as.Date(tickets$REPORT_DATE, "%Y-%m-%d")
tickets$CLOSE_DATE <- as.Date(tickets$CLOSE_DATE, "%Y-%m-%d")
#tickets$REPORT_DATE <- as.Date(tickets$REPORT_DATE, "%d/%m/%Y")
#tickets$CLOSE_DATE <- as.Date(tickets$CLOSE_DATE, "%d/%m/%Y")

#generate month of report
tickets$month_report <- as.factor(format(tickets$REPORT_DATE, "%m")) 
tickets$month_report2 <- as.factor(format(tickets$REPORT_DATE, "%b")) 
tickets$month_reportX <- factor(tickets$month_report,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
tickets$yr_report <- as.factor(format(tickets$REPORT_DATE, "%Y")) 
table(tickets$month_report[tickets$NAME=="Data Governance"])
table(tickets$yr_report ,tickets$month_report, tickets$LOCATION_NAME)

#generate month of close
tickets$month_close <- as.factor(format(tickets$CLOSE_DATE, "%m")) 
tickets$month_close2 <- as.factor(format(tickets$CLOSE_DATE, "%b")) 
tickets$yr_close <- as.factor(format(tickets$CLOSE_DATE, "%Y")) 
table(tickets$yr_close  ,tickets$month_close, tickets$LOCATION_NAME)

