library(ggplot2)
library("plyr")
library("dplyr")
library(RODBC) #connect to the database sudo apt-get install libiodbc2-dev
library(scales)
#tickets <- read.csv("data/servicedesk data.csv")
#4169E1 -royal blue
 	#8A2BE2 - blueviolet
cbPalette <- c( "#0033ff" ,"#009900")

integer_breaks <- function(x){
  seq.int(0, ceiling(max(x)) ,by = 100) 
}

#connect to the database
#dbase <-odbcConnect("helpdesk")
#ticket_data <- sqlFetch(dbase, 'open_ticket_vw')
#tickets <- sqlQuery(dbase, "select * FROM open_ticket_vw",as.is = TRUE)


dbase <-odbcConnect(dsn = "sql03" ,pwd = "test" , uid = "test")

ticket_data <- sqlFetch(dbase, 'open_ticket_vw')
tickets <- sqlQuery(dbase, "select * FROM open_ticket_vw",as.is = TRUE)
close(dbase)

head(tickets)
table(tickets$NAME)

tickets$REPORT_DATE <- as.Date(tickets$REPORT_DATE, "%Y-%m-%d")
tickets$CLOSE_DATE <- as.Date(tickets$CLOSE_DATE, "%Y-%m-%d")
tickets <- tickets[tickets$CLOSE_DATE < as.Date("2015-05-01") & tickets$REPORT_DATE> as.Date("2014-03-31") & !is.na(tickets$REPORT_DATE )   ,]
#generate month of report
tickets$month_report <- as.factor(format(tickets$REPORT_DATE, "%m")) 
tickets$yr_report <- as.factor(format(tickets$REPORT_DATE, "%Y")) 
table(tickets$month_report[tickets$NAME=="Data Governance"])
table(tickets$yr_report)

#generate month of close
tickets$month_close <- as.factor(format(tickets$CLOSE_DATE, "%m")) 
tickets$yr_close <- as.factor(format(tickets$CLOSE_DATE, "%Y")) 
table(tickets$yr_close  ,tickets$month_close, tickets$LOCATION_NAME)


#consolidted tickets
open_all <-ddply(tickets,.(month_report,yr_report, LOCATION_NAME) ,nrow )
closed_all <-ddply(tickets,.(month_close,yr_close, LOCATION_NAME) ,nrow )
names(open_all) <- c( 'month_report', 'yr_report','LOCATION','count')
names(closed_all) <- c( 'month_report', 'yr_report','LOCATION','count')
open_all$type <- 'open'
closed_all$type <- 'closed'
cons_merged <- rbind(open_all,closed_all)
cons_merged$yr_report <- as.numeric(as.character(cons_merged$yr_report))
cons_merged$month <- as.numeric(as.character(cons_merged$month_report))
cons_merged$month_yr <-  paste(cons_merged$yr_report , cons_merged$month_report)
table(cons_merged$month_yr)


cons_merged$month_report <- factor(cons_merged$month_report,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
#cons_merged$month_yr <-  paste(cons_merged$month_report,"-",cons_merged$yr_report)

  
#2014 Kilifi
cons_mergedAll <- cons_merged[ cons_merged$LOCATION =="Kilifi" & !is.na(cons_merged$month) ,]

cons_mergedAll <- cons_mergedAll[order(cons_mergedAll$month_report, cons_mergedAll$yr_report),]
#cons_mergedAll$month_yr <-  paste(cons_mergedAll$yr_report , cons_mergedAll$month_report)
table(cons_merged$month_yr)

cons_mergedAll$month_yrL <- factor(cons_mergedAll$month_yr,
                                   labels=c("Apr-2014","May-2014","Jun-2014","Jul-2014","Aug-2014",
                                            "Sep-2014","Oct-2014","Nov-2014","Dec-2014", "Jan-2015","Feb-2015","Mar-2015","Apr-2015"),ordered=TRUE)


ggplot(cons_mergedAll[order(cons_mergedAll$month_report, cons_mergedAll$yr_report),], aes( month_yrL ,count, fill=type)) + geom_bar( stat="identity",position="dodge") +
ylab("Number of Tickets") +   xlab("Month") +  ggtitle("Tickets in Kilifi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
theme_bw()  +  scale_fill_manual(values=cbPalette) +  scale_y_continuous(breaks=integer_breaks) +   scale_x_discrete(drop = FALSE)


ggsave(file="images/Consolidated Kilifi 2015.png", width=12.03, height=10.9)


#2014 Nairobi
cons_mergedAll <- cons_merged[ cons_merged$LOCATION =="Nairobi" & !is.na(cons_merged$month) ,]

cons_mergedAll <- cons_mergedAll[order(cons_mergedAll$month_report, cons_mergedAll$yr_report),]
#cons_mergedAll$month_yr <-  paste(cons_mergedAll$yr_report , cons_mergedAll$month_report)
table(cons_merged$month_yr)

cons_mergedAll$month_yrL <- factor(cons_mergedAll$month_yr,
                                   labels=c("Apr-2014","May-2014","Jun-2014","Jul-2014","Aug-2014",
                                            "Sep-2014","Oct-2014","Nov-2014","Dec-2014", "Jan-2015","Feb-2015","Mar-2015","Apr-2015"),ordered=TRUE)

ggplot(cons_mergedAll[order(cons_mergedAll$month_report, cons_mergedAll$yr_report),], aes( month_yrL ,count, fill=type)) + geom_bar( stat="identity",position="dodge") +
  ylab("Number of Tickets") +   xlab("Month") +  ggtitle("Tickets in Nairobi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw() +   scale_fill_manual(values=cbPalette) +  scale_y_continuous(breaks=integer_breaks) +   scale_x_discrete(drop = FALSE)

ggsave(file="images/Consolidated Nairobi 2015.png", width=12.03, height=10.9)


#2014 Mtwapa
cons_mergedAll <- cons_merged[ cons_merged$LOCATION =="Mtwapa" & !is.na(cons_merged$month) ,]

cons_mergedAll <- cons_mergedAll[order(cons_mergedAll$month_report, cons_mergedAll$yr_report),]
#cons_mergedAll$month_yr <-  paste(cons_mergedAll$yr_report , cons_mergedAll$month_report)
table(cons_mergedAll$month_yr)

cons_mergedAll$month_yrL <- factor(cons_mergedAll$month_yr,
                                   labels=c("Apr-2014","May-2014","Jun-2014","Jul-2014","Aug-2014",
                                            "Sep-2014","Oct-2014","Nov-2014","Dec-2014", "Jan-2015","Feb-2015","Mar-2015"),ordered=TRUE)

ggplot(cons_mergedAll[order(cons_mergedAll$month_report, cons_mergedAll$yr_report),], aes( month_yrL ,count, fill=type)) + geom_bar( stat="identity",position="dodge") +
  ylab("Number of Tickets") +   xlab("Month") +  ggtitle("Tickets in Mtwapa") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw() +  scale_fill_manual(values=cbPalette) + scale_y_continuous(breaks=1:20)

ggsave(file="images/Consolidated Mtwapa 2015.png", width=12.03, height=10.9)
