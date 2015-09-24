library(ggplot2)
library("plyr")
library("dplyr")
library(RODBC) #connect to the database sudo apt-get install libiodbc2-dev
library(scales)

cbPalette <- c( "#0033ff" ,"#009900")
# integer_breaks <- function(x){
# if(x > 10 ){
#   seq.int(0, ceiling(max(x)) ,by = 10) 
#   }
# else{
#   seq.int(0, ceiling(max(x)))
# }}

integer_breaks <- function(x){
    seq.int(0, ceiling(max(x)) ,by = 10) 
    }



#connect to the database
dbase <-odbcConnect(dsn = "sql03" ,pwd = "test" , uid = "test")
ticket_data <- sqlFetch(dbase, 'open_ticket_vw')
tickets <- sqlQuery(dbase, "select * FROM open_ticket_vw",as.is = TRUE)
close(dbase)


#tickets <- read.csv("data/servicedesk data.csv")
#tickets <- read.csv("data/ticket_as.csv")


head(tickets)
table(tickets$NAME)

tickets$REPORT_DATE <- as.Date(tickets$REPORT_DATE, "%Y-%m-%d")
tickets$CLOSE_DATE <- as.Date(tickets$CLOSE_DATE, "%Y-%m-%d")

#generate month of report
tickets$month_report <- as.factor(format(tickets$REPORT_DATE, "%m")) 
tickets$yr_report <- as.factor(format(tickets$REPORT_DATE, "%Y")) 
table(tickets$month_report[tickets$NAME=="Data Governance"])
table(tickets$yr_report)

#generate month of close
tickets$month_close <- as.factor(format(tickets$CLOSE_DATE, "%m")) 
tickets$yr_close <- as.factor(format(tickets$CLOSE_DATE, "%Y")) 
table(tickets$month_close)

#keep only tickets for the previous month
table(tickets$yr_report ,tickets$month_report)
tickets <- subset(tickets ,tickets$month_report=='04' & tickets$yr_report=='2015')
write.csv(tickets,"data/tickets_april_2015.csv")

#consolidted tickets
open_all <-ddply(tickets,.(month_report, LOCATION_NAME) ,nrow )
closed_all <-ddply(tickets,.(month_close, LOCATION_NAME) ,nrow )
names(open_all) <- c( 'month_report','LOCATION','count')
names(closed_all) <- c( 'month_report','LOCATION','count')
open_all$type <- 'open'
closed_all$type <- 'closed'
cons_merged <- rbind(open_all,closed_all)
#cons_merged$yr_report <- as.numeric(as.character(cons_merged$yr_report))
cons_merged$month <- as.numeric(as.character(cons_merged$month_report))
#cons_merged$month_report <- factor(cons_merged$month_report,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)


#----kilifi ----

#2015
cons_merged_2015 <- cons_merged[ cons_merged$LOCATION =="Kilifi" & !is.na(cons_merged$month) ,]

ggplot(cons_merged_2015, aes(month_report ,count, fill=type)) + geom_bar( stat="identity") +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Apr 2015 Kilifi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw() +  scale_fill_manual(values=cbPalette)
#+    geom_text(aes(y = count, label = count , stat="identity"), vjust = 3 )
ggsave(file="images/2015 Apr Consolidated Kilifi.png", width=12.03, height=10.9)


#----Nairobi ----
#2015
cons_merged_2015 <- cons_merged[cons_merged$LOCATION =="Nairobi" & !is.na(cons_merged$month) ,]

ggplot(cons_merged_2015, aes(month_report ,count, fill=type)) + geom_bar( stat="identity") +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Apr 2015 Nairobi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw() +  scale_fill_manual(values=cbPalette)
ggsave(file="images/2015 Apr Consolidated Nairobi.png", width=12.03, height=10.9)


#----Mtwapa ----
#2015
cons_merged_2015 <- cons_merged[cons_merged$LOCATION =="Mtwapa" & !is.na(cons_merged$month) ,]

ggplot(cons_merged_2015, aes(month_report ,count, fill=type)) + geom_bar( stat="identity") +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Apr 2015 Mtwapa") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw() +  scale_fill_manual(values=cbPalette)
ggsave(file="images/2015 Apr Consolidated Mtwapa.png", width=12.03, height=10.9)


#-------------------------graphs for specific groups-------------------------------------------
open <-ddply(tickets,.(NAME,month_report, LOCATION_NAME) ,nrow )
closed <-ddply(tickets,.(NAME,month_close, LOCATION_NAME) ,nrow )
names(open) <- c('NAME', 'month_report','LOCATION','count')
names(closed) <- c('NAME', 'month_report','LOCATION','count')
#zz3 <- merge(zz,zz2, by=c('NAME' , 'month_report', 'yr_report'))
open$type <- 'open'
closed$type <- 'closed'


all_merged <- rbind(open,closed)
#all_merged$yr_report <- as.numeric(as.character(all_merged$yr_report))
all_merged$month <- as.numeric(as.character(all_merged$month_report))
#all_merged$month_report <- factor(all_merged$month_report,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
all_merged$NAME <- factor(all_merged$NAME,
                          levels = c("Data Governance","Desktop Support","Development" , "Information Services" , "Infrastructure Support", "Lab Support", "Printer Support" , "Service Desk"),
                          labels = c("DG", "DS", "D" , "IS" , "InS", "LS" , "PS" , "SD")) 


#Kilifi 2015
all_merged2 <- all_merged[all_merged$LOCATION =="Kilifi" & !is.na(all_merged$month) ,]
all_merged2 <- all_merged2[all_merged2$month!=5 ,]
# ggplot(all_merged2, aes(NAME ,count, fill=type)) + geom_bar( stat="identity") + facet_grid(. ~ month_report) +
#   ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Mar 2015 Kilifi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
#   theme_bw()



ggplot(all_merged2, aes(NAME ,count, fill=type)) + geom_bar( stat="identity",position="dodge") +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Apr 2015 Kilifi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()  +  scale_fill_manual(values=cbPalette) +  facet_grid(. ~ month_report) +  scale_y_continuous(breaks=integer_breaks) +
  scale_x_discrete(drop = FALSE)
ggsave(file="images/Apr 2015 Kilifi.png", width=12.03, height=10.9)





#Nairobi 
all_merged3 <- all_merged[ all_merged$LOCATION =="Nairobi" & !is.na(all_merged$month) ,]
all_merged3 <- all_merged3[all_merged3$month!=5 ,]
ggplot(all_merged3, aes(NAME ,count, fill=type)) + geom_bar( stat="identity",position="dodge") +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Apr 2015 Nairobi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()  +  scale_fill_manual(values=cbPalette) +  facet_grid(. ~ month_report) +  scale_y_continuous(breaks=integer_breaks) +
  scale_x_discrete(drop = FALSE)
ggsave(file="images/Apr 2015 Nairobi.png", width=12.03, height=10.9)


#Mtwapa 2015
all_merged4 <- all_merged[ all_merged$LOCATION =="Mtwapa" & !is.na(all_merged$month) ,]
all_merged4 <- all_merged4[all_merged4$month!=5 ,]
ggplot(all_merged4, aes(NAME ,count, fill=type)) + geom_bar( stat="identity",position="dodge") +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Apr 2015 Mtwapa") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()  +  scale_fill_manual(values=cbPalette) +  facet_grid(. ~ month_report) +  scale_y_continuous(breaks=integer_breaks) +
  scale_x_discrete(drop = FALSE)

ggsave(file="images/Apr 2015 Mtwapa.png", width=12.03, height=10.9)