library(ggplot2)
library("plyr")
library("dplyr")
#tickets <- read.csv("data/servicedesk data.csv")
#tickets <- read.csv("data/jan_feb_2015.csv")
dbase <-odbcConnect(dsn = "sql03" ,pwd = "test" , uid = "test")

ticket_data <- sqlFetch(dbase, 'open_ticket_vw')
tickets <- sqlQuery(dbase, "select * FROM open_ticket_vw",as.is = TRUE)
close(dbase)

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
cons_merged$month_report <- factor(cons_merged$month_report,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)

#----kilifi ----
#2013
cons_merged_2013 <- cons_merged[cons_merged$yr_report==2013 & cons_merged$LOCATION =="Kilifi" & !is.na(cons_merged$month) ,]

ggplot(cons_merged_2013, aes(month_report ,count, fill=type)) + geom_bar( stat="identity") +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in 2013 Kilifi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()
ggsave(file="images/2013 Consolidated Kilifi.png", width=12.03, height=10.9)


#2014
cons_merged_2014 <- cons_merged[cons_merged$yr_report==2014 & cons_merged$LOCATION =="Kilifi" & !is.na(cons_merged$month) ,]

ggplot(cons_merged_2014, aes(month_report ,count, fill=type)) + geom_bar( stat="identity") +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in 2014 Kilifi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()
ggsave(file="images/2014 Consolidated Kilifi.png", width=12.03, height=10.9)


#2015
cons_merged_2015 <- cons_merged[cons_merged$yr_report==2015 & cons_merged$LOCATION =="Kilifi" & !is.na(cons_merged$month) ,]

ggplot(cons_merged_2015, aes(month_report ,count, fill=type)) + geom_bar( stat="identity") +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in 2015 Kilifi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()
#+    geom_text(aes(y = count, label = count , stat="identity"), vjust = 3 )
ggsave(file="images/2015 Consolidated Kilifi.png", width=12.03, height=10.9)



#----Nairobi ----
#2013
cons_merged_2013 <- cons_merged[cons_merged$yr_report==2013 & cons_merged$LOCATION =="Nairobi" & !is.na(cons_merged$month) ,]

ggplot(cons_merged_2013, aes(month_report ,count, fill=type)) + geom_bar( stat="identity") +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in 2013 Nairobi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()
ggsave(file="images/2013 Consolidated Nairobi.png", width=12.03, height=10.9)


#2014
cons_merged_2014 <- cons_merged[cons_merged$yr_report==2014 & cons_merged$LOCATION =="Nairobi" & !is.na(cons_merged$month) ,]

ggplot(cons_merged_2014, aes(month_report ,count, fill=type)) + geom_bar( stat="identity") +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in 2014 Nairobi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()
ggsave(file="images/2014 Consolidated Nairobi.png", width=12.03, height=10.9)


#2015
cons_merged_2015 <- cons_merged[cons_merged$yr_report==2015 & cons_merged$LOCATION =="Nairobi" & !is.na(cons_merged$month) ,]

ggplot(cons_merged_2015, aes(month_report ,count, fill=type)) + geom_bar( stat="identity") +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in 2015 Nairobi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()
ggsave(file="images/2015 Consolidated Nairobi.png", width=12.03, height=10.9)


#----Mtwapa ----
#2013
cons_merged_2013 <- cons_merged[cons_merged$yr_report==2013 & cons_merged$LOCATION =="Mtwapa" & !is.na(cons_merged$month) ,]

ggplot(cons_merged_2013, aes(month_report ,count, fill=type)) + geom_bar( stat="identity") +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in 2013 Mtwapa") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()

ggsave(file="images/2013 Consolidated Mtwapa.png", width=12.03, height=10.9)

#2014
cons_merged_2014 <- cons_merged[cons_merged$yr_report==2014 & cons_merged$LOCATION =="Mtwapa" & !is.na(cons_merged$month) ,]

ggplot(cons_merged_2014, aes(month_report ,count, fill=type)) + geom_bar( stat="identity") +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in 2014 Mtwapa") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()
ggsave(file="images/2014 Consolidated Mtwapa.png", width=12.03, height=10.9)
#2015
cons_merged_2015 <- cons_merged[cons_merged$yr_report==2015 & cons_merged$LOCATION =="Mtwapa" & !is.na(cons_merged$month) ,]

ggplot(cons_merged_2015, aes(month_report ,count, fill=type)) + geom_bar( stat="identity") +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in 2015 Mtwapa") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()
ggsave(file="images/2015 Consolidated Mtwapa.png", width=12.03, height=10.9)

#-------------------------graphs for specific groups-------------------------------------------
open <-ddply(tickets,.(NAME,month_report,yr_report, LOCATION_NAME) ,nrow )
closed <-ddply(tickets,.(NAME,month_close,yr_close, LOCATION_NAME) ,nrow )
names(open) <- c('NAME', 'month_report', 'yr_report','LOCATION','count')
names(closed) <- c('NAME', 'month_report', 'yr_report','LOCATION','count')
#zz3 <- merge(zz,zz2, by=c('NAME' , 'month_report', 'yr_report'))
open$type <- 'open'
closed$type <- 'closed'


all_merged <- rbind(open,closed)
all_merged$yr_report <- as.numeric(as.character(all_merged$yr_report))
all_merged$month <- as.numeric(as.character(all_merged$month_report))
all_merged$month_report <- factor(all_merged$month_report,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
write.csv(all_merged[all_merged$yr_report==2015,], "~/tickets/groups_2015.csv" ,  row.names=FALSE )

all_merged$NAME <- factor(all_merged$NAME,
                    levels = c("Data Governance","Desktop Support","Development" , "Information Services" , "Infrastructure Support", "Lab Support", "Printer Support" , "Service Desk"),
                    labels = c("DG", "DS", "D" , "IS" , "InS", "LS" , "PS" , "SD")) 

#-------------------------------Kilifi------------------------------------------------
#Kilifi 2013
all_merged2 <- all_merged[all_merged$yr==2013 & all_merged$month <7 & all_merged$LOCATION =="Kilifi" & !is.na(all_merged$month) ,]

ggplot(all_merged2, aes(NAME ,count, fill=type)) + geom_bar( stat="identity") + facet_grid(. ~ month_report) +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Jan-June 2013 Kilifi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()

ggsave(file="images/Jan-June 2013 Kilifi.png", width=12.03, height=10.9)

all_merged3 <- all_merged[all_merged$yr==2013 & all_merged$month >6 & all_merged$LOCATION =="Kilifi" & !is.na(all_merged$month) ,]
ggplot(all_merged3, aes(NAME ,count, fill=type)) + geom_bar( stat="identity") + facet_grid(. ~ month_report) +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Jul-Dec 2013 Kilifi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()
ggsave(file="images/Jul-Dec 2013 Kilifi.png", width=12.03, height=10.9)

#Kilifi 2014
all_merged2 <- all_merged[all_merged$yr==2014 & all_merged$month <7 & all_merged$LOCATION =="Kilifi" & !is.na(all_merged$month) ,]

ggplot(all_merged2, aes(NAME ,count, fill=type)) + geom_bar( stat="identity") + facet_grid(. ~ month_report) +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Jan-June 2014 Kilifi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()
ggsave(file="images/Jan-June 2014 Kilifi.png", width=12.03, height=10.9)


all_merged3 <- all_merged[all_merged$yr==2014 & all_merged$month >6 & all_merged$LOCATION =="Kilifi" & !is.na(all_merged$month) ,]
ggplot(all_merged3, aes(NAME ,count, fill=type)) + geom_bar( stat="identity") + facet_grid(. ~ month_report) +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Jul-Dec 2014 Kilifi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()
ggsave(file="images/Jul-Dec 2014 Kilifi.png", width=12.03, height=10.9)


#Kilifi 2015
all_merged2 <- all_merged[all_merged$yr==2015 & all_merged$month <7 & all_merged$LOCATION =="Kilifi" & !is.na(all_merged$month) ,]

ggplot(all_merged2, aes(NAME ,count, fill=type)) + geom_bar( stat="identity") + facet_grid(. ~ month_report) +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Jan-Mar 2015 Kilifi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()

ggsave(file="images/Jan-June 2015 Kilifi.png", width=12.03, height=10.9)

all_merged3 <- all_merged[all_merged$yr==2015 & all_merged$month >6 & all_merged$LOCATION =="Kilifi" & !is.na(all_merged$month) ,]
ggplot(all_merged3, aes(NAME ,count, fill=type)) + geom_bar( stat="identity") + facet_grid(. ~ month_report) +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Jul-Mar 2015 Kilifi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()
ggsave(file="images/Jul-Dec 2015 Kilifi.png", width=12.03, height=10.9)

#-------------------------------Nairobi------------------------------------------------
#Nairobi 2013
all_merged2 <- all_merged[all_merged$yr==2013 & all_merged$month <7 & all_merged$LOCATION =="Nairobi" & !is.na(all_merged$month) ,]

ggplot(all_merged2, aes(NAME ,count, fill=type)) + geom_bar( stat="identity") + facet_grid(. ~ month_report) +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Jan-June 2013 Nairobi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()

ggsave(file="images/Jan-June 2013 Nairobi.png", width=12.03, height=10.9)

all_merged3 <- all_merged[all_merged$yr==2013 & all_merged$month >6 & all_merged$LOCATION =="Nairobi" & !is.na(all_merged$month) ,]
ggplot(all_merged3, aes(NAME ,count, fill=type)) + geom_bar( stat="identity") + facet_grid(. ~ month_report) +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Jul-Dec 2013 Nairobi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()
ggsave(file="images/Jul-Dec 2013 Nairobi.png", width=12.03, height=10.9)

#Nairobi 2014
all_merged2 <- all_merged[all_merged$yr==2014 & all_merged$month <7 & all_merged$LOCATION =="Nairobi" & !is.na(all_merged$month) ,]

ggplot(all_merged2, aes(NAME ,count, fill=type)) + geom_bar( stat="identity") + facet_grid(. ~ month_report) +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Jan-June 2014 Nairobi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()

ggsave(file="images/Jan-June 2014 Nairobi.png", width=12.03, height=10.9)

all_merged3 <- all_merged[all_merged$yr==2014 & all_merged$month >6 & all_merged$LOCATION =="Nairobi" & !is.na(all_merged$month) ,]
ggplot(all_merged3, aes(NAME ,count, fill=type)) + geom_bar( stat="identity") + facet_grid(. ~ month_report) +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Jul-Dec 2014 Nairobi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()
ggsave(file="images/Jul-Dec 2014 Nairobi.png", width=12.03, height=10.9)

#Nairobi 2015
all_merged2 <- all_merged[all_merged$yr==2015 & all_merged$month <7 & all_merged$LOCATION =="Nairobi" & !is.na(all_merged$month) ,]

ggplot(all_merged2, aes(NAME ,count, fill=type)) + geom_bar( stat="identity") + facet_grid(. ~ month_report) +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Jan-Mar 2015 Nairobi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()
ggsave(file="images/Jan-June 2015 Nairobi.png", width=12.03, height=10.9)


all_merged3 <- all_merged[all_merged$yr==2015 & all_merged$month >6 & all_merged$LOCATION =="Nairobi" & !is.na(all_merged$month) ,]
ggplot(all_merged3, aes(NAME ,count, fill=type)) + geom_bar( stat="identity") + facet_grid(. ~ month_report) +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Jul-Dec 2015 Nairobi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()

ggsave(file="images/Jul-Dec 2015 Nairobi.png", width=12.03, height=10.9)

#-------------------------------Mtwapa------------------------------------------------
#Mtwapa 2013
all_merged2 <- all_merged[all_merged$yr==2013 & all_merged$month <7 & all_merged$LOCATION =="Mtwapa" & !is.na(all_merged$month) ,]

ggplot(all_merged2, aes(NAME ,count, fill=type)) + geom_bar( stat="identity") + facet_grid(. ~ month_report) +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Jan-June 2013 Mtwapa") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()

ggsave(file="images/Jan-June 2013 Mtwapa.png", width=12.03, height=10.9)

all_merged3 <- all_merged[all_merged$yr==2013 & all_merged$month >6 & all_merged$LOCATION =="Mtwapa" & !is.na(all_merged$month) ,]
ggplot(all_merged3, aes(NAME ,count, fill=type)) + geom_bar( stat="identity") + facet_grid(. ~ month_report) +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Jul-Dec 2013 Mtwapa") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()
ggsave(file="images/Jul-Dec 2013 Mtwapa.png", width=12.03, height=10.9)

#Mtwapa 2014
all_merged2 <- all_merged[all_merged$yr==2014 & all_merged$month <7 & all_merged$LOCATION =="Mtwapa" & !is.na(all_merged$month) ,]

ggplot(all_merged2, aes(NAME ,count, fill=type)) + geom_bar( stat="identity") + facet_grid(. ~ month_report) +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Jan-June 2014 Mtwapa") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()

ggsave(file="images/Jan-June 2014 Mtwapa.png", width=12.03, height=10.9)

all_merged3 <- all_merged[all_merged$yr==2014 & all_merged$month >6 & all_merged$LOCATION =="Mtwapa" & !is.na(all_merged$month) ,]
ggplot(all_merged3, aes(NAME ,count, fill=type)) + geom_bar( stat="identity") + facet_grid(. ~ month_report) +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Jul-Dec 2014 Mtwapa") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()

ggsave(file="images/Jul-Dec 2014 Mtwapa.png", width=12.03, height=10.9)

#Mtwapa 2015
all_merged2 <- all_merged[all_merged$yr==2015 & all_merged$month <7 & all_merged$LOCATION =="Mtwapa" & !is.na(all_merged$month) ,]

ggplot(all_merged2, aes(NAME ,count, fill=type)) + geom_bar( stat="identity") + facet_grid(. ~ month_report) +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Jan-Mar 2015 Mtwapa") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()

ggsave(file="images/Jan-June 2015 Mtwapa.png", width=12.03, height=10.9)

all_merged3 <- all_merged[all_merged$yr==2015 & all_merged$month >6 & all_merged$LOCATION =="Mtwapa" & !is.na(all_merged$month) ,]
ggplot(all_merged3, aes(NAME ,count, fill=type)) + geom_bar( stat="identity") + facet_grid(. ~ month_report) +
  ylab("Number of Tickets") +   xlab("Groups") +  ggtitle("Tickets in Jul-Dec 2015 Mtwapa") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme_bw()

ggsave(file="images/Jul-Dec 2015 Mtwapa.png", width=12.03, height=10.9)



tickets2 <-  ddply(tickets,.(NAME,month_report,yr_report) ,transform,counts_open=length(job_ticket_id))
tickets2 <-  ddply(tickets2,.(NAME,month_close,yr_close) ,transform,counts_close=length(job_ticket_id) )
