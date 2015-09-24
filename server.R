#cut(Sys.Date(), "month")
#ymd(Sys.Date()) - years(1)
#
shinyServer(function(input, output,session){
  library(lubridate)
  library(shiny)
  library(ggplot2)
  library(shinyBS)
  
  
  
  #2014 Kilifi
  
  #to create a subset of the admsions data for graphs
  consolidated <- reactive( {
    last_D <- format(ymd(Sys.Date()) - years(1), "%Y-%m-%d")
    last_D  <- format(ymd(last_D) - months(1), "%Y-%m-%d")
    last_D  <- cut(as.Date(last_D , format = "%Y-%m-%d"), "month")
    cur_D <- cut(Sys.Date(), "month")
    
    #change the selected dates to last year
    selected <- paste("01" , input$month , format(Sys.Date(), "%Y") , sep = "-")
    selected_D <-as.Date(selected , format = "%d-%b-%Y")
    
    ##change to last year
    selected_D_last <- as.Date(ymd(selected_D) - years(1))
    selected_D_last  <- format(ymd(selected_D_last) - months(1), "%Y-%m-%d")
    
    
    tickets <- tickets[tickets$CLOSE_DATE < selected_D  & tickets$REPORT_DATE> selected_D_last & !is.na(tickets$REPORT_DATE )   ,]
    #consolidted tickets
    open_all <-ddply(tickets,.(month_report,yr_report, LOCATION_NAME,month_report2) ,nrow )
    closed_all <-ddply(tickets,.(month_close,yr_close, LOCATION_NAME , month_close2) ,nrow )
    names(open_all) <- c( 'month_report', 'yr_report','LOCATION','month_2' ,'count')
    names(closed_all) <- c( 'month_report', 'yr_report','LOCATION','month_2' , 'count')
    open_all$type <- 'open'
    closed_all$type <- 'closed'
    cons_merged <- rbind(open_all,closed_all)
    cons_merged$yr_report <- as.numeric(as.character(cons_merged$yr_report))
    cons_merged$month <- as.numeric(as.character(cons_merged$month_report))
    cons_merged$month_yr <-  paste(cons_merged$yr_report , cons_merged$month_report)
    cons_merged$month_yr2 <-  paste(cons_merged$yr_report , cons_merged$month_2)
    # table(cons_merged$month_yr2)
    
    cons_merged$month_report <- factor(cons_merged$month_report,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
    cons_merged
    
  })
  
  
  
  plotInput <- reactive( {
    selected2 <- paste("01" , input$month , format(Sys.Date(), "%Y") , sep = "-")
    selected_D2 <-as.Date(selected2 , format = "%d-%b-%Y")
    
    if (input$location=="Kilifi" & Sys.Date()>selected_D2)  {
      cons_mergedAll <- consolidated()
      cons_mergedAll <- cons_mergedAll[ cons_mergedAll$LOCATION =="Kilifi" & !is.na(cons_mergedAll$month) ,]
      
      cons_mergedAll$month_yr_s <- as.numeric(gsub(" " ,"",x=cons_mergedAll$month_yr))
      cons_mergedAll <- cons_mergedAll[order(cons_mergedAll$month_yr_s),]
      cons_mergedAll$order = c(1:length(cons_mergedAll$count))
      
      
      
      graphKlf <-  ggplot(cons_mergedAll , aes( reorder(month_yr2, order) ,y=count, fill=type )) + geom_bar( stat="identity",position="dodge") +
        ylab("Number of Tickets") +   xlab("Month") +  ggtitle("Tickets in Kilifi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
        theme_bw()  +  scale_fill_manual(values=cbPalette) +  scale_y_continuous(breaks=integer_breaks) +   scale_x_discrete(drop = FALSE)
      
      
      
      #ggsave(file="images/Consolidated Kilifi 2015.png", width=12.03, height=10.9)
      
      # print(graphKlf) 
      
    }
    else if (input$location=="Nairobi" & Sys.Date()>selected_D2)  {
      cons_mergedAll <- consolidated()
      cons_mergedAll <- cons_mergedAll[ cons_mergedAll$LOCATION =="Nairobi" & !is.na(cons_mergedAll$month) ,]
      
      cons_mergedAll$month_yr_s <- as.numeric(gsub(" " ,"",x=cons_mergedAll$month_yr))
      cons_mergedAll <- cons_mergedAll[order(cons_mergedAll$month_yr_s),]
      cons_mergedAll$order = c(1:length(cons_mergedAll$count))
      
      
      graphNrb <-  ggplot(cons_mergedAll , aes( reorder(month_yr2, order) ,y=count, fill=type )) + geom_bar( stat="identity",position="dodge") +
        ylab("Number of Tickets") +   xlab("Month") +  ggtitle("Tickets in Nairobi") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
        theme_bw()  +  scale_fill_manual(values=cbPalette) +  scale_y_continuous(breaks=integer_breaks) +   scale_x_discrete(drop = FALSE)
      
      
      
      #ggsave(file="images/Consolidated Kilifi 2015.png", width=12.03, height=10.9)
      
      #  print(graphNrb) 
      
    }
    
    else if (input$location=="Mtwapa" & Sys.Date()>selected_D2)  {
      cons_mergedAll <- consolidated()
      cons_mergedAll <- cons_mergedAll[ cons_mergedAll$LOCATION =="Mtwapa" & !is.na(cons_mergedAll$month) ,]
      
      cons_mergedAll$month_yr_s <- as.numeric(gsub(" " ,"",x=cons_mergedAll$month_yr))
      cons_mergedAll <- cons_mergedAll[order(cons_mergedAll$month_yr_s),]
      cons_mergedAll$order = c(1:length(cons_mergedAll$count))
      
      
      graphMtp <-  ggplot(cons_mergedAll , aes( reorder(month_yr2, order) ,y=count, fill=type )) + geom_bar( stat="identity",position="dodge") +
        ylab("Number of Tickets") +   xlab("Month") +  ggtitle("Tickets in Mtwapa") +theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
        theme_bw()  +  scale_fill_manual(values=cbPalette) +  scale_y_continuous(breaks=integer_breaks) +   scale_x_discrete(drop = FALSE)
      
      
      
      #ggsave(file="images/Consolidated Kilifi 2015.png", width=12.03, height=10.9)
      
      # print(graphMtp) 
      
    }
    
    else   {
      #warning("Selected a future month")
      
      #  addPopover(session,id="yearlyPlot", title="!!FUTURE MONTH!!", content = paste0("You have selected a month which is
      #                                                            after the current month"), trigger = 'hover')
      #
      showshinyalert(session, "shinyalert1", paste("Select a month less than" ,format(Sys.Date(), '%b'), 
                                                   "WARNING: You have selected a future month"), styleclass = "danger")
      
    }
    
  })
  
  output$yearlyPlot <- renderPlot({
    print(plotInput())
  })
  
  
  
  
  
  # output$downloadGraph <- downloadHandler(
  #   filename <-  function() { paste(input$location,"_consolidated" , '.png', sep='') },
  #   #filename <- paste(input$location,"_consolidated" , '.jpeg', sep=''),
  #  content <-  function(file) {
  #   
  #    # jpeg(file)
  #   plot <-  plotInput()
  #    print(plot)
  #   dev.copy(png,file)
  #     dev.off()
  #     #
  #   })
  
  
  output$downloadGraph <- downloadHandler(
    filename = function() {
      
      paste(input$location,"_consolidated" , '.png', sep='')
    },
    content = function(file) {
      name <- paste(input$location,"_consolidated" , '.png', sep='')
      
      ggsave(name, plotInput() )
      file.copy(name, file, overwrite=TRUE)
    }
  )
  
  
  
  
})#end server
