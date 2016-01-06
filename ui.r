library(shiny)
library(shinyBS)
library(shinysky)
shinyUI(navbarPage('Service Desk Ticket Reports',
                   tabPanel("Graph Reports",
                            sidebarLayout(
                              sidebarPanel(
                                helpText(),
                                selectInput('month', 'Select the month to be used to create the summary:',names(table(tickets$month_reportX)), selected= format( Sys.Date(), "%b")),
                                radioButtons('location', 'Select Location', c('Kilifi', 'Nairobi', 'Mtwapa'),
                                             inline = TRUE),
                                downloadButton('downloadGraph'),
                                
                                bsTooltip("month", paste("Select a month less than" ,format(Sys.Date(), '%b')), options = list(container = "body"))
                              ),
                              mainPanel(
                                # selected3 <- paste("01" , input.month , format(Sys.Date(), "%Y") , sep = "-"),
                                #selected_D3 <-as.Date(selected3 , format = "%d-%b-%Y"), 
                                
                                
                                div(    plotOutput('yearlyPlot')   ), 
                                div(class="container-fluid span6", 
                                    shinyalert("shinyalert1", FALSE,auto.close.after = 10)
                                )
                              ) #end main panel
                            ) #end side bar layout      
                   ), #end tabs panel
                   tabPanel("Table Reports",
                      
                            sidebarLayout(
                              sidebarPanel(
                                helpText(),
                                selectInput('yrTicket', 'Select the year to be used to create the summary:', sort(as.numeric(names(table(all_merged$yr_report ))), decreasing = TRUE), selected= format( Sys.Date(), "%Y")),
                                radioButtons('type', 'Select the ticke type', c('Open', 'Closed'),
                                             inline = TRUE),
                                downloadButton('downloadTicketTable'),
                                
                                bsTooltip("month", paste("Select a month less than" ,format(Sys.Date(), '%b')), options = list(container = "body"))
                              ),
                              mainPanel(
                                # selected3 <- paste("01" , input.month , format(Sys.Date(), "%Y") , sep = "-"),
                                #selected_D3 <-as.Date(selected3 , format = "%d-%b-%Y"), 
                                
                                
                                div(       dataTableOutput("ticket_tables")  ), 
                                div(class="container-fluid span6", 
                                    shinyalert("shinyalert1", FALSE,auto.close.after = 10)
                                )
                              ) #end main panel
                            ) #end side bar layout  
                         
                            
                            )    #end tabs panel        
                   
                   
))

