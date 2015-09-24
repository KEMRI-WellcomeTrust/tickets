library(shiny)
library(shinyBS)
library(shinysky)
shinyUI(fluidPage(
  title = 'Download a Ticket Reports',
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
      
    
    
   
    
    )
  )
))

