####Explore the link https://gist.github.com/withr/8799489

library(dplyr)
library(magrittr) 
library(data.table)
library(tidyr)
library(diur)
library(reshape)
library(DT)
library(shinyjs)

db <- rs_dplyr()        ####or to run stuff quicker it can be hardcoded - e.g. sysDate - 2
max_date<-tbl(db,"wakoopa_emea_web_sessions") %>%
  summarise(mindate=max(used_at)) %>%
  collect %>%
  data.table

max_date<-as.vector(max_date)
max_date<-as.vector(as.matrix(max_date))
max_date<-as.Date(max_date)
max_date<-as.character(max_date)

# Define UI for application 
shinyUI(fluidPage(
  
  sidebarPanel(
    
    ##logo
    tags$img(height=30, width=300, 
             src = "http://cdn.yougov.com/cumulus_uploads/document/d7d2l9gywe/YGV-Pulse.png"),
    # Application title
    tags$br(),
    titlePanel("Panellist Report"),
    tags$hr(),
    
    # Select input
    dateInput(inputId = "start_date", "Start date", value = max_date, min = "2013-09-10",
              max = max_date, format = "yyyy-mm-dd", startview = "month",
              weekstart = 0, language = "en"),
    
    dateInput(inputId = "end_dat", "End date", value = max_date, min = "2013-09-10",
              max = max_date, format = "yyyy-mm-dd", startview = "month",
              weekstart = 0, language = "en"),
    tags$hr(),
    
    radioButtons("panel", "Panel:",
                 c("UK" = 13,
                   "German" = 4
                 ), ),
    tags$hr(),
    
    useShinyjs(),
    actionButton("goButton", "Generate report"),
    disabled(downloadButton('downloadData', 'Download'))
    
  ),
  
  mainPanel(
    div(class = "busy",  
        img(height=120, width=600,src="http://www.barchart.com/shared/images/progress_bar.gif")
    ),
    
    ## loading msg
    tagList(
      tags$head(
        tags$link(rel="stylesheet", type="text/css",href="style.css"),
        tags$script(type="text/javascript", src = "busy.js")
      )
    ),
    tags$br(),
    tags$br(),
    tags$br(),
    
    div(DT::dataTableOutput("table"), style = "font-size:75%")
    

  )
))
