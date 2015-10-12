library(dplyr)
library(magrittr) 
library(data.table)
library(tidyr)
library(diur)
library(reshape)
library(DT)

pulse_panellists_number<-function(start_date, end_dat, panel){
  ## connect to redshift
  db <- rs_dplyr()
  ##List of UK panelists
  uk_pmxids <- tbl(db,"pmxdb_panelmemberships_emea") %>%    
    filter(panelid==panel) %>%     
    select (id)  %>%  
    distinct %>%
    collect %>%                                         
    data.table
  rename(uk_pmxids, c("id"="pmxid"))
  ##Change the date for the script
  end_date<-as.Date(end_dat)  
  end_date<-as.character(end_date + 1 )
  ##### Select pmxids for desktops/laptops (include UK panellists only)
  desktop_laptop_emea <- tbl(db,"wakoopa_emea_web_visits") %>%    
    filter(used_at >= start_date) %>%                  
    filter(used_at < end_date) %>% 
    select (pmxid)  %>%    
    distinct %>%
    collect %>%                                         
    data.table
  ##please note this has to be rune separately (the number of uk pmxids is very large, and that is not enought memory if you try to filter by uk pmxids in the query above)
  desktop_laptop_uk<-inner_join(desktop_laptop_emea, uk_pmxids, by="pmxid") 
  ###Select pmxid, device id for mobiles/tables (websites)and then match device_os and device_kind
  mobile_tablet_emea <- tbl(db,"wakoopa_emea_web_sessions") %>%    
    filter(used_at >= start_date) %>%                  
    filter(used_at < end_date) %>%     
    select (pmxid, device_id)  %>%    
    distinct %>%
    collect %>%                                         
    data.table
  mobile_tablet_uk<-inner_join(mobile_tablet_emea, uk_pmxids, by="pmxid")
  mob_tab_deviceid<-as.vector(mobile_tablet_uk$device_id)
  mob_tab_extra_data<-tbl(db,"wakoopa_emea_device_metadata") %>%  
    filter(device_id %in% mob_tab_deviceid) %>% 
    select (device_id, device_os, device_kind) %>%
    distinct %>%
    collect %>%                                         
    data.table
  mobile_tablet_uk<-left_join(mobile_tablet_uk, mob_tab_extra_data, by = "device_id")
  mobile_tablet_uk<-select (mobile_tablet_uk, pmxid, device_os, device_kind )
  ###Select pmxid, device id for mobiles/tables (apps) and then match device_os and device_kind CHECK!!!!!!!!!!!!!!!!!!!!!
  mobile_tablet_emea_app <- tbl(db,"wakoopa_emea_app_sessions") %>%    
    filter(used_at >= start_date) %>%                  
    filter(used_at < end_date) %>%     
    select (pmxid, device_id)  %>%    
    distinct %>%
    collect %>%                                         
    data.table
  mobile_tablet_uk_app<-inner_join(mobile_tablet_emea_app, uk_pmxids, by="pmxid")
  mob_tab_deviceid_app<-as.vector(mobile_tablet_uk_app$device_id)
  mob_tab_extra_data_app<-tbl(db,"wakoopa_emea_device_metadata") %>%  
    filter(device_id %in% mob_tab_deviceid_app) %>% 
    select (device_id, device_os, device_kind) %>%
    distinct %>%
    collect %>%                                         
    data.table
  mobile_tablet_uk_app<-left_join(mobile_tablet_uk_app, mob_tab_extra_data_app, by = "device_id")
  mobile_tablet_uk_app<-select (mobile_tablet_uk_app, pmxid, device_os, device_kind ) 
  ##Merge tablets/mobiles apps and websites into a single table  !!!!!!!!!!!!!!!!!!!!!!!!!!!CHECK
  mobile_tablet_uk<- rbind(mobile_tablet_uk, mobile_tablet_uk_app)
  mobile_tablet_uk<-unique(mobile_tablet_uk)
  #########Calculate numbers
  ## unique pmxids per device
  uk_desktop<-nrow(desktop_laptop_uk) 
  uk_smatrphone<-nrow(unique(select((filter(mobile_tablet_uk, device_kind=='mobile')),pmxid)))
  uk_smatrphone_ios<-nrow(unique(select((filter(mobile_tablet_uk, device_kind=='mobile'& device_os == 'ios')),pmxid)))
  uk_smatrphone_android<-nrow(unique(select((filter(mobile_tablet_uk, device_kind=='mobile'& device_os == 'android')),pmxid)))
  uk_tablet<-nrow(unique(select((filter(mobile_tablet_uk, device_kind=='tablet')),pmxid)))
  uk_tablet_andr<-nrow(unique(select((filter(mobile_tablet_uk, device_kind=='tablet' & device_os == 'android')),pmxid)))
  uk_tablet_ios<-nrow(unique(select((filter(mobile_tablet_uk, device_kind=='tablet' & device_os == 'ios')),pmxid)))
  ###crossover
  #use desktop_laptop_uk
  tablet_pmxid<-as.data.table(filter(mobile_tablet_uk, device_kind == "tablet"))
  tablet_pmxid<-unique(select(tablet_pmxid, pmxid))
  smartphone_pmxid<-as.data.table(filter(mobile_tablet_uk, device_kind == "mobile"))
  smartphone_pmxid<-unique(select(smartphone_pmxid, pmxid))
  total_panel<-nrow(unique(bind_rows(desktop_laptop_uk, smartphone_pmxid, tablet_pmxid)))
  desk_and_smar<-nrow(inner_join(desktop_laptop_uk, smartphone_pmxid, by = "pmxid", copy = TRUE))
  desk_and_tab<-nrow(inner_join(desktop_laptop_uk, tablet_pmxid, by = "pmxid", copy = TRUE))
  smart_and_tab<-nrow(inner_join(smartphone_pmxid, tablet_pmxid, by = "pmxid", copy = TRUE))
  desk_smar_tab<-inner_join(desktop_laptop_uk, tablet_pmxid, by = "pmxid", copy=TRUE)
  desk_smar_tab<-inner_join(desk_smar_tab, smartphone_pmxid, by = "pmxid", copy = TRUE)
  desk_smar_tab<-nrow(desk_smar_tab)
  
  ###name of the panel
  panel_name_of<-""
  if (panel==4) {panel_name_of <-"GERMAN PANEL"}
  else if (panel==13) {panel_name_of <- "UK PANEL"}
  
  ###Insert the output into data table
  names <- c(panel_name_of, "Start date", "End date","Total panel", "Desktop", "Smartphone", 
             "         Smartphone Android", "Smartphone ios","Tablet", "Tablet Android", "Tablet ios", "CROSSOVER", "Desktop + Smartphone",
             "Desktop + Tablet", "Smartphone + Tablet", "Desktop + Smartphone + Tablet")
  figures<-c(" ", as.character(start_date), as.character(end_dat), total_panel, uk_desktop, uk_smatrphone, uk_smatrphone_android, uk_smatrphone_ios,
             uk_tablet, uk_tablet_andr, uk_tablet_ios, "  ", desk_and_smar, desk_and_tab, smart_and_tab, desk_smar_tab)
  output_data<-as.data.frame(cbind(names, figures))
  colnames(output_data)<-NULL
  row.names(output_data)<-NULL
  return(output_data)
}

shinyServer(function(input, output, session) {
  
  observeEvent(input$goButton, {
    enable("downloadData")
  })
  
  
  data_funct<-eventReactive(input$goButton, {
    
    validate(
      need(as.Date(input$start_date) <= as.Date(input$end_dat), "Start Date should not be older than End Date!!")
    )
    
    pulse_panellists_number(input$start_date,input$end_dat, input$panel)})
  
  output$table <-DT::renderDataTable(datatable(
    data_funct(), filter = "top",  
    options = list(pageLength = 16, autoWidth = TRUE, lengthMenu = c(5, 10, 16)),
    rownames= FALSE
  ))
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("Pulse_report", input$start_date, "to",input$end_dat, ".csv", sep='') },
    content = function(file) {
      write.csv(data_funct(), file)
    }
  )
}) 
