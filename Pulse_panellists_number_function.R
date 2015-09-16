######Function to generate csv export with number of panelists per device/operating system and crossover of devices

pulse_panellists_number<-function(start_date, end_dat){
  
  ###load libraries  
  library(dplyr)
  library(magrittr) 
  library(data.table)
  library(tidyr)
  library(diur)
  library(reshape)
  
  ## connect to redshift
  db <- rs_dplyr()
   
  ##List of UK panelists
  uk_pmxids <- tbl(db,"pmxdb_panelmemberships_emea") %>%    
    filter(panelid==13) %>%     
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
  
  ###Select pmxid, device id for mobiles/tables and then match device_os and device_kind
  
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
  
  #########Calculate numbers
  
  ## unique pmxids per device
   
  uk_desktop<-nrow(desktop_laptop_uk) 
  uk_smatrphone<-nrow(unique(select((filter(mobile_tablet_uk, "device_kind=='mobile'")),pmxid)))
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
  
  ###Insert the output into data table
  
  names <- c("Start date", "End date","Total panel", "Desktop", "Smartphone", 
             "Smartphone Android", "Smartphone ios","Tablet", "Tablet Android", "Tablet ios", "CROSSOVER", "Desktop + Smartphone",
             "Desktop + Tablet", "Smartphone + Tablet", "Desktop + Smartphone + Tablet")
  
  
  figures<-c(start_date, end_dat, total_panel, uk_desktop, uk_smatrphone, uk_smatrphone_android, uk_smatrphone_ios,
             uk_tablet, uk_tablet_andr, uk_tablet_ios, "  ", desk_and_smar, desk_and_tab, smart_and_tab, desk_smar_tab)
  
  output<-as.data.frame(cbind(names, figures))
  colnames(output)<-NULL
  
  
  ###Save the output
  
  file_name<-paste("Pulse_panel_report_", start_date, end_dat,".csv")
  
  write.csv(output, file_name, row.names = F)
  
}











