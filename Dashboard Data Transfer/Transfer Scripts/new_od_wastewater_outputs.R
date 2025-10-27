#This script reads in "Wastewater" open data outputs and save them ti Open Data folder
#24/10/2025
#Nipuni Rajapaksha

#* Week is from Saturday to Friday

file_suffix<-c("HB","LA","scotland","wwtw")

ww_files_list<-
  map(file_suffix, 
      ~read_csv(match_base_filename(paste0(file_paths$Data$Open_data_folder,
                       "covid19_wastewater_",.x,".csv")))
  )

#assign them to environment with a prefix
walk2(ww_files_list, file_suffix, 
      ~ assign(paste0("wastewater_covid19_", .y), .x, envir = .GlobalEnv))

#1.Scotland level---------------------------------------------------------------

ww_scotland<-wastewater_covid19_scotland %>% 
  mutate(SevenDayEnding=paste0(str_sub(SevenDayEnding,1,4),
                               "-",str_sub(SevenDayEnding,5,6),
                               "-",str_sub(SevenDayEnding,7,8))) %>% 
  merge(date_season_week %>% select(SevenDayEnding=Date,Season,ISOyear)) %>% 
  relocate(c(Season,ISOyear),.before =SevenDayEnding) %>% 
  arrange(SevenDayEnding) %>% 
  mutate(SevenDayEnding=str_remove_all(SevenDayEnding,"-"))


##write csv----
message("Writing Wastewater_COVID19_Scotland file")
write_csv(ww_scotland,paste0(file_paths$Outputs$Output_folder,
                             "Wastewater_COVID19_Scotland_",od_report_date,".csv"))

#2. by Health Board-------------------------------------------------------------
ww_hb<-wastewater_covid19_HB %>%
  rename(WeekBeginning=WeekStartDate,
         WeekEnding=WeekEndDate,
         HBcode=HB) %>% 
  merge(HB %>% filter(HBName !="Scotland"),by=c("HBcode")) %>% 
  relocate(c(HBName,HBcode),.after = WeekEnding) %>%
  arrange(WeekBeginning)

##write csv----
message("Writing Wastewater_COVID19_HB file")
write_csv(ww_hb,paste0(file_paths$Outputs$Output_folder,
                       "Wastewater_COVID19_by_HB_",od_report_date,".csv"))

#3. by Local Authority----------------------------------------------------------
ww_la<-wastewater_covid19_LA %>%
  rename(WeekBeginning=WeekStartDate,
         WeekEnding=WeekEndDate,
         LAcode=LA) %>% 
  merge(CA_lookup%>% rename(LAName =local_authority,
                            LAcode=ca2019),by=c("LAcode")) %>% 
  relocate(c(LAName,LAcode),.after = WeekEnding) %>%
  arrange(WeekBeginning)

##write csv----
message("Writing Wastewater_COVID19_LA file")
write_csv(ww_la,paste0(file_paths$Outputs$Output_folder,
                       "Wastewater_COVID19_by_LA_",od_report_date,".csv"))

#4. by Wate water treatment unit------------------------------------------------
ww_wwtw<-wastewater_covid19_wwtw %>%
  rename(WeekBeginning=WeekStartDate,
         WeekEnding=WeekEndDate) %>% 
  arrange(WeekBeginning)

##write csv----
message("Writing Wastewater_COVID19_wwtw file")
write_csv(ww_wwtw,paste0(file_paths$Outputs$Output_folder,
                         "Wastewater_COVID19_by_wwtw_",od_report_date,".csv"))


###################### END #####################################-

rm(list = ls(pattern = "^wastewater"))
rm(list = ls(pattern = "^ww"))
gc()

