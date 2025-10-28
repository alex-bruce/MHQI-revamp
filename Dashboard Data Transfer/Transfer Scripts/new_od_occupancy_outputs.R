#This script creates "OCCUPANCY" open data outputs
#10/10/2025
#Nipuni Rajapaksha

#1.Scotland level- bed occupancy (RAPID)----------------------------------------

occupancy_rapid_rsv_flu_covid19_scotland<-
  read_csv(paste0(file_paths$Data$Dashboard_output_folder,
                  "occupancy_rapid.csv")) %>% 
  mutate(Pathogen = recode(pathogen, "Influenza"="Influenza (All)"),
         Country="S92000003",
         Season=paste0(
           str_sub(Season,1,4),"/",
           str_sub(Season,8,9)
         )) %>% 
  select(Season,
         ISOyear=Year, 
         ISOweek=ISOWeek,
         WeekBeginning=week_start,
         WeekEnding=Date,
         Country,
         Pathogen,
         BedOccupancy=bed_occupancy,
         SevenDayAverageInpatients=sevenday_ave_inpatients
  ) 

###write csv----
message("Writing Occupancy_RAPID_weekly_COVID19_FLU_RSV_scotland file")
write_csv(occupancy_rapid_rsv_flu_covid19_scotland %>% 
            mutate(WeekBeginning=str_remove_all(WeekBeginning,"-"),
                   WeekEnding=str_remove_all(WeekEnding,"-")),
          paste0(file_paths$Outputs$Output_folder,
                 "Occupancy_RAPID_weekly_COVID19_FLU_RSV_scotland_",
                 od_report_date,".csv")) 

#2.HB level- weekly----------------------------------------

##2a.Weekly-RAPID ----
occupancy_covid19_rsv_flu_hb_a<-
  read_csv(paste0(
    file_paths$Data$Dashboard_input_folder,
    "occupancy_rapid_HB_",od_date,".csv"))

occupancy_covid19_rsv_flu_hb<-
  occupancy_covid19_rsv_flu_hb_a%>% 
  mutate(health_board=recode(health_board,
                             "NATIONAL FACILITY"="Golden Jubilee National Hospital"),
         health_board=str_replace(health_board,"&","and"),
         health_board=str_to_title(health_board),
         health_board=str_replace(health_board,"And","and"),
         health_board=str_replace(health_board,"Nhs","NHS"),
         Season=paste0(str_sub(Season,1,4),"/",str_sub(Season,8,9)),
         pathogen=recode(pathogen,
                         "Influenza"="Influenza (All)")) %>% 
  select(Season,ISOyear=Year,ISOweek=ISOWeek,
         WeekEnding=week_ending,
         WeekBeginning=week_start,
         HBName=health_board,
         Pathogen=pathogen,
         BedOccupancy=bed_occupancy,
         SevenDayAverageInpatients=sevenday_ave_inpatients) %>% 
  merge(HB_admissions,by=c("HBName")) %>% 
  relocate(HBcode,.after = HBName) %>% 
  #Scotland values for QF
  rbind(occupancy_rapid_rsv_flu_covid19_scotland %>% 
          rename(HBcode=Country) %>% 
          mutate(HBName="Scotland")) %>% 
  mutate(HBQF=if_else(HBName=="Scotland","d","")) %>% 
  select(Season, ISOyear,ISOweek,WeekBeginning,WeekEnding,Pathogen,
         HBName,HBcode,HBQF,BedOccupancy,SevenDayAverageInpatients) %>%
  arrange(WeekBeginning,Pathogen,HBQF) %>% 
  mutate(WeekBeginning=str_remove_all(WeekBeginning,"-"),
         WeekEnding=str_remove_all(WeekEnding,"-"))


###write csv----
message("Writing Occupancy_weekly_covid19_rsv_flu_by_HB file")
write_csv(occupancy_covid19_rsv_flu_hb,
          paste0(file_paths$Outputs$Output_folder,
                 "Occupancy_RAPID_weekly_COVID19_FLU_RSV_by_HB_",
                 od_report_date ,".csv")) 


##2b.Weekly(COVID19 only) by the boards (ARCHIVE) ----
occupancy_covid19_hb_a<-
  read_csv(paste0(
    file_paths$Data$Dashboard_output_folder,
    "Occupancy_Weekly_Hospital_HB.csv"))

occupancy_covid19_hb<-
  occupancy_covid19_hb_a%>% 
  rename(HBName=HealthBoardName) %>% 
  merge(df_template_hb_admissions %>% 
          filter(Pathogen=="COVID-19" &
                   WeekEnding>=min(occupancy_covid19_hb_a$WeekEnding)),
        by=c("WeekEnding","HBName")) %>% 
  mutate(HBQF=if_else(is.na(HealthBoardQF),"",HealthBoardQF),
         HospitalOccupancyQF=if_else(is.na(HospitalOccupancyQF),"",HospitalOccupancyQF),
         SevenDayAverageQF=if_else(is.na(SevenDayAverageQF),"",SevenDayAverageQF)) %>% 
  select(Season, ISOyear,ISOweek,WeekBeginning,WeekEnding,Pathogen,
         HBName,HBcode,HBQF,HospitalOccupancy,HospitalOccupancyQF,
         SevenDayAverage,SevenDayAverageQF) %>% 
  mutate(WeekBeginning=str_remove_all(WeekBeginning,"-"),
         WeekEnding=str_remove_all(WeekEnding,"-"))


###write csv----
message("Writing Occupancy_weekly_COVID19_by_HB file")
write_csv(occupancy_covid19_hb,
          paste0(file_paths$Outputs$Output_folder,
                 "Occupancy_weekly_COVID19_by_HB_",
                 od_report_date ,"(ARCHIVE).csv")) 

# ##2b.RAPID daily----
# 
# occupancy_live_covid19_hb_a<-
#   read_csv(paste0(
#     file_paths$Data$Dashboard_output_folder,
#     "Occupancy_Hospital_HB.csv"))
# 
# occupancy_live_covid19_hb<-
#   occupancy_live_covid19_hb_a%>% 
#   rename(HBName=HealthBoard) %>% 
#   merge(date_season_week %>%
#           select(-c(WeekBeginning,WeekEnding)) %>% 
#           filter(Date>=min(occupancy_live_covid19_hb_a$Date)),
#         by=c("Date")) %>% 
#   mutate(HBQF=if_else(is.na(HealthBoardQF),"",HealthBoardQF),
#          HospitalOccupancyQF=if_else(is.na(HospitalOccupancyQF),"",HospitalOccupancyQF),
#          SevenDayAverageQF=if_else(is.na(SevenDayAverageQF),"",SevenDayAverageQF)) %>% 
#   merge(df_template_hb_admissions %>% 
#           filter(Pathogen=="COVID-19"),
#         by=c("Season","ISOyear","ISOweek","HBName")) %>% 
#   select(Season, ISOyear,ISOweek,Date,Pathogen,
#          HBName,HBcode,HBQF,HospitalOccupancy,HospitalOccupancyQF,
#          SevenDayAverage,SevenDayAverageQF) %>% 
#   arrange(Date) %>% 
#   mutate(Date=str_remove_all(Date,"-"))
#   
# 
# ###write csv----
# write_csv(occupancy_live_covid19_hb,
#           paste0(file_paths$Outputs$Output_folder,
#                  "Occupancy_RAPID_daily_COVID19_by_HB_",
#                  od_report_date,".csv")) 


######## END #########-

rm(occupancy_covid19_hb,occupancy_covid19_hb_a,
   #occupancy_live_covid19_hb,
   #occupancy_live_covid19_hb_a,
   occupancy_rapid_rsv_flu_covid19_scotland)



