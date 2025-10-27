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

#2.HB level- live and weekly----------------------------------------

##COVID 19----

##2a.Weekly---- by the boards (ARCHIVE) ----
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



