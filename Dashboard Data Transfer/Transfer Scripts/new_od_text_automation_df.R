#Text automation

#Hard coded---------------------------------------------------------------------
##Pathogens to keep----
non_covid_pathogens<-c("Adenovirus","HMPV",
                       "Influenza (All)","Influenza A",
                       "Influenza B","Mycoplasma pneumoniae",
                       "Parainfluenza (Any Type)","RSV","Rhinovirus",
                       "Seasonal coronavirus")


#Seasons to include---- (add remove seasons as required)
resp_seasons_to_include <- c("2016/17", "2017/18", "2018/19", "2019/20", "2020/21",
                             "2021/22", "2022/23", "2023/24", "2024/25", "2025/26")



#Organism start dates-----------------------------------------------------------

##TESTS----
#COVID-19 (from wk40 2022), Influenza (from wk40 2020) and RSV (from wk40 2024)

FLU_start_date<-
  date_reference %>% 
  filter(ISOyear=="2020" & ISOweek=="40") %>% 
  select(Season=flu_season_V2,ISOyear,ISOweek,
         WeekBeginning=ISOweek_beginning,
         WeekEnding=ISOweek_ending) %>% 
  distinct()


RSV_start_date<-
  date_reference %>% 
  filter(ISOyear=="2024" & ISOweek=="40") %>% 
  select(Season=flu_season_V2,ISOyear,ISOweek,
         WeekBeginning=ISOweek_beginning,
         WeekEnding=ISOweek_ending) %>% 
  distinct()


COVID_19_start_date<-
  date_reference %>% 
  filter(ISOyear=="2022" & ISOweek=="40") %>% 
  select(Season=flu_season_V2,ISOyear,ISOweek,
         WeekBeginning=ISOweek_beginning,
         WeekEnding=ISOweek_ending) %>% 
  distinct()


#Dates for saving and reading files---------------------------------------------

od_date <- floor_date(today(), "week", 1) + 1              #current week Tuesday

od_admsn_date<-format(as.Date(od_date)-days(1), "%Y%m%d")

report_date <- floor_date(today(), "week", 1) + 2        #current week Wednesday

od_report_date <- format(report_date, "%Y%m%d")

od_archive_date <-format(report_date-7)

od_sunday<- floor_date(today(), "week", 1) -1 #last sunday (week ending day of the report ISOweek)

od_isoweek<-date_reference %>% 
  filter(date==od_sunday) %>% 
  pull(ISOweek)

od_isoyear<-date_reference %>% 
  filter(date==od_sunday) %>% 
  pull(ISOyear)

od_sunday_minus_7 <- floor_date(today(), "week", 1) -8
