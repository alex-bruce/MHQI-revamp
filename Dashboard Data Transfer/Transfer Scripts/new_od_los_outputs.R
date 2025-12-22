## Create los open data file

## avg los by age, by week ending 

avg_los_weekly <-   read_csv_with_options(match_base_filename(glue(input_data, "avg_los_week.csv")))


avg_los_weekly_od <- avg_los_weekly %>% 
  mutate(week_ending = ymd(week_ending)) %>% 
  mutate(ISOyear = isoyear(week_ending),
         ISOweek = isoweek(week_ending),
         Season = paste(substr(Season,1, 4), "/", substr(Season, 8, 9), sep=""),
         avg_los = round(avg_los, 2)) %>% 
  rename(WeekEnding = week_ending,
         WeekBeginning = week_start,
         AgeGroup = los_age_band,
         AverageLengthOfStay = avg_los,
         TotalLengthOfStay = total_los) %>% 
  mutate(Country="S92000003") %>% 
  mutate(WeekBeginning=str_remove_all(WeekBeginning,"-"),
         WeekEnding=str_remove_all(WeekEnding,"-")) %>% 
  select(Season, ISOyear, ISOweek, WeekBeginning, WeekEnding, Country, Pathogen, AgeGroup, NumberOfAdmissions, TotalLengthOfStay, AverageLengthOfStay)

head(avg_los_weekly_od)


write_csv(avg_los_weekly_od,
          paste0(file_paths$Outputs$Output_folder,
                 "ALOS_COVID19_FLU_RSV_by_Agegroup_",
                 od_report_date,".csv"))
