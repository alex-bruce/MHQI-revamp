# Dashboard data transfer for RAPID hospital occupancy data
# Sourced from ../dashboard_data_transfer.R

i_rapid_occupancy <- read_csv_with_options(match_base_filename(glue(input_data, "occupancy_rapid.csv")))

write.csv(i_rapid_occupancy, glue(output_folder, "occupancy_rapid.csv"), row.names = FALSE)

i_hb_occupancy <- read_csv(glue(output_folder, "Occupancy_Weekly_Hospital_HB.csv"))


### Make data set with hospital occupancy using manual submissions up to W39 2025
### Then figures based on RAPID from that point onwards
i_occupancy_manual <- i_hb_occupancy %>%
  filter(HealthBoardQF== "d") %>% #filters for Scotland values
  filter(WeekEnding <= as_date("2025/09/28")) %>% 
  arrange(desc(WeekEnding_od)) %>% 
  select(WeekEnding, HospitalOccupancy, SevenDayAverage)

i_occupancy_covid_rapid <- i_rapid_occupancy %>%
  filter(pathogen == "COVID-19") %>% 
  filter(week_ending > as_date("2025/09/28")) %>% 
  arrange(desc(week_ending)) %>% 
  select(WeekEnding = week_ending,
         HospitalOccupancy = bed_occupancy,
         SevenDayAverage= sevenday_ave_inpatients) %>%
  mutate(WeekEnding = as.Date(WeekEnding))

i_occupancy_covid <- bind_rows(i_occupancy_covid_rapid, i_occupancy_manual)

write.csv(i_occupancy_covid, glue(output_folder, "occupancy_covid.csv"), row.names = FALSE)


rm(i_rapid_occupancy, i_hb_occupancy, i_occupancy_manual, i_occupancy_covid_rapid, i_occupancy_covid)


