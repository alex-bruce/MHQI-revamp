# Dashboard data transfer for RAPID hospital occupancy data
# Sourced from ../dashboard_data_transfer.R

i_rapid_occupancy <- read_csv_with_options(match_base_filename(glue(input_data, "occupancy_rapid.csv")))

i_rapid_occupancy <- i_rapid_occupancy %>%
  mutate(Season = gsub("-", "/", Season))

write.csv(i_rapid_occupancy, glue(output_folder, "occupancy_rapid.csv"), row.names = FALSE)

# i_hb_occupancy <- read_csv(glue(output_folder, "Occupancy_Weekly_Hospital_HB.csv"))


### Make data set with hospital occupancy using manual submissions up to W39 2025
### Then figures based on RAPID from that point onwards
# i_occupancy_manual <- i_hb_occupancy %>%
#   filter(HealthBoardQF== "d") %>% #filters for Scotland values
#   filter(WeekEnding <= as_date("2025/09/28")) %>% 
#   arrange(desc(WeekEnding_od)) %>% 
#   select(WeekEnding, HospitalOccupancy, SevenDayAverage)
# 
# i_occupancy_covid_rapid <- i_rapid_occupancy %>%
#   filter(pathogen == "COVID-19") %>% 
#   filter(week_ending > as_date("2025/09/28")) %>% 
#   arrange(desc(week_ending)) %>% 
#   select(WeekEnding = week_ending,
#          HospitalOccupancy = bed_occupancy,
#          SevenDayAverage= sevenday_ave_inpatients) %>%
#   mutate(WeekEnding = as.Date(WeekEnding))
# 
# i_occupancy_covid <- bind_rows(i_occupancy_covid_rapid, i_occupancy_manual)
# 
# write.csv(i_occupancy_covid, glue(output_folder, "occupancy_covid.csv"), row.names = FALSE)

# Health Board level occupancy
i_rapid_occupancy_hb <- read_csv_with_options(match_base_filename(glue(input_data, "occupancy_rapid_hb.csv"))) %>% 
  mutate( health_board = recode(health_board,
         "NHS AYRSHIRE & ARRAN" = "NHS Ayrshire and Arran", 
         "NHS BORDERS"  = "NHS Borders",
         "NHS DUMFRIES & GALLOWAY" = "NHS Dumfries and Galloway",       
         "NHS FIFE" = "NHS Fife",
         "NHS FORTH VALLEY" = "NHS Forth Valley",
         "NHS GRAMPIAN" = "NHS Grampian",
         "NHS GREATER GLASGOW & CLYDE" = "NHS Greater Glasgow and Clyde",
         "NHS HIGHLAND" = "NHS Highland",
         "NHS LANARKSHIRE" = "NHS Lanarkshire",
         "NHS LOTHIAN" = "NHS Lothian",
         "NHS ORKNEY" = "NHS Orkney",
         "NHS SHETLAND" = "NHS Shetland",
         "NHS TAYSIDE" = "NHS Tayside",
         "NHS WESTERN ISLES" = "NHS Western Isles",
         "NATIONAL FACILITY" = "Golden Jubilee National Hospital",
         "NHS Scotland" = "Scotland")
         )

i_rapid_occupancy_hb <- i_rapid_occupancy_hb %>%
  mutate(Season = gsub("-", "/", Season))

write.csv(i_rapid_occupancy_hb, glue(output_folder, "occupancy_rapid_hb.csv"), row.names = FALSE)

### Make data set with hospital occupancy using manual submissions up to W39 2025
### Then figures based on RAPID from that point onwards
# i_occupancy_manual_hb <- i_hb_occupancy %>%
#   filter(HealthBoardName != "Scotland") %>% #filters out Scotland values
#   filter(WeekEnding <= as_date("2025/09/28")) %>% 
#   arrange(desc(WeekEnding_od)) %>% 
#   select(WeekEnding, health_board = HealthBoardName, HospitalOccupancy, SevenDayAverage)


# i_occupancy_covid_rapid_hb <- i_rapid_occupancy_hb %>%
#   filter(pathogen == "COVID-19") %>% 
#   filter(week_ending > as_date("2025/09/28")) %>% 
#   arrange(desc(week_ending)) %>% 
#   select(WeekEnding = week_ending,
#          health_board,
#          HospitalOccupancy = bed_occupancy,
#          SevenDayAverage= sevenday_ave_inpatients) %>%
#   mutate(WeekEnding = as.Date(WeekEnding)) 
# 
# i_occupancy_covid_hb <- bind_rows(i_occupancy_manual_hb, i_occupancy_covid_rapid_hb)
# 
# write.csv(i_occupancy_covid_hb, glue(output_folder, "occupancy_covid_hb.csv"), row.names = FALSE)

rm(i_rapid_occupancy, i_rapid_occupancy_hb)#i_hb_occupancy, i_occupancy_manual, i_occupancy_covid_rapid, i_occupancy_covid,
   #, i_occupancy_manual_hb, i_occupancy_covid_rapid_hb, i_occupancy_covid_hb)


