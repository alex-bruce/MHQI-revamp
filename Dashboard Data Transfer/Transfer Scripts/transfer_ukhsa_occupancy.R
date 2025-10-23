#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# RStudio Workbench is strictly for use by Public Health Scotland staff and     
# authorised users only, and is governed by an <Acceptable Usage Policy>.
#
# This is a shared resource and is hosted on a pay-as-you-go cloud computing
# platform.  Your usage will incur direct financial cost to Public Health
# Scotland.  As such, please ensure
#
#   1. that this session is appropriately sized with the minimum number of CPUs
#      and memory required for the size and scale of your analysis;
#   2. the code you write in this script is optimal and only writes out the
#      data required, nothing more.
#   3. you close this session when not in use; idle sessions still cost PHS
#      money!
#
# For further guidance, please see <insert link>.
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


i_rapid_occupancy_c19_rapid <- read_csv_with_options(match_base_filename(glue(input_data, "occupancy_c19_rapid.csv"))) %>%
  select(-SevenDayAverage) %>%
  mutate(HealthBoard = "Scotland")

i_occupancy_boards <- read_all_excel_sheets(glue("/PHI_conf/Respiratory_Surveillance_Viral/Dashboard/Data/Occupancy/Hospital-ICU Daily Numbers 29092025.xlsx"))

g_occupancy_hospital_healthboard <- i_occupancy_boards$Data %>%
  clean_names() %>%
  rename(HospitalOccupancy = total_number_of_confirmed_c19_inpatients_in_hospital_at_8am_yesterday_new_measure_number_of_confirmed_c19_inpatients_in_hospital_10_days_at_8am_as_of_08_05_2023,
         #ICUOccupancy28OrLess = total_number_of_confirmed_c19_inpatients_in_icu_28_days_or_less_at_8am_yesterday_new_measure,
         #ICUOccupancy28OrMore = total_number_of_confirmed_c19_inpatients_in_icu_greater_than_28_days_at_8am_yesterday_measure_as_of_20_01_21,
         Date = date,
         HealthBoard = health_board) %>%
  filter(Date >= "2020-09-08" & Date <= "2025-09-29") %>% # filter to sunday date
  mutate(HealthBoardQF = "",
         HospitalOccupancy = as.numeric(HospitalOccupancy),
         #ICUOccupancy28OrLess = as.numeric(ICUOccupancy28OrLess),
         #ICUOccupancy28OrMore = as.numeric(ICUOccupancy28OrMore),
         #Date = as.Date(as.POSIXct(Date-1, 'GMT')),
         Date = format(as.Date(Date-1), "%Y%m%d"), #-1 as number is for "8am yesterday"
         HealthBoard = str_replace(HealthBoard, "&", "and")) %>% 
  select(Date, HospitalOccupancy, HealthBoard, HealthBoardQF) 

g_occupancy_hospital_scotland <- g_occupancy_hospital_healthboard %>%
  group_by(Date) %>%
  summarise(HospitalOccupancy = sum(HospitalOccupancy,na.rm=T)) %>%
  #ICUOccupancy28OrLess = sum(ICUOccupancy28OrLess,na.rm=T),
  #ICUOccupancy28OrMore = sum(ICUOccupancy28OrMore,na.rm=T)) %>%
  ungroup() %>%
  mutate(HealthBoard = "Scotland",
         HealthBoardQF = "d")

g_occupancy_hospital <- g_occupancy_hospital_scotland %>% 
  # filter(Date <= 20250510) %>%  # filter for summer month reporting. Temporary
  rbind(i_rapid_occupancy_c19_rapid) %>%
  arrange(Date) %>%
  # group_by(HealthBoard) %>%
  mutate(SevenDayAverage = round_half_up(zoo::rollmean(HospitalOccupancy, k = 7, fill = NA, align="right"),0),
         SevenDayAverageQF = ifelse(is.na(SevenDayAverage), ":", ""),
         SevenDayAverageQF = ifelse(Date <= 20200912 , "z", SevenDayAverageQF),
         HospitalOccupancyQF = ifelse(is.na(HospitalOccupancy), ":", "")) %>%
  #ungroup() %>%
  #arrange(Date) %>%
  select(Date, HealthBoard, HealthBoardQF, HospitalOccupancy, HospitalOccupancyQF, SevenDayAverage, SevenDayAverageQF) %>%
  # mutate(HealthBoard = ifelse(substr(HealthBoard,1,1)=="Z", "Other", HealthBoard),
  #        HealthBoard = unlist(hblookup[HealthBoard]),
  #        HealthBoardQF = ifelse(HealthBoard == "", ":", HealthBoardQF)) %>%
  # filter(HealthBoard == "S92000003") #for disclosure reasons temporarily filtering for Scotland only
  
  
# save to UKHSA adm folder
write_csv(g_occupancy_hospital, glue(ukhsa_adm, "Occupancy_Hospital.csv"))
