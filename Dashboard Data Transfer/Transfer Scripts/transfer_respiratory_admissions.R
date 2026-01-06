# Dashboard data transfer for all pathogen hospital admissions data
# Sourced from ../dashboard_data_transfer.R



#create weekord
date_reference_ord <-readRDS("/conf/C19_Test_and_Protect/Analyst Space/Calum (Analyst Space)/flu_seasons.Rds") %>%
  distinct(flu_season, year, ISOweek) %>%
  mutate(alltime_weekord = row_number()) %>%
  group_by(flu_season) %>%
  mutate(season_weekord = row_number()) %>%
  ungroup() %>%
  rename(Weekord = season_weekord) %>%
  select(-alltime_weekord)

#create lookup to add season and weekord to data
date_reference <- readRDS("/conf/C19_Test_and_Protect/Analyst Space/Calum (Analyst Space)/flu_seasons.Rds") %>%
  distinct(date, year, ISOweek, flu_season) %>%
  left_join(date_reference_ord)

i_respiratory_admissions <- read_csv_with_options(match_base_filename(glue(input_data, "All_path_admissions_by_week_ending.csv")))


g_respiratory_admissions <- i_respiratory_admissions %>%
  dplyr::rename(Date = date) %>%
  mutate(date = as.Date(Date)) %>%
  left_join(date_reference, by = c("date" = "date", "iso_year" = "year", "iso_week" = "ISOweek")) %>%
  mutate(Date = as_date(ceiling_date(as_date(Date), "week",
                                     change_on_boundary = F))) %>%
  dplyr::rename(Year = iso_year,
                ISOWeek = iso_week,
                Season = flu_season)



write.csv(g_respiratory_admissions, glue(output_folder, "all_pathogen_admissions.csv"), row.names = FALSE)

rm(i_respiratory_admissions, g_respiratory_admissions)


## Admissions by age
i_respiratory_age_admissions <- read_csv_with_options(match_base_filename(glue(input_data, "age_rate_data_all_path.csv")))

write.csv(i_respiratory_age_admissions, glue(output_folder, "age_rate_data_all_path.csv"))

rm(i_respiratory_age_admissions)

## Admissions by SIMD
i_respiratory_simd_admissions <- read_csv_with_options(match_base_filename(glue(input_data,
                                                                                  " - simd summary - all path.csv"))) %>% 
  select(WeekEnding=date,everything())

covid19_flu_rsv_simd_admissions<-
  i_respiratory_simd_admissions %>% 
  pivot_longer(cols = c(starts_with("Total"),ends_with("rate"))) %>% 
  mutate(parameter=str_extract_all(name, "Total"),
         parameter=as.character(parameter),
         parameter=if_else(parameter!="Total","Rate",parameter)) %>% 
  rename(Pathogen=name, Population=pop) %>% 
  mutate(Pathogen=str_remove_all(Pathogen,"Total_"),
         Pathogen=str_remove_all(Pathogen,"_rate")) %>% 
  mutate(Pathogen=recode(Pathogen,
                         "cov"="COVID-19",
                         "flu"="Influenza (All)",
                         "rsv"="RSV")) %>% 
  pivot_wider(names_from = parameter,values_from = value) %>% 
  rename(NumberOfAdmissions=Total, RateOfAdmissions=Rate) %>% 
  mutate(RateOfAdmissions=round(RateOfAdmissions,digits = 1)) %>% 
  select(WeekEnding, Pathogen,SIMD=simd,NumberOfAdmissions,RateOfAdmissions,
         Population) %>% 
  arrange(WeekEnding,Pathogen,SIMD) %>% 
  mutate(ProvisionalFlag = case_when(
    WeekEnding > (report_date-10) ~ 1,
    TRUE ~ 0)) %>% 
  mutate(WeekEnding=str_remove_all(WeekEnding,"-"))


write.csv(covid19_flu_rsv_simd_admissions, glue(output_folder, "admissions_simd_Cov_flu_RSV.csv"), row.names = FALSE)

rm(covid19_flu_rsv_simd_admissions)



i_respiratory_hb_admissions <- read_csv_with_options(match_base_filename(glue(input_data, " - hb summary - all path.csv")))

g_respiratory_hb_admissions <- i_respiratory_hb_admissions[, -1] 

g_respiratory_hb_admissions <- g_respiratory_hb_admissions %>% 
  mutate(week_ending = as_date(week_ending)) %>% 
  mutate(health_board_of_treatment = recode(health_board_of_treatment,
                              "NHS AYRSHIRE AND ARRAN" = "NHS Ayrshire and Arran",
                              "NHS BORDERS" = "NHS Borders",
                              "NHS DUMFRIES AND GALLOWAY" = "NHS Dumfries and Galloway",
                              "NHS FIFE" = "NHS Fife",
                              "NHS FORTH VALLEY" = "NHS Forth Valley",
                              "NHS GRAMPIAN" = "NHS Grampian",
                              "NHS GREATER GLASGOW AND CLYDE" = "NHS Greater Glasgow and Clyde",
                              "NHS HIGHLAND" = "NHS Highland",
                              "NHS LANARKSHIRE" = "NHS Lanarkshire",
                              "NHS LOTHIAN" = "NHS Lothian",
                              "NHS ORKNEY" = "NHS Orkney",
                              "NHS SHETLAND" = "NHS Shetland",
                              "NHS TAYSIDE" = "NHS Tayside",
                              "NHS WESTERN ISLES" = "NHS Western Isles",
                              "NHS Scotland" = "Scotland",
                              "NATIONAL FACILITY" = "Golden Jubilee National Hospital")) %>%
  mutate(Season = gsub("-", "/", Season))


write.csv(g_respiratory_hb_admissions, glue(output_folder, "admissions_hb_all_path.csv"), row.names = FALSE)

three_sunday_dates <- data.frame(week_ending=seq(as.Date("2018-10-07"), as.Date(od_date-1), "week")) %>%
  # mutate(WeekEnding= format(strptime(WeekEnding, format = "%Y-%m-%d")) ) %>%
  slice_tail(n = 3)

HealthBoardName= data.frame(health_board_of_treatment=c("NHS Ayrshire and Arran", 
                                                        "NHS Borders",
                                                        "NHS Dumfries and Galloway","NHS Fife",
                                                        "NHS Forth Valley","NHS Grampian",
                                                        "NHS Greater Glasgow and Clyde",
                                                        "NHS Highland",
                                                        "NHS Lanarkshire",
                                                        "NHS Lothian",
                                                        "NHS Orkney",
                                                        "NHS Shetland",
                                                        "NHS Tayside",
                                                        "NHS Western Isles",
                                                        "Golden Jubilee National Hospital"))

pathogens = data.frame(admission_type = c("cov", "flu", "rsv",  "rhino", "coron", "para", "adeno", "hmpv", "mpn"))

hb_last_three_weeks <- expand.grid(health_board_of_treatment=unique(HealthBoardName$health_board_of_treatment),
                                   week_ending=unique(three_sunday_dates$week_ending),
                                   admission_type = unique(pathogens$admission_type),
                                   KEEP.OUT.ATTRS = FALSE,
                                   stringsAsFactors = FALSE)

g_respiratory_hb_admissions_3wks <- hb_last_three_weeks %>%
  left_join(g_respiratory_hb_admissions, by=c("health_board_of_treatment","week_ending", "admission_type")) %>% 
  select(-c("NRS_population_estimate", "n")) %>% 
  replace_na(list(n = 0, rate = 0))

write.csv(g_respiratory_hb_admissions_3wks, glue(output_folder, "admissions_hb_all_path_3wks.csv"), row.names = FALSE)

rm(i_respiratory_hb_admissions, g_respiratory_hb_admissions, g_respiratory_hb_admissions_3wks)

#### RSV healthboard admissions
#i_rsv_hb_admissions <- read_csv_with_options(match_base_filename(glue(input_data, "admissions_rsv_hb.csv")))



# g_rsv_adm_hb<- i_rsv_hb_admissions %>%
#   mutate(WeekBeginning = as.Date(week_start)) %>%
#   mutate(WeekEnding = as.Date(week_end)) %>%
#   arrange(WeekEnding , NHS_BOARD_NAME_TREATMENT) %>%
#   mutate(HealthBoardOfTreatment = recode(NHS_BOARD_NAME_TREATMENT,
#                                          "NHS AYRSHIRE & ARRAN" = "NHS Ayrshire and Arran",
#                                          "NHS BORDERS" = "NHS Borders",
#                                          "NHS DUMFRIES & GALLOWAY" = "NHS Dumfries and Galloway",
#                                          "NHS FIFE" = "NHS Fife",
#                                          "NHS FORTH VALLEY" = "NHS Forth Valley",
#                                          "NHS GRAMPIAN" = "NHS Grampian",
#                                          "NHS GREATER GLASGOW & CLYDE" = "NHS Greater Glasgow and Clyde",
#                                          "NHS HIGHLAND" = "NHS Highland",
#                                          "NHS LANARKSHIRE" = "NHS Lanarkshire",
#                                          "NHS LOTHIAN" = "NHS Lothian",
#                                          "NHS ORKNEY" = "NHS Orkney",
#                                          "NHS SHETLAND" = "NHS Shetland",
#                                          "NHS TAYSIDE" = "NHS Tayside",
#                                          "NATIONAL FACILITY" = "Golden Jubilee National Hospital",
#                                          "NHS WESTERN ISLES" = "NHS Western Isles" )) %>%
#   select(Flu_Season,week,WeekBeginning,WeekEnding, HealthBoardOfTreatment,
#          TotalInfections=Freq_RSV_positives)
# 
# g_rsv_adm_scot <- g_rsv_adm_hb  %>%
#   group_by(Flu_Season,week,WeekBeginning,WeekEnding) %>%
#   summarise(TotalInfections = sum(TotalInfections)) %>%
#   mutate(HealthBoardOfTreatment = "Scotland")
# 
# g_rsv_adm_hb %<>%
#   mutate(summer_2025_flag = case_when(WeekEnding >= summer_2025 ~"flag",
#                                       TRUE~"")) %>% 
#   filter(summer_2025_flag !="flag")  %>%
#   select(-summer_2025_flag) %>% 
#   bind_rows(g_rsv_adm_scot) %>%
#   arrange(WeekEnding)
# 
# write.csv(g_rsv_adm_hb, glue(output_folder, "RSV_Admissions_HB.csv"), row.names = FALSE)
# 

#  create 3 week framework to hang  admissions ######
# 
# three_sunday_dates <- data.frame(WeekEnding=seq(as.Date("2018-10-07"), as.Date(od_date-1), "week")) %>%
#  # mutate(WeekEnding= format(strptime(WeekEnding, format = "%Y-%m-%d")) ) %>%
#   slice_tail(n = 3)
# 
# HealthBoardName= data.frame(HealthBoardOfTreatment=c("NHS Ayrshire and Arran",  "NHS Borders",
#                                               "NHS Dumfries and Galloway","NHS Fife",
#                                               "NHS Forth Valley","NHS Grampian",
#                                               "NHS Greater Glasgow and Clyde",
#                                               "NHS Highland","NHS Lanarkshire",
#                                               "NHS Lothian","NHS Orkney","NHS Shetland","NHS Tayside",
#                                               "NHS Western Isles","Golden Jubilee National Hospital",
#                                               "Scotland" ))
# 
# 
# hb_last_three_weeks <- expand.grid(HealthBoardOfTreatment=unique(HealthBoardName$HealthBoardOfTreatment),
#                                    WeekEnding=unique(three_sunday_dates$WeekEnding),
#                                    KEEP.OUT.ATTRS = FALSE,
#                                    stringsAsFactors = FALSE)
# 
# g_rsv_adm_hb_3weeks<-g_rsv_adm_hb %>%
#   filter(WeekEnding>=od_sunday_minus_14)
# 
# 
# g_rsv_adm_hb_3weeks_full<-hb_last_three_weeks %>%
#   left_join(g_rsv_adm_hb_3weeks, by=c("HealthBoardOfTreatment","WeekEnding")) %>%
#   select(WeekEnding, HealthBoardOfTreatment, TotalInfections) %>%
#   mutate(TotalInfections=if_else(is.na(TotalInfections),0,TotalInfections))
#   #arrange(HealthBoardOfTreatment)


#write.csv(g_rsv_adm_hb_3weeks_full, glue(output_folder, "RSV_Admissions_HB_3wks.csv"), row.names = FALSE)



# rm(i_rsv_hb_admissions, g_rsv_adm_scot, g_rsv_adm_hb , g_rsv_adm_hb_3weeks,
#    g_rsv_adm_hb_3weeks_full, hb_last_three_weeks, HealthBoardName,
#    three_sunday_dates)