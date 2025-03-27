##### Length of Stay data transfer


# i_los <- read_csv_with_options(glue(input_data, "{format(report_date -2,'%Y-%m-%d')}_LOS Table Dashboard.csv"))
# i_los_median <- read_csv_with_options(glue(input_data, "los_mean_4weeks.csv"))

i_los_weekly <- read_csv_with_options(glue(input_data, "los_weekly.csv"))
i_los_season <- read_csv_with_options(glue(input_data, "los_season.csv"))
i_los_median <- read_csv_with_options(glue(input_data, "los_mean_4weeks_TEST.csv"))

#  los by week ending 
g_los_weekly <- i_los_weekly %>%
  mutate(PercentageOfAdmissions =  round_half_up(ProportionOfAdmissions*100, 1),
         AdmissionWeekEnding = format(as.Date(WeekEnding), "%Y%m%d"),
         AdmissionWeekStarting = format(as.Date(WeekStart), "%Y%m%d"),
         AgeGroupQF = ifelse(age_band_custom == "All Ages", "d", ""),
         AgeGroup = factor(age_band_custom,
                           levels = c("0-17", "18-64",  "65-74", "75+", "All Ages")),
         LengthOfStay = factor(los,
                               levels = c("1 day or less", "2-3 days", "4-5 days", "6-7 days", "8+ days"))) %>% 
  select(Season, AdmissionWeekEnding, AdmissionWeekStarting, Week, Weekord,
         Pathogen, admission_type, AgeGroup,  AgeGroupQF,
         LengthOfStay,NumberOfAdmissions, ProportionOfAdmissions, PercentageOfAdmissions) %>%
  arrange(Pathogen, AdmissionWeekEnding, AgeGroup ,LengthOfStay)

write.csv(g_los_weekly, glue(output_folder, "Length_of_Stay_Weekly.csv"), row.names=FALSE)

# los by season # 
g_los_season <- i_los_season %>% 
  rename(ProportionOfAdmissions=ProportionOfAdmission) %>% 
  mutate(PercentageOfAdmissions =  round_half_up(ProportionOfAdmissions*100, 1)) %>% 
  arrange(Pathogen, Season, AgeGroup, LengthOfStay)

write.csv(g_los_season, glue(output_folder, "Length_of_Stay_Season.csv"), row.names=FALSE)


# last 4 weekly stats
g_los_median <- i_los_median %>%
  mutate(AgeGroupQF = ifelse(AgeGroup == "All Ages", "d", ""),
         Pathogen = case_when(admission_type == "adm_flu" ~ "flu",
                              admission_type == "adm_rsv" ~ "rsv", 
                              admission_type == "cov_adm" ~ "cov"),
         CaseDefinition= case_when(Pathogen =="cov"~ "COVID-19",
                                   Pathogen == "flu"~ "Influenza",
                                   Pathogen == "rsv"~"RSV")         ) %>%
  select(Pathogen,CaseDefinition, AgeGroup, AgeGroupQF, 
         MeanLengthOfStay = mean_los, MedianLengthOfStay = median_los)

write.csv(g_los_median, glue(output_folder, "Length_of_Stay_Median.csv"), row.names=FALSE)


rm(i_los_weekly, g_los_weekly, i_los_median, g_los_median, g_los_season, i_los_season)
