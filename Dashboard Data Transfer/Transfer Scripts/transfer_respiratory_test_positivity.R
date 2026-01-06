# Dashboard data transfer for test positivity
# Sourced from ../dashboard_data_transfer.R

##### Respiratory Test Positivity

filenames <- c("agg", "agg_agegrp")

for (filename in filenames){
  
  assign(glue("Respiratory_test_pos_{filename}"),
         read_csv_with_options(
           match_base_filename(
             glue("{input_data}/test_pos_{filename}.csv")
           )
         )
  )
}



## Add 'All ages' category to file split by age

Respiratory_test_pos_agg_agegrp <- rbind(Respiratory_test_pos_agg_agegrp,
                                          Respiratory_test_pos_agg %>% 
                                            mutate(agegrp = 'All ages') %>% 
                                            relocate(agegrp, .before = positive_count)) %>%
  mutate(agegrp = factor(agegrp, levels = c("Under 1", "1-4", "5-14", "15-44", "45-64", "65-74", "Over 75", "All ages"))) %>% 
  arrange(year, ISOweek, pathogen, agegrp)

# Output
write_csv(Respiratory_test_pos_agg, glue(output_folder, "Respiratory_Pathogens_Test_Positivity.csv"))
write_csv(Respiratory_test_pos_agg_agegrp, glue(output_folder, "Respiratory_Pathogens_Test_Positivity_by_Age.csv"))



## Create open data outputs - no longer needed as created in new_od_tests_outputs.R
# for (filename in filenames[1]){
#   
#   df1 <-  base::get(glue("Respiratory_test_pos_{filename}")) %>%
#     mutate(WeekEnding = ISOweek2date(paste0(year, "-W",
#                                             str_pad(as.character(ISOweek), width = 2,
#                                                     side = "left", pad = "0"), "-7")),
#            WeekBeginning = WeekEnding - days(6)) %>%
#     mutate(WeekEnding = gsub("-", "", as.character(WeekEnding)),
#            WeekBeginning = gsub("-", "", as.character(WeekBeginning))) %>%
#     mutate(WeekEnding = as.numeric(as.character(WeekEnding)),
#            WeekBeginning = as.numeric(as.character(WeekBeginning))) %>%
#     dplyr::rename(Season = season,
#                   ISOyear = year,
#                   Pathogen = pathogen,
#                   PositiveCount = positive_count,
#                   TotalSamples = total_samples,
#                   PositivityPercentage = positivity_percentage) %>%
#     select(WeekBeginning, WeekEnding, Season, ISOweek, ISOyear, Pathogen, PositiveCount, TotalSamples, PositivityPercentage)
#   
#   assign(glue("Respiratory_test_pos_{filename}_od"), df1) 
# }
# 
# for (filename in filenames[2]){
#   
#   df2 <-  base::get(glue("Respiratory_test_pos_{filename}")) %>%
#     mutate(WeekEnding = ISOweek2date(paste0(year, "-W",
#                                             str_pad(as.character(ISOweek), width = 2,
#                                                     side = "left", pad = "0"), "-7")),
#            WeekBeginning = WeekEnding - days(6)) %>%
#     mutate(WeekEnding = gsub("-", "", as.character(WeekEnding)),
#            WeekBeginning = gsub("-", "", as.character(WeekBeginning))) %>%
#     mutate(WeekEnding = as.numeric(as.character(WeekEnding)),
#            WeekBeginning = as.numeric(as.character(WeekBeginning))) %>%
#     dplyr::rename(Season = season,
#                   ISOyear = year,
#                   Pathogen = pathogen,
#                   AgeGroup = agegrp,
#                   PositiveCount = positive_count,
#                   TotalSamples = total_samples,
#                   PositivityPercentage = positivity_percentage) %>%
#     select(WeekBeginning, WeekEnding, Season, ISOweek, ISOyear, Pathogen, AgeGroup, PositiveCount, TotalSamples, PositivityPercentage)
#   
#   assign(glue("Respiratory_test_pos_{filename}_od"), df2) 
# }# Output to Open Data subfolder with datestamp - no longer needed, saved out elsewhere
#write_csv(Respiratory_test_pos_agg_od, glue(od_folder, "new/", "Respiratory_Pathogens_Test_Positivity_{od_report_date}.csv"))
