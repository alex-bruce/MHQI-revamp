# Dashboard data transfer for test positivity
# Sourced from ../dashboard_data_transfer.R

##### Respiratory Test Positivity

filenames <- c("agg")

for (filename in filenames){
  
  assign(glue("Respiratory_test_pos_{filename}"),
         read_csv_with_options(
           match_base_filename(
             glue("{input_data}/test_pos_{filename}.csv")
           )
         )
  )
}

for (filename in filenames){
  
  df1 <-  base::get(glue("Respiratory_test_pos_{filename}")) %>%
    mutate(WeekEnding = ISOweek2date(paste0(year, "-W",
                                            str_pad(as.character(ISOweek), width = 2,
                                                    side = "left", pad = "0"), "-7")),
           WeekBeginning = WeekEnding - days(6)) %>%
    mutate(WeekEnding = gsub("-", "", as.character(WeekEnding)),
           WeekBeginning = gsub("-", "", as.character(WeekBeginning))) %>%
    mutate(WeekEnding = as.numeric(as.character(WeekEnding)),
           WeekBeginning = as.numeric(as.character(WeekBeginning))) %>%
    select(WeekBeginning, WeekEnding, season, ISOweek, year, pathogen, positive_count, total_samples, positivity_percentage)
}


assign(glue("Respiratory_test_pos_{filename}_od"), df1)

# Output
write_csv(Respiratory_test_pos_agg, glue(output_folder, "Respiratory_Pathogens_Test_Positivity.csv"))


# Output to Open Data subfolder with datestamp
write_csv(Respiratory_test_pos_agg_od, glue(od_folder, "new/", "Respiratory_Pathogens_Test_Positivity_{od_report_date}.csv"))
