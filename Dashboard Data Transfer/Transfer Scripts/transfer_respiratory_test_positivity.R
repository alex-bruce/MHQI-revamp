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

# Output
write_csv(Respiratory_test_pos_agg, glue(output_folder, "Respiratory_Pathogens_Test_Positivity.csv"))


# Output to Open Data subfolder with datestamp
write_csv(Respiratory_test_pos_agg, glue(od_folder, "Respiratory_Pathogens_Test_Positivity_{od_report_date}.csv"))

