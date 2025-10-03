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
#write_csv(Respiratory_CARI_agegp, glue(output_folder, "Respiratory_Pathogens_CARI_Age.csv"))


# filenames <- c("scotland", "agegp_sex", "agegp", "sex", "hb")
# 
# ## Getting respiratory data
# for (filename in filenames){
#   assign(glue("i_respiratory_{filename}_agg"),
#          read_csv_with_options(
#            match_base_filename(
#              glue("{input_data}/{filename}_agg.csv")
#            )
#          )
#   )
# }