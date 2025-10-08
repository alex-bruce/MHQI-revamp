# Dashboard data transfer for RAPID hospital occupancy data
# Sourced from ../dashboard_data_transfer.R

i_rapid_occupancy <- read_csv_with_options(match_base_filename(glue(input_data, "occupancy_rapid.csv")))

write.csv(i_rapid_occupancy, glue(output_folder, "occupancy_rapid.csv"), row.names = FALSE)

rm(i_rapid_occupancy)


