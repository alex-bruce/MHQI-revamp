# Dashboard data transfer for RAPID hospital occupancy data
# Sourced from ../dashboard_data_transfer.R

i_rapid_occupancy <- read_csv_with_options(match_base_filename(glue(input_data, "occupancy_rapid.csv")))

write.csv(g_rsv_admissions, glue(output_folder, "RSV_admissions.csv"), row.names = FALSE)

rm(i_rsv_admissions, g_rsv_admissions)


