### File only to be run temporarily while processes are being updated
### After updates are finished, files will be moved directly from the
### Weekly Data Folder to the Analyst Space (with Input folder, Output 
### folder and dashboard_data_transfer.R no longer required)

updated_files <- c("COVID_Wastewater_National_table.csv",
                   "Respiratory_Euromomo.csv",
                   "Respiratory_Pathogens_CARI_Age.csv",
                   "Respiratory_Pathogens_CARI_codetections.csv",
                   "Respiratory_Pathogens_CARI_duodetections.csv",
                   "Respiratory_Pathogens_CARI_HB.csv",
                   "Respiratory_Pathogens_CARI_Scot.csv",
                   "COVID_Wastewater_CA_table.csv",
                   "COVID_Wastewater_HB_table.csv")

updates_to_move <- paste(input_data, updated_files, sep="")

purrr::walk(updates_to_move, file.copy, to = output_folder, recursive=TRUE, overwrite=TRUE)

