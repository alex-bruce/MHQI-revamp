#This script reads in "CARI" open data output and save them to Open Data folder
#24/10/2025
#Nipuni Rajapaksha

#1. CARI-------------------------------------------------------

# cari<-read_csv(paste0(file_paths$Data$Dashboard_input_folder,
#                       "cari_open_data_",report_date,".csv")) %>%
#   mutate(HB=str_replace_all(HB,"&","and")) %>% 
#   #standardising HBnames and codes
#   merge(HB_tests %>% rename(HB=reporting_health_board),
#         all=TRUE) %>% 
#   mutate(HBName=if_else(is.na(HBName),HB,HBName),
#          HBcode=if_else(is.na(HBcode),HBName,HBcode)) %>% 
#   relocate(c(HBName,HBcode,Pathogen),.after = WeekEnding) %>% 
#   select(-HB) %>% 
#   mutate(Pathogen=recode(Pathogen, #left=current name, right=new name
#                          "Adeno"="Adenovirus",
#                          "hMPV"="HMPV",
#                          "Flu_A_or_B"="Influenza (All)",
#                          "Flu_A"="Influenza A",
#                          "Flu_B"="Influenza B",
#                          "MPN"="Mycoplasma pneumoniae",
#                          "Parainf"="Parainfluenza (Any Type)",
#                          "RSV"="RSV",
#                          "Rhino"="Rhinovirus",
#                          "Corona"="Seasonal coronavirus",
#                          "SARS_CoV2"="COVID-19"
#   )) %>% 
#   arrange(WeekBeginning,Pathogen,HBcode,Sex,AgeGroup) %>% 
#   mutate(across(
#     c(starts_with("TestPositivity")), 
#     ~if_else(is.na(.),"0.0%",.))
#   ) %>% 
#   mutate(across(
#     c(WeekBeginning,WeekEnding),
#     ~str_remove_all(.,"-")))
# 
# 
# 
# ##write csv----
# message("Writing CARI file")
# write_csv(cari,paste0(file_paths$Outputs$Output_folder,
#                       "CARI_",od_report_date,".csv"))
# 
# rm(cari)


# 2022/2023 file
cari_2223 <- read_csv(paste0(file_paths$Data$Dashboard_input_folder,
                      "cari_open_data_2223_",report_date,".csv")) %>%
  mutate(HB=str_replace_all(HB,"&","and")) %>% 
  #standardising HBnames and codes
  merge(HB_tests %>% rename(HB=reporting_health_board),
        all=TRUE) %>% 
  mutate(HBName=if_else(is.na(HBName),HB,HBName),
         HBcode=if_else(is.na(HBcode),HBName,HBcode)) %>% 
  relocate(c(HBName,HBcode,Pathogen),.after = WeekEnding) %>% 
  select(-HB) %>% 
  mutate(Pathogen=recode(Pathogen, #left=current name, right=new name
                         "Adeno"="Adenovirus",
                         "hMPV"="HMPV",
                         "Flu_A_or_B"="Influenza (All)",
                         "Flu_A"="Influenza A",
                         "Flu_B"="Influenza B",
                         "MPN"="Mycoplasma pneumoniae",
                         "Parainf"="Parainfluenza (Any Type)",
                         "RSV"="RSV",
                         "Rhino"="Rhinovirus",
                         "Corona"="Seasonal coronavirus",
                         "SARS_CoV2"="COVID-19"
  )) %>% 
  arrange(WeekBeginning,Pathogen,HBcode,Sex,AgeGroup) %>% 
  mutate(across(
    c(starts_with("TestPositivity")), 
    ~if_else(is.na(.),"0.0%",.))
  ) %>% 
  mutate(across(
    c(WeekBeginning,WeekEnding),
    ~str_remove_all(.,"-")))



##write csv----
message("Writing CARI 2022/2023 file")
write_csv(cari_2223,paste0(file_paths$Outputs$Output_folder,
                      "CARI_2223_",od_report_date,".csv"))

rm(cari_2223)

# 2023/2024 file
cari_2324 <- read_csv(paste0(file_paths$Data$Dashboard_input_folder,
                             "cari_open_data_2324_",report_date,".csv")) %>%
  mutate(HB=str_replace_all(HB,"&","and")) %>% 
  #standardising HBnames and codes
  merge(HB_tests %>% rename(HB=reporting_health_board),
        all=TRUE) %>% 
  mutate(HBName=if_else(is.na(HBName),HB,HBName),
         HBcode=if_else(is.na(HBcode),HBName,HBcode)) %>% 
  relocate(c(HBName,HBcode,Pathogen),.after = WeekEnding) %>% 
  select(-HB) %>% 
  mutate(Pathogen=recode(Pathogen, #left=current name, right=new name
                         "Adeno"="Adenovirus",
                         "hMPV"="HMPV",
                         "Flu_A_or_B"="Influenza (All)",
                         "Flu_A"="Influenza A",
                         "Flu_B"="Influenza B",
                         "MPN"="Mycoplasma pneumoniae",
                         "Parainf"="Parainfluenza (Any Type)",
                         "RSV"="RSV",
                         "Rhino"="Rhinovirus",
                         "Corona"="Seasonal coronavirus",
                         "SARS_CoV2"="COVID-19"
  )) %>% 
  arrange(WeekBeginning,Pathogen,HBcode,Sex,AgeGroup) %>% 
  mutate(across(
    c(starts_with("TestPositivity")), 
    ~if_else(is.na(.),"0.0%",.))
  ) %>% 
  mutate(across(
    c(WeekBeginning,WeekEnding),
    ~str_remove_all(.,"-")))



##write csv----
message("Writing CARI 2023/2024 file")
write_csv(cari_2324,paste0(file_paths$Outputs$Output_folder,
                           "CARI_2324_",od_report_date,".csv"))

rm(cari_2324)

# 2024/2025 file
cari_2425 <- read_csv(paste0(file_paths$Data$Dashboard_input_folder,
                             "cari_open_data_2425_",report_date,".csv")) %>%
  mutate(HB=str_replace_all(HB,"&","and")) %>% 
  #standardising HBnames and codes
  merge(HB_tests %>% rename(HB=reporting_health_board),
        all=TRUE) %>% 
  mutate(HBName=if_else(is.na(HBName),HB,HBName),
         HBcode=if_else(is.na(HBcode),HBName,HBcode)) %>% 
  relocate(c(HBName,HBcode,Pathogen),.after = WeekEnding) %>% 
  select(-HB) %>% 
  mutate(Pathogen=recode(Pathogen, #left=current name, right=new name
                         "Adeno"="Adenovirus",
                         "hMPV"="HMPV",
                         "Flu_A_or_B"="Influenza (All)",
                         "Flu_A"="Influenza A",
                         "Flu_B"="Influenza B",
                         "MPN"="Mycoplasma pneumoniae",
                         "Parainf"="Parainfluenza (Any Type)",
                         "RSV"="RSV",
                         "Rhino"="Rhinovirus",
                         "Corona"="Seasonal coronavirus",
                         "SARS_CoV2"="COVID-19"
  )) %>% 
  arrange(WeekBeginning,Pathogen,HBcode,Sex,AgeGroup) %>% 
  mutate(across(
    c(starts_with("TestPositivity")), 
    ~if_else(is.na(.),"0.0%",.))
  ) %>% 
  mutate(across(
    c(WeekBeginning,WeekEnding),
    ~str_remove_all(.,"-")))



##write csv----
message("Writing CARI 2024/2025 file")
write_csv(cari_2425,paste0(file_paths$Outputs$Output_folder,
                           "CARI_2425_",od_report_date,".csv"))

rm(cari_2425)


# 2025/2026 file
cari_2526 <- read_csv(paste0(file_paths$Data$Dashboard_input_folder,
                             "cari_open_data_2526_",report_date,".csv")) %>%
  mutate(HB=str_replace_all(HB,"&","and")) %>% 
  #standardising HBnames and codes
  merge(HB_tests %>% rename(HB=reporting_health_board),
        all=TRUE) %>% 
  mutate(HBName=if_else(is.na(HBName),HB,HBName),
         HBcode=if_else(is.na(HBcode),HBName,HBcode)) %>% 
  relocate(c(HBName,HBcode,Pathogen),.after = WeekEnding) %>% 
  select(-HB) %>% 
  mutate(Pathogen=recode(Pathogen, #left=current name, right=new name
                         "Adeno"="Adenovirus",
                         "hMPV"="HMPV",
                         "Flu_A_or_B"="Influenza (All)",
                         "Flu_A"="Influenza A",
                         "Flu_B"="Influenza B",
                         "MPN"="Mycoplasma pneumoniae",
                         "Parainf"="Parainfluenza (Any Type)",
                         "RSV"="RSV",
                         "Rhino"="Rhinovirus",
                         "Corona"="Seasonal coronavirus",
                         "SARS_CoV2"="COVID-19"
  )) %>% 
  arrange(WeekBeginning,Pathogen,HBcode,Sex,AgeGroup) %>% 
  mutate(across(
    c(starts_with("TestPositivity")), 
    ~if_else(is.na(.),"0.0%",.))
  ) %>% 
  mutate(across(
    c(WeekBeginning,WeekEnding),
    ~str_remove_all(.,"-")))



##write csv----
message("Writing CARI 2025/2026 file")
write_csv(cari_2526,paste0(file_paths$Outputs$Output_folder,
                           "CARI_2526_",od_report_date,".csv"))

rm(cari_2526)
