#This script reads in "CARI" open data output and save them to Open Data folder
#24/10/2025
#Nipuni Rajapaksha

#1. CARI-------------------------------------------------------

cari<-read_csv(paste0(file_paths$Data$Dashboard_input_folder,
                      "cari_open_data_",report_date,".csv")) %>%
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
message("Writing CARI file")
write_csv(cari,paste0(file_paths$Outputs$Output_folder,
                      "CARI_",od_report_date,".csv"))

rm(cari)


