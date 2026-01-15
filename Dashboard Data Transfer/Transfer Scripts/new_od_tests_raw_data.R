#This script creates "TESTS" open data outputs
#08/10/2025
#Nipuni Rajapaksha


##1.RSV Flu----
load(paste0(file_paths$Data$Respiratory_swab_positivity_folder,
            "Swab_Positivity_cache_report.RData"))

#limit upto specific start dates (COVID-19 (from wk40 2022), Influenza (from wk40 2020) and RSV (from wk40 2024))
RSV_scotland_tests_a<-RSV_all_Scotland_pos %>% 
  filter(Year!=RSV_start_date$ISOyear |
           (Year==RSV_start_date$ISOyear &
              ISOweek >=RSV_start_date$ISOweek )) %>% 
  #limit upto report week
  filter(Year!=od_isoyear |
           (Year==od_isoyear & ISOweek <=od_isoweek)) %>% 
  rename(ISOyear=Year) %>% 
  mutate(Pathogen="RSV")

RSV_agGp_tests_a<-RSV_all_AgeGp_pos %>% 
  filter(Year!=RSV_start_date$ISOyear |
           (Year==RSV_start_date$ISOyear &
              ISOweek >=RSV_start_date$ISOweek )) %>% 
  #limit upto report week
  filter(Year!=od_isoyear |
           (Year==od_isoyear & ISOweek <=od_isoweek))  %>% 
  rename(ISOyear=Year) %>% 
  mutate(Pathogen="RSV")

FLU_scotland_tests_a<-Flu_AorB_Scotland_pos %>%
  filter(Year!=FLU_start_date$ISOyear |
           (Year==FLU_start_date$ISOyear &
              ISOweek >=FLU_start_date$ISOweek )) %>%
  #limit upto report week
  filter(Year!=od_isoyear |
           (Year==od_isoyear & ISOweek <=od_isoweek))  %>% 
  rename(ISOyear=Year) %>% 
  mutate(Pathogen ="Influenza (All)")

FLU_agGp_tests_a<-Flu_AorB_AgeGp_pos %>%
  filter(Year!=FLU_start_date$ISOyear |
           (Year==FLU_start_date$ISOyear &
              ISOweek >=FLU_start_date$ISOweek )) %>%
  #limit upto report week
  filter(Year!=od_isoyear |
           (Year==od_isoyear & ISOweek <=od_isoweek))  %>% 
  rename(ISOyear=Year) %>% 
  mutate(Pathogen ="Influenza (All)")

##2.COVID 19----
#Only pillar1 and Pillar2 are considered for positivity and total samples (PCR)

#i_covid19_all_cases_tests_data created in Raw_data_CASES.R script

# i_covid19_PCR_data_raw<-i_covid19_all_cases_tests_data%>% 
#   #only pick PCR tests (remove LFD)
#   filter(test_type=="PCR" &
#            Date>=COVID_19_start_date$WeekBeginning)%>% 
#   select(specimen_id,Date,Sex,reporting_health_board,local_authority,
#          test_type,test_result,
#          test_result_record_source,postcode,AgeGroup) %>% 
#   #get isoweek start and end
#   merge(date_reference %>% 
#           select(Date=date,ISOweek_beginning,ISOweek_ending),
#         by="Date") %>% 
#   #remove out of Scotland, no fixed abode & NA HBs by merging with HB template
#   left_join(HB_tests,by="reporting_health_board") %>% 
#   filter(!is.na(HBcode)) %>%
#   filter(Date>=COVID_19_start_date$WeekBeginning,
#          Date <=od_sunday) 

# Read in OD Cases file 
od_tests_file <- read_all_excel_sheets(glue(input_data, 
                                            "COVID-19 Tests - File for Open Data {format(report_date-1, format='%d%m%Y')}.xlsx"))

gc()

