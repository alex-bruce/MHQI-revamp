#This script reads in all input data for open data
#03/10/2025
#Nipuni Rajapaksha

#file paths are loaded from 000_File_paths.R

#1. CASES-----------------------------------------------------------------------

#1a. Non COVID-19 respiratory---------------------------------------------------

##read in all files at once from input folder and create df list----
resp_filenames <- c("scotland", "agegp_sex", "hb")

resp_files<-
  #read all files
  map(resp_filenames,
      ~read_csv(match_base_filename(paste0(file_paths$Data$Dashboard_input_folder,
                       .x,"_agg.csv")))) 

##agegp_sex: seasons are in a diffrent format, correcting it
resp_files[[2]]<-resp_files[[2]] %>% 
  mutate(season = paste0(substr(season,1,5), substr(season,8,9)))

#filtering pathogens and seasons
resp_files2<-map(resp_files,
                 ~.x %>% 
                   mutate(pathogen=recode(
                     pathogen,
                     "Influenza (A or B)"="Influenza (All)",
                     "Influenza A (Any Subtype)"="Influenza A")) %>% 
                   filter(pathogen %in% non_covid_pathogens &
                            season %in% resp_seasons_to_include) %>% 
                   #get isoweek begining and ending from date reference file
                   merge(date_reference %>% 
                           select(year=ISOyear,
                                  week=ISOweek,
                                  ISOweek_beginning,ISOweek_ending),
                         by=c("year","week")) %>% 
                   arrange(year,week) %>% 
                   distinct()
)


#assign them to environment with a prefix
walk2(resp_files2, resp_filenames, 
      ~ assign(paste0("i_non_covid_", .y), .x, envir = .GlobalEnv))

##SIMD----
i_non_covid_simd_past<-
  arrow::read_parquet(
    paste0(file_paths$Data$Respiratory_ECOSS_folder,
           "/Historic Linelists/Historic_ECOSS_Episodes.parquet")
  ) %>% 
  select(CHI,SpecimenDate,ECOSSID,Organism,Type,Result,
         PostCode,Year,ISOweek,FluSeason)

i_non_covid_simd_curr_season<-
  arrow::read_parquet(
    paste0(file_paths$Data$Respiratory_ECOSS_folder,
           "Linelists/Single Detection Episodes/RespViral_CurrSeason_Episodes_report.parquet")) %>% 
  select(CHI,SpecimenDate,ECOSSID,Organism,Type,Result,PostCode,Year,ISOweek,FluSeason)


i_non_covid_simd<-rbind(i_non_covid_simd_past,i_non_covid_simd_curr_season) %>% 
  distinct() %>% 
  rename(ISOyear=Year) %>% 
  #merge week beginning and week ending
  merge(date_reference %>% 
          select(ISOweek,ISOyear,Season=flu_season_V2,
                 WeekBeginning=ISOweek_beginning,
                 WeekEnding=ISOweek_ending)%>% 
          distinct(),
        by=c("ISOyear","ISOweek")) %>% 
  filter(Season %in% resp_seasons_to_include ) %>%
  mutate(PostCode=str_replace_all(string=PostCode, pattern=" ", repl=""),
         #Influenza types
         Pathogen=if_else(Organism=="Influenza",
                          paste0(Organism," (",Type,")"),
                          Organism),
         Pathogen=recode(Pathogen,
                         "Influenza (Type A)"="Influenza A",
                         "Influenza (Type B)"="Influenza B",
                         "Parainfluenza"="Parainfluenza (Any Type)")) %>% 
  #link with SIMD
  merge(ref_simd_lookup,by="PostCode",all.x=TRUE) %>% 
  mutate(SIMD=if_else(is.na(SIMD),"Unknown",SIMD)) %>% 
  count(Season,ISOyear,ISOweek,WeekBeginning,WeekEnding,
        Pathogen,SIMD, name = "NumberCasesPerWeek") %>% 
  #limit upto current week
  filter(ISOyear != od_isoyear |
          (ISOyear == od_isoyear & ISOweek <= od_isoweek))


#1b. COVID 19 ------------------------------------------------------------------
#directly from transfer respiratory script

i_covid19_all_cases_tests_data<-
  readRDS(paste0(paste0(file_paths$Data$Covid_19_all_data_folder),
                 "Time_Series_Test_data_",od_date,".rds")) %>%
  select(specimen_id,reporting_health_board, local_authority, postcode, specimen_date,
         test_type,test_result, subject_sex, submitted_subject_sex,
         age, date_reporting, date_test_received_at_bi,
         test_result_record_source,episode_number_deduplicated, episode_derived_case_type) %>% 
  filter(specimen_date>="2020-02-24" & specimen_date <=od_sunday) %>% 
  # filter(!(reporting_health_board %in% c("UK (not resident in Scotland)",
  #                                        "Outside UK","No Fixed Abode"))&
  #          !is.na(reporting_health_board)) %>%
  #keep only valid HBs
  merge(HB_tests %>% select(reporting_health_board),
        by="reporting_health_board") %>% 
  mutate(Sex = case_when(is.na(subject_sex)~submitted_subject_sex,TRUE ~ subject_sex))%>% 
  mutate(Sex=case_when(is.na(Sex)~"Unknown", 
                       Sex=="NotSpecified"~"Unknown",
                       Sex=="Unknown"~"Unknown",
                       Sex=="U"~"Unknown", Sex=="FEMALE"~"Female",
                       Sex=="MALE"~"Male",TRUE ~ Sex)) %>% 
  select(specimen_id,reporting_health_board, local_authority, postcode, specimen_date,
         test_type,test_result, Sex, age, date_reporting, date_test_received_at_bi,
         test_result_record_source,episode_number_deduplicated, episode_derived_case_type) %>% 
  rename(Date=specimen_date) %>% 
  #get isoweek start and end
  merge(date_season_week %>% 
          select(Date,WeekBeginning,WeekEnding),
        by="Date")%>% 
  mutate(AgeGroup=case_when(is.na(age) ~ "Unknown",
                            age<1~"<1",
                            age>=1 & age <=4~"1 to 4",
                            age>=5 & age<=14~"5 to 14",
                            age>=15 & age<=44~ "15 to 44",
                            age>=45 & age<=64~ "45 to 64",
                            age>=65 & age<=74~ "65 to 74",
                            age>=75~"75+",
                            TRUE~"Unknown"),
         AgeGroup = factor(AgeGroup,
                           levels = c("<1", "1 to 4", "5 to 14", "15 to 44",
                                      "45 to 64", "65 to 74", "75+","Unknown")))


i_covid_cases_raw<- i_covid19_all_cases_tests_data %>% 
  mutate(PostCode=str_replace_all(string=postcode, pattern=" ", repl=""))%>%
  mutate(episode_number_deduplicated = replace_na(episode_number_deduplicated,0),
         flag_episode = ifelse(episode_number_deduplicated>0,1,0),
         flag_first_infection = ifelse(episode_number_deduplicated==1,1,0),
         flag_reinfection = ifelse(episode_number_deduplicated>1,1,0))%>%
  #filter(episode_number_deduplicated != 0) %>%
  mutate(Date=as.Date(Date)) %>%
  filter(Date <= as.Date(od_sunday))

##################################### END #######################################-

gc()


