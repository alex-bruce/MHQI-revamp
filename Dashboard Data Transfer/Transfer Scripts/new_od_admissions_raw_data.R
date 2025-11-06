#This script reads in all input data for open data
#07/10/2025
#Nipuni Rajapaksha

#**Note
#admission data is currently only available for COVID19,RSV and Flu. Once rest-
#-of the data is available, update the input file in the 'Admissions' section

#file paths are loaded from 000_File_paths.R
#automated variables from 000_setup.R

################################ ADMISSIONS

#1. Scotland level--------------------------------------------------------------
i_admsn_all_pathogens_scotland<-
  read_csv(paste0(file_paths$Data$Dashboard_output_folder,
                  "all_pathogen_admissions.csv")) %>%
  select(ISOweek=ISOWeek, ISOyear=Year,cov,flu,rsv,rhino,coron,para,adeno,hmpv,mpn) %>% 
  pivot_longer(cols = c(cov,flu,rsv,rhino,coron,para,adeno,hmpv,mpn),
               names_to = "Pathogen",values_to = "NumberAdmissionsPerWeek") %>% 
  mutate(Pathogen=recode(Pathogen,
                         "cov"="COVID-19",
                         "flu"="Influenza (All)",
                         "rsv"="RSV",
                         "rhino"="Rhinovirus",
                         "coron"="Seasonal coronavirus",
                         "para"="Parainfluenza (Any Type)",
                         "adeno"="Adenovirus",
                         "hmpv"="HMPV",
                         "mpn"="Mycoplasma pneumoniae")) %>% 
  merge(season_week %>% 
          filter(WeekBeginning >="2020-09-28"),
        by=c("ISOweek","ISOyear")) %>% 
  select(WeekBeginning,WeekEnding,
         Pathogen,NumberAdmissionsPerWeek)

admsn_df_start<-i_admsn_all_pathogens_scotland%>% 
  distinct(WeekBeginning) %>% 
  arrange(WeekBeginning) %>% 
  slice_head(n=1) %>% 
  pull(WeekBeginning)

i_admsn_flu_scotland<-
  read_csv(paste0(file_paths$Data$Dashboard_input_folder,
                  "admissions_flu_",od_date,".csv")) %>% 
  select(WeekBeginning=week_start, WeekEnding=week_end,
         Pathogen=Flu_type_AB,NumberAdmissionsPerWeek=Frequency) %>% 
  filter(WeekBeginning >=admsn_df_start)


#2. Age group Sex---------------------------------------------------------------

i_admsn_covid19_flu_rsv_ageGp_Sex<-
  read_csv(paste0(file_paths$Data$Dashboard_input_folder,
                  "age_sex_weekly_adm_all paths.csv")) %>% 
  select(WeekBeginning=week_start, WeekEnding,
         Pathogen,AgeGroup,Sex,NumberAdmissionsPerWeek=Count)



#3. Health Board----------------------------------------------------------------

i_admsn_all_resp_hb<-
  read_csv(paste0(file_paths$Data$Dashboard_input_folder,
                  od_admsn_date," - hb summary - all path.csv")) %>% 
  mutate(Pathogen="Influenza (All)") %>% 
  select(WeekEnding=week_ending,Pathogen=admission_type,
         NumberAdmissionsPerWeek=n,
         HB=health_board_of_treatment) %>% 
  #only covid19,flu,rsv
  filter(Pathogen %in% c ("cov","flu","rsv")) %>% 
  mutate(Pathogen=recode(Pathogen,
                         "cov"="COVID-19",
                         "flu"="Influenza (All)",
                         "rsv"="RSV"
                         # "rhino"="Rhinovirus",
                         # "coron"="Seasonal coronavirus",
                         # "para"="Parainfluenza (Any Type)",
                         # "adeno"="Adenovirus",
                         # "hmpv"="HMPV",
                         # "mpn"="Mycoplasma pneumoniae"
                         ))


#4. SIMD------------------------------------------------------------------------

i_admsn_covid19_flu_rsv_simd<-
  read_csv(paste0(file_paths$Data$Dashboard_input_folder,
                  od_admsn_date," - simd summary - all path.csv")) %>% 
  mutate(Pathogen="Influenza (All)") %>% 
  select(WeekEnding=date,simd,contains("Total"))


gc()
