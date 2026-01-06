#This script creates "CASES" open data outputs
#03/10/2025
#Nipuni Rajapaksha

#data sourced from 0_Raw_data_CASES.R

################## CASES ##########################

#1.Scotland level--------------------------------------------------------

##1a.Non_covid----
noncovid_scotland_cases<-i_non_covid_scotland %>%
  select(Season=season,ISOyear=year,
         ISOweek=week,WeekBeginning=ISOweek_beginning,
         WeekEnding=ISOweek_ending,
         Pathogen=pathogen,NumberCasesPerWeek=count,
         RateCasesPerWeek=rate,Population=Pop)


##1b.COVID-19----
covid_19_scotland_cases <- i_covid_cases_raw%>%
  #weekly cases
  group_by(WeekBeginning,WeekEnding)%>%
  summarise(NumberCasesPerWeek = sum(flag_episode)) %>% 
  #Add pop reference to calculate rate
  merge(ref_pop_scotland,by=c("WeekBeginning","WeekEnding")) %>%
  mutate(RateCasesPerWeek=round((NumberCasesPerWeek/Population)*100000,
                                 digits = 1)) %>% 
  ungroup() %>% 
  #matching columns
  mutate(Pathogen="COVID-19") %>% 
  select(Season,ISOyear,ISOweek,WeekBeginning,WeekEnding,
         Pathogen,NumberCasesPerWeek,RateCasesPerWeek,Population)

## final df-Scotland level combined----
all_resp_scotland_cases<-rbind(noncovid_scotland_cases,covid_19_scotland_cases) %>% 
  mutate(Country="S92000003") %>% 
  mutate(WeekBeginning=str_remove_all(WeekBeginning,"-"),
         WeekEnding=str_remove_all(WeekEnding,"-")) %>% 
  relocate(Country,.after = WeekEnding) %>% 
  relocate(c(Season,ISOyear,ISOweek),.before=WeekBeginning) %>% 
  arrange(WeekBeginning,Pathogen) 


##write csv----
message("Writing Cases_All_respiratory_pathogens_Scotland file")
write_csv(all_resp_scotland_cases,
          paste0(file_paths$Outputs$Output_folder,
                 "Cases_All_respiratory_pathogens_Scotland_",od_report_date,".csv"))

###############################################################################-

#2.Age group and sex--------------------------------------------------

##2a.Non_covid----
noncovid_agegrp_sex_cases_a<-i_non_covid_agegp_sex %>% 
  select(Season=season,ISOyear=year,
         ISOweek=week,WeekBeginning=ISOweek_beginning,
         WeekEnding=ISOweek_ending,Pathogen=pathogen,
         Sex=sex,AgeGroup=agegp,NumberCasesPerWeek=count,
         RateCasesPerWeek=rate,
         Population=Pop) %>%
  mutate(RateCasesPerWeek=round(RateCasesPerWeek,digits=1)) %>% 
  arrange(WeekBeginning,Sex,AgeGroup)

###Creating totals (QF) rows----
#Sex total--
noncovid_agegrp_sex_cases_b<-noncovid_agegrp_sex_cases_a %>% 
  group_by(Season,ISOyear,
           ISOweek,WeekBeginning,WeekEnding,
           Pathogen,Sex) %>% 
  summarise(Population=sum(Population),
            NumberCasesPerWeek=sum(NumberCasesPerWeek),
            RateCasesPerWeek=round((NumberCasesPerWeek/Population)*100000,
                                   digits = 1)) %>% 
  mutate(AgeGroup="Total")

#Age group totals--
noncovid_agegrp_sex_cases_c<-noncovid_agegrp_sex_cases_a %>% 
  group_by(Season,ISOyear,
           ISOweek,WeekBeginning,WeekEnding,
           Pathogen,AgeGroup) %>% 
  summarise(Population=sum(Population),
            NumberCasesPerWeek=sum(NumberCasesPerWeek),
            RateCasesPerWeek=round((NumberCasesPerWeek/Population)*100000,
                                   digits = 1)) %>%  
  mutate(Sex="Total")


#Weekly total--
noncovid_agegrp_sex_cases_d<-noncovid_scotland_cases %>% 
  mutate(Sex="Total",
         AgeGroup="Total")

###Non covid cases by agegroup and sex final df----
noncovid_agegrp_sex_cases<-
  rbind(noncovid_agegrp_sex_cases_a,noncovid_agegrp_sex_cases_b,
        noncovid_agegrp_sex_cases_c,noncovid_agegrp_sex_cases_d)  %>% 
 mutate(#QF columns
         AgeGroupQF= if_else(AgeGroup=="Total","d",""),
         SexQF= if_else(Sex=="Total","d","")) %>% 
  relocate(SexQF,.after = Sex) %>% 
  relocate(AgeGroupQF,.after = AgeGroup) %>% 
  relocate(c(Season,ISOyear,ISOweek),.before =WeekBeginning)

#####-----####-  

##2b.COVID-19----
covid_19_agegroup_sex_cases_a <- i_covid_cases_raw%>%
  group_by(WeekBeginning,WeekEnding,Sex,AgeGroup) %>% 
  summarise(NumberCasesPerWeek=sum(flag_episode)) %>% 
  #add population data to calculate rate
  #no unknown population references
  merge(ref_pop_age_sex %>%
          select(-c(Season,ISOyear,ISOweek)) %>% 
          filter(WeekBeginning >="2020-02-24"),all=TRUE) %>% 
  #add ISO year, Season, ISOweek, pathogen variables
  merge(df_template_agegroup_sex %>% 
          filter(Pathogen=="COVID-19" &
                   WeekBeginning >="2020-02-24" &
                   Sex!="Total" & AgeGroup !="Total"),
        all=TRUE) %>% 
  mutate(RateCasesPerWeek=round((NumberCasesPerWeek/Population)*100000,
                                digits = 1),
         NumberCasesPerWeek=replace_na(NumberCasesPerWeek,0)) %>% 
  relocate(RateCasesPerWeek,.before = Population) %>% 
  relocate(c(Season,ISOyear,ISOweek),.before = WeekBeginning) %>% 
  relocate(Pathogen,.before = Sex)


###Creating totals rows----
#Sex total--
covid_19_agegroup_sex_cases_b<-covid_19_agegroup_sex_cases_a %>% 
  group_by(Season,ISOyear,ISOweek,WeekBeginning,WeekEnding,Pathogen,Sex) %>% 
  summarise(Population=sum(Population,na.rm = TRUE),
            NumberCasesPerWeek=sum(NumberCasesPerWeek),
            RateCasesPerWeek=round((NumberCasesPerWeek/Population)*100000,
                                   digits = 1)) %>% 
  mutate(AgeGroup="Total")

#Age group totals--
covid_19_agegroup_sex_cases_c<-covid_19_agegroup_sex_cases_a %>% 
  group_by(Season,ISOyear,ISOweek,WeekBeginning,WeekEnding,Pathogen,AgeGroup) %>% 
  summarise(Population=sum(Population,na.rm = TRUE),
            NumberCasesPerWeek=sum(NumberCasesPerWeek),
            RateCasesPerWeek=round((NumberCasesPerWeek/Population)*100000,
                                   digits = 1)) %>% 
  mutate(Sex="Total")


#Weekly total--
covid_19_agegroup_sex_cases_d<-covid_19_scotland_cases %>%
  mutate(Sex="Total",
         AgeGroup="Total")


###covid cases by agegroup and sex final df----
covid_19_agegroup_sex_cases<-
  rbind(covid_19_agegroup_sex_cases_a,covid_19_agegroup_sex_cases_b,
        covid_19_agegroup_sex_cases_c,covid_19_agegroup_sex_cases_d) %>%
  mutate(AgeGroup = factor(AgeGroup,
                           levels = c("<1", "1 to 4", "5 to 14", "15 to 44", 
                                      "45 to 64", "65 to 74", "75+","Unknown","Total"))) %>% 
  arrange(WeekBeginning,Sex,AgeGroup) %>%
  mutate(NumberCasesPerWeek=if_else(is.na(NumberCasesPerWeek),
                                    0,NumberCasesPerWeek),
         #QF columns
         AgeGroupQF= if_else(AgeGroup=="Total","d",""),
         SexQF= if_else(Sex=="Total","d","")) %>% 
  relocate(SexQF,.after = Sex) %>% 
  relocate(AgeGroupQF,.after = AgeGroup)  %>% 
  relocate(c(Season,ISOyear,ISOweek),.before =WeekBeginning) %>% 
  relocate(Pathogen,.after = WeekEnding) 

## final df-Cases by agegroup and sex combined----
all_resp_by_ageGroup_sex_cases<-rbind(covid_19_agegroup_sex_cases,
                                      noncovid_agegrp_sex_cases) %>% 
  mutate(Country="S92000003") %>% 
  relocate(c(Country, Pathogen),.after = WeekEnding) %>% 
  mutate(AgeGroup=factor(
    AgeGroup,levels = c("<1", "1 to 4", "5 to 14", "15 to 44", 
                        "45 to 64", "65 to 74", "75+","Unknown","Total")),
    Sex=factor(Sex,levels=c("Female","Male","Unknown","Total")))%>% 
  arrange(WeekBeginning,Pathogen,Sex,AgeGroup)%>% 
  mutate(WeekBeginning=str_remove_all(WeekBeginning,"-"),
         WeekEnding=str_remove_all(WeekEnding,"-")) 

##write csv----
message("Writing Cases_All_respiratory_pathogens_by_agegroup_sex file")
write_csv(all_resp_by_ageGroup_sex_cases,
          paste0(file_paths$Outputs$Output_folder,
                 "Cases_All_respiratory_pathogens_by_agegroup_sex_",
                 od_report_date,".csv"))

############################################################################-

#3.Health Board-------------------------------------------------------

##3a.Non COVID----
noncovid_hb_cases_a<-i_non_covid_hb %>% 
  select(Season=season,ISOyear=year,
         ISOweek=week,WeekBeginning=ISOweek_beginning,
         WeekEnding=ISOweek_ending,Pathogen=pathogen,
         HBcode,HBName,NumberCasesPerWeek=count,
         RateCasesPerWeek=rate,
         Population=Pop) %>%
  mutate(RateCasesPerWeek=round(RateCasesPerWeek,digits=1)) %>% 
  arrange(WeekBeginning,HBcode)

#Scotland level counts to create QF column
noncovid_hb_cases_b<-noncovid_scotland_cases%>%
  #Limit for FLu and RSV
  filter(Pathogen %in% c("Influenza (All)","Influenza A",
                         "Influenza B","RSV")) %>% 
  mutate(HBcode="S92000003",
         HBName="Scotland")

###noncovid cases by HB final df----
noncovid_hb_cases<-rbind(noncovid_hb_cases_a,noncovid_hb_cases_b) %>% 
  relocate(c(Season,ISOyear,ISOweek),.before =WeekBeginning)

##----##----##----##

##3b.COVID 19----

covid_19_hb_cases_a<-i_covid_cases_raw %>% 
  group_by(WeekBeginning,WeekEnding,reporting_health_board) %>% 
  summarise(NumberCasesPerWeek=sum(flag_episode)) %>% 
  mutate(HBcode = recode(reporting_health_board,
                         "Ayrshire and Arran" = "S08000015",
                         "Borders" = "S08000016",
                         "Dumfries and Galloway" = "S08000017",
                         "Fife" = "S08000029",
                         "Forth Valley" = "S08000019",
                         "Greater Glasgow and Clyde" = "S08000031",
                         "Grampian" = "S08000020",
                         "Highland" = "S08000022",
                         "Lanarkshire" = "S08000032",
                         "Lothian" = "S08000024",
                         "Orkney" = "S08000025",
                         "Shetland" = "S08000026",
                         "Tayside" = "S08000030",
                         "Western Isles" = "S08000028"),
         HBName = paste0("NHS ", phsmethods::match_area(HBcode))) %>%
  merge(ref_pop_hb) %>% 
  mutate(RateCasesPerWeek=round((NumberCasesPerWeek/Population)*100000,
                                digits = 1)) %>% 
  mutate(Pathogen="COVID-19")%>% 
  select(Season,ISOyear,ISOweek,WeekBeginning,WeekEnding,Pathogen,
         HBcode,HBName,NumberCasesPerWeek,RateCasesPerWeek,Population) 

#Scotland level counts to create QF column
covid_19_hb_cases_b<-covid_19_scotland_cases%>% 
  mutate(HBcode="S92000003",
         HBName="Scotland")

###COVID 19 cases by hb final df----
covid_19_hb_cases<-rbind(covid_19_hb_cases_a, 
                         covid_19_hb_cases_b)


##final df-Cases by HB ----
all_resp_by_hb_cases<-rbind(covid_19_hb_cases,noncovid_hb_cases) %>% 
  mutate(HBQF=if_else(HBName=="Scotland","d",""),
         NumberCasesPerWeek=if_else(is.na(NumberCasesPerWeek),0,NumberCasesPerWeek)) %>% 
  relocate(HBQF,.after = HBName) %>% 
  mutate(WeekBeginning=str_remove_all(WeekBeginning,"-"),
         WeekEnding=str_remove_all(WeekEnding,"-")) %>% 
  arrange(WeekBeginning,Pathogen,HBcode) %>% 
  relocate(Pathogen,.after = WeekEnding)

##Write csv----
message("Writing Cases_All_respiratory_pathogens_by_HB file")
write_csv(all_resp_by_hb_cases,
          paste0(file_paths$Outputs$Output_folder,
                 "Cases_COVID19_FLU_RSV_by_HB_",od_report_date,".csv"))


##############################################################################-

#4. SIMD------------------------------------------------------------------------

##4a.Non_covid----
#Add all influenza row
flu_all_simd_cases<-i_non_covid_simd %>% 
  filter(grepl("Influenza",Pathogen)) %>% 
  group_by(Season,ISOyear,ISOweek,WeekBeginning,WeekEnding,SIMD) %>% 
  summarise(NumberCasesPerWeek=sum(NumberCasesPerWeek)) %>% 
  mutate(Pathogen="Influenza (All)")


noncovid_simd_cases<-
  rbind(i_non_covid_simd,flu_all_simd_cases) %>% 
  #Add missing weeks
  merge(df_template_simd %>% 
          #Limit for FLu and RSV
          filter(Pathogen %in% c("Influenza (All)","Influenza A",
                                 "Influenza B","RSV")),
        by=c("WeekBeginning","WeekEnding","Season",
             "SIMD","Pathogen","ISOyear","ISOweek"),
        all=TRUE) %>% 
  mutate(NumberCasesPerWeek=if_else(is.na(NumberCasesPerWeek),0,
                                    NumberCasesPerWeek))

##4b.COVID-19----
covid_19_simd_cases<-i_covid_cases_raw %>% 
  select(WeekBeginning,WeekEnding,
         PostCode,flag_episode) %>% 
  #Link SIMD
  merge(ref_simd_lookup,by="PostCode",
        all.x = TRUE) %>% 
  mutate(SIMD=if_else(is.na(SIMD),"Unknown",SIMD)) %>% 
  group_by(WeekBeginning,WeekEnding,SIMD) %>% 
  summarise(NumberCasesPerWeek=sum(flag_episode)) %>% #Cases by SIMD
  #Add missing weeks
  merge(df_template_simd %>% 
          filter(Pathogen=="COVID-19" & WeekBeginning >="2020-02-24"),
        by=c("WeekBeginning","WeekEnding","SIMD"),all=TRUE) %>% 
  mutate(NumberCasesPerWeek=if_else(is.na(NumberCasesPerWeek),0,
                                    NumberCasesPerWeek))

##final df - Cases by SIMD----
all_resp_by_simd_cases<-rbind(noncovid_simd_cases,covid_19_simd_cases) %>% 
  mutate(Country="S92000003") %>% 
  mutate(SIMD = recode(SIMD,
                       "1"="1 (most deprived)",
                       "5"= "5 (least deprived)")) %>% 
  mutate(SIMD = factor(SIMD, 
                       levels = c("1 (most deprived)","2","3","4",
                                  "5 (least deprived)","Unknown"))) %>% 
  #add population and rates
  merge(ref_pop_simd %>% select(-ISOyear),
        by=c("Season","SIMD"),all.x=TRUE) %>% 
  mutate(RateCasesPerWeek=round((NumberCasesPerWeek/Population)*100000,
                                digits = 1)) %>%
  select(Season,ISOyear,ISOweek,WeekBeginning,WeekEnding,Country,Pathogen,
         SIMD,NumberCasesPerWeek,RateCasesPerWeek,Population)%>% 
  mutate(WeekBeginning=str_remove_all(WeekBeginning,"-"),
         WeekEnding=str_remove_all(WeekEnding,"-")) %>% 
  arrange(WeekBeginning,Pathogen,SIMD)


##write csv----
message("Writing Cases_all_respiratory_pathogens_by_SIMD file")
write_csv(all_resp_by_simd_cases,
          paste0(file_paths$Outputs$Output_folder,
                 "Cases_COVID19_FLU_RSV_by_SIMD_",
                 od_report_date,".csv"))

##############################################################################-

#5.Local authority----
covid_19_la_cases_a<-i_covid_cases_raw %>% 
  group_by(WeekBeginning,WeekEnding,
           local_authority) %>% 
  summarise(NumberCasesPerWeek=sum(flag_episode)) %>% 
  merge(CA_lookup,by="local_authority",
        all=TRUE) %>% 
  merge(season_week,by=c("WeekBeginning","WeekEnding")) %>% 
  mutate(Pathogen="COVID-19") %>% 
  select(Season,ISOyear,ISOweek,WeekBeginning,WeekEnding,
         Pathogen,LAname=local_authority,
         LAcode=ca2019,NumberCasesPerWeek) %>% 
  #add population and calculate rate
  merge(ref_pop_la %>% select(-ISOyear),all.x=TRUE) %>%
  mutate(RateCasesPerWeek=round((NumberCasesPerWeek/Population)*100000,
                                digits = 1))
  
covid_19_la_cases_b<-covid_19_scotland_cases %>% 
  mutate(LAname="Scotland",
         LAcode="S92000003")

covid_19_la_cases<-rbind(covid_19_la_cases_a,covid_19_la_cases_b) %>% 
  #add missing LAs
  merge(df_template_la %>% 
          rename(LAname=local_authority,
                 LAcode=ca2019) %>% 
          filter(WeekBeginning >= min(covid_19_la_cases_a$WeekBeginning) ),
        all=TRUE) %>% 
  #add missing population
  merge(ref_pop_la %>% select(-ISOyear),all.x=TRUE) %>% 
  mutate(NumberCasesPerWeek=replace_na(NumberCasesPerWeek,0),
         RateCasesPerWeek=if_else(is.na(RateCasesPerWeek) &
                                    LAname!="Unknown",0,RateCasesPerWeek),
         LAQF=if_else(LAname=="Scotland","d",""),
         LAcode=if_else(LAname=="Unknown","Unknown",LAcode)) %>% 
  relocate(LAQF,.after = LAcode) %>% 
  arrange(WeekBeginning,LAQF) %>% 
  relocate(c(LAcode,LAQF),.after = LAname) %>% 
  relocate(Population,.after = RateCasesPerWeek) %>% 
  mutate(WeekBeginning=str_remove_all(WeekBeginning,"-"),
         WeekEnding=str_remove_all(WeekEnding,"-"))

##Write csv----
message("Writing Cases_COVID19_by_LA file")
write_csv(covid_19_la_cases,
          paste0(file_paths$Outputs$Output_folder,
                 "Cases_COVID19_by_LA_",od_report_date,".csv"))


########################## END ###########################################-

#Clean environment 

rm(noncovid_scotland_cases,covid_19_scotland_cases,all_resp_scotland_cases,
   noncovid_agegrp_sex_cases_a,noncovid_agegrp_sex_cases_b,
   noncovid_agegrp_sex_cases_c,noncovid_agegrp_sex_cases_d,
   noncovid_agegrp_sex_cases,covid_19_agegroup_sex_cases_a,
   covid_19_agegroup_sex_cases_b,covid_19_agegroup_sex_cases_c,
   covid_19_agegroup_sex_cases_d,covid_19_agegroup_sex_cases,
   all_resp_by_ageGroup_sex_cases,noncovid_hb_cases_a,noncovid_hb_cases_b,
   noncovid_hb_cases,covid_19_hb_cases_a,covid_19_hb_cases_b,
   covid_19_hb_cases,all_resp_by_hb_cases,i_non_covid_simd,flu_all_simd_cases,
   noncovid_simd_cases,all_resp_by_simd_cases,
   covid_19_la_cases_a,covid_19_la_cases_b,covid_19_la_cases)

rm(resp_filenames,resp_files,resp_files2,i_non_covid_agegp_sex,
   i_non_covid_hb,i_non_covid_scotland,
   i_non_covid_simd_curr_season,i_non_covid_simd_past,
   i_covid_cases_raw)

gc()
