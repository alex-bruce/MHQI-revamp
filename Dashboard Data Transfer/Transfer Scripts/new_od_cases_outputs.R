#This script creates "CASES" open data outputs
#03/10/2025
#Nipuni Rajapaksha

#data sourced from 0_Raw_data_CASES.R

################## CASES ##########################

#1.Scotland level--------------------------------------------------------

##1a.Non_covid----
noncovid_scotland_cases<-i_non_covid_scotland %>%
  select(WeekBeginning=ISOweek_beginning,
         WeekEnding=ISOweek_ending,
         Pathogen=pathogen,NumberCasesPerWeek=count)


##1b.COVID-19----
covid_19_scotland_cases <- i_covid_cases_raw%>%
  #weekly cases
  group_by(WeekBeginning,WeekEnding)%>%
  summarise(weekly_positive = sum(flag_episode)) %>% 
  ungroup() %>% 
  #matching columns
  mutate(Pathogen="COVID-19") %>% 
  select(WeekBeginning,WeekEnding,
         Pathogen,NumberCasesPerWeek=weekly_positive)

## final df-Scotland level combined----
all_resp_scotland_cases<-rbind(noncovid_scotland_cases,covid_19_scotland_cases) %>% 
  mutate(Country="S92000003") %>% 
  merge(season_week,by=c("WeekBeginning","WeekEnding")) %>% 
  mutate(WeekBeginning=str_remove_all(WeekBeginning,"-"),
         WeekEnding=str_remove_all(WeekEnding,"-")) %>% 
  relocate(Country,.after = WeekEnding) %>% 
  relocate(c(Season,ISOyear,ISOweek),.before=WeekBeginning) %>% 
  arrange(WeekBeginning) 


##write csv----
message("Writing Cases_All_respiratory_pathogens_Scotland file")
write_csv(all_resp_scotland_cases,
          paste0(file_paths$Outputs$Output_folder,
                 "Cases_All_respiratory_pathogens_Scotland_",od_report_date,".csv"))

###############################################################################-

#2.Age group and sex--------------------------------------------------

##2a.Non_covid----
noncovid_agegrp_sex_cases_a<-i_non_covid_agegp_sex %>% 
  mutate(agegp = recode(agegp,
                        "gunder1" = "<1",
                        "g1to4" = "1 to 4",
                        "g5to14" = "5 to 14",
                        "g15to44" = "15 to 44",
                        "g45to64" = "45 to 64",
                        "g65to74" = "65 to 74",
                        "g75plus" = "75+"),
         agegp = factor(agegp,
                        levels = c("<1", "1 to 4", "5 to 14", "15 to 44", 
                                   "45 to 64", "65 to 74", "75+"))) %>% 
  select(WeekBeginning=ISOweek_beginning,
         WeekEnding=ISOweek_ending,Pathogen=pathogen,
         Sex=sex,AgeGroup=agegp,weeklyCases=count) %>% 
  arrange(WeekBeginning,Sex,AgeGroup)

###Creating totals (QF) rows----
#Sex total--
noncovid_agegrp_sex_cases_b<-noncovid_agegrp_sex_cases_a %>% 
  group_by(WeekBeginning,WeekEnding,Pathogen,Sex) %>% 
  summarise(weeklyCases=sum(weeklyCases)) %>% 
  mutate(AgeGroup="Total")

#Age group totals--
noncovid_agegrp_sex_cases_c<-noncovid_agegrp_sex_cases_a %>% 
  group_by(WeekBeginning,WeekEnding,Pathogen,AgeGroup) %>% 
  summarise(weeklyCases=sum(weeklyCases)) %>% 
  mutate(Sex="Total")


#Weekly total--
noncovid_agegrp_sex_cases_d<-noncovid_agegrp_sex_cases_a %>% 
  group_by(WeekBeginning,WeekEnding,Pathogen) %>% 
  summarise(weeklyCases=sum(weeklyCases)) %>% 
  mutate(Sex="Total",
         AgeGroup="Total")

###Non covid cases by agegroup and sex final df----
noncovid_agegrp_sex_cases<-
  rbind(noncovid_agegrp_sex_cases_a,noncovid_agegrp_sex_cases_b,
        noncovid_agegrp_sex_cases_c,noncovid_agegrp_sex_cases_d) %>%
  mutate(Sex=recode(Sex,"F"="Female",
                    "M"="Male")) %>% 
  ##merge all choices combination df to insert 0s
  merge(df_template_agegroup_sex%>% 
          filter(Pathogen!="COVID-19"),
        by=c("WeekBeginning","WeekEnding",
             "Pathogen","Sex","AgeGroup"),
        all=TRUE) %>% 
  #impute 0 for NAs
  mutate(weeklyCases=if_else(is.na(weeklyCases),0,weeklyCases),
         #QF columns
         AgeGroupQF= if_else(AgeGroup=="Total","d",""),
         SexQF= if_else(Sex=="Total","d","")) %>% 
  relocate(SexQF,.after = Sex) %>% 
  relocate(AgeGroupQF,.after = AgeGroup) %>% 
  relocate(c(Season,ISOyear,ISOweek),.before =WeekBeginning) %>% 
  rename(NumberCasesPerWeek=weeklyCases) %>% 
  mutate(WeekBeginning=str_remove_all(WeekBeginning,"-"),
         WeekEnding=str_remove_all(WeekEnding,"-"))

#####-----####-  

##2b.COVID-19----
covid_19_agegroup_sex_cases_a <- i_covid_cases_raw%>%
  group_by(WeekBeginning,WeekEnding,Sex,AgeGroup) %>% 
  summarise(NumberCasesPerWeek=sum(flag_episode)) 


###Creating totals rows----
#Sex total--
covid_19_agegroup_sex_cases_b<-covid_19_agegroup_sex_cases_a %>% 
  group_by(WeekBeginning,WeekEnding,Sex) %>% 
  summarise(NumberCasesPerWeek=sum(NumberCasesPerWeek)) %>% 
  mutate(AgeGroup="Total")

#Age group totals--
covid_19_agegroup_sex_cases_c<-covid_19_agegroup_sex_cases_a %>% 
  group_by(WeekBeginning,WeekEnding,AgeGroup) %>% 
  summarise(NumberCasesPerWeek=sum(NumberCasesPerWeek)) %>% 
  mutate(Sex="Total")


#Weekly total--
covid_19_agegroup_sex_cases_d<-covid_19_agegroup_sex_cases_a %>% 
  group_by(WeekBeginning,WeekEnding) %>% 
  summarise(NumberCasesPerWeek=sum(NumberCasesPerWeek)) %>% 
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
  ##merge all choices combination df to insert 0s
  merge(df_template_agegroup_sex %>% 
          filter(Pathogen=="COVID-19" &
                   WeekBeginning>="2020-02-24"),
        by=c("WeekBeginning","WeekEnding","Sex","AgeGroup"),
        all=TRUE) %>% 
  #impute 0 for NAs
  mutate(NumberCasesPerWeek=if_else(is.na(NumberCasesPerWeek),
                                    0,NumberCasesPerWeek),
         #QF columns
         AgeGroupQF= if_else(AgeGroup=="Total","d",""),
         SexQF= if_else(Sex=="Total","d","")) %>% 
  relocate(SexQF,.after = Sex) %>% 
  relocate(AgeGroupQF,.after = AgeGroup)  %>% 
  relocate(c(Season,ISOyear,ISOweek),.before =WeekBeginning) %>% 
  relocate(Pathogen,.after = WeekEnding) %>% 
  mutate(WeekBeginning=str_remove_all(WeekBeginning,"-"),
         WeekEnding=str_remove_all(WeekEnding,"-"))



## final df-Cases by agegroup and sex combined----
all_resp_by_ageGroup_sex_cases<-rbind(covid_19_agegroup_sex_cases,
                                      noncovid_agegrp_sex_cases) %>% 
  mutate(Country="S92000003") %>% 
  relocate(c(Country, Pathogen),.after = WeekEnding) %>% 
  mutate(AgeGroup=factor(
    AgeGroup,levels = c("<1", "1 to 4", "5 to 14", "15 to 44", 
                        "45 to 64", "65 to 74", "75+","Unknown","Total")),
    Sex=factor(Sex,levels=c("Female","Male","Unknown","Total")))%>% 
  arrange(WeekBeginning,Pathogen,Sex,AgeGroup) 

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
  mutate(HBcode = recode(HealthBoard,
                         "AA" = "S08000015","BR" = "S08000016",
                         "DG" = "S08000017","FF" = "S08000029",
                         "FV" = "S08000019","GC" = "S08000031",
                         "GR" = "S08000020","HG" = "S08000022",
                         "LN" = "S08000032","LO" = "S08000024",
                         "OR" = "S08000025","SH" = "S08000026",
                         "TY" = "S08000030","WI" = "S08000028"),
         HBName = paste0("NHS ", phsmethods::match_area(HBcode))) %>% 
  select(WeekBeginning=ISOweek_beginning,WeekEnding=ISOweek_ending,
         Pathogen=pathogen,HBcode,HBName,NumberCasesPerWeek=count) 

#Scotland level counts to create QF column
noncovid_hb_cases_b<-noncovid_scotland_cases%>%
  mutate(HBcode="S92000003",
         HBName="Scotland")

###noncovid cases by HB final df----
noncovid_hb_cases<-rbind(noncovid_hb_cases_a,noncovid_hb_cases_b) %>% 
  #impute missing weeks and health boards
  merge(df_template_hb %>% 
          filter(Pathogen!="COVID-19"),
        by=c("WeekBeginning","WeekEnding","Pathogen",
             "HBcode","HBName"),
        all=TRUE) %>% 
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
  select(WeekBeginning,WeekEnding,
         ,HBcode,HBName,NumberCasesPerWeek) %>% 
  mutate(Pathogen="COVID-19")

#Scotland level counts to create QF column
covid_19_hb_cases_b<-covid_19_scotland_cases%>% 
  mutate(HBcode="S92000003",
         HBName="Scotland")

###COVID 19 cases by hb final df----
covid_19_hb_cases<-rbind(covid_19_hb_cases_a, 
                         covid_19_hb_cases_b) %>% 
  merge(df_template_hb %>% 
          filter(Pathogen=="COVID-19" & 
                   WeekBeginning >= "2020-02-24"),
        by=c("WeekBeginning","WeekEnding",
             "HBcode","HBName", "Pathogen"),
        all=TRUE) %>% 
  relocate(c(Season,ISOyear,ISOweek),.before =WeekBeginning)

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
                 "Cases_All_respiratory_pathogens_by_HB_",od_report_date,".csv"))


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
          filter(Pathogen!="COVID-19"),
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
  relocate(c(Season,ISOyear,ISOweek),.before =WeekBeginning) %>% 
  relocate(c(Country,Pathogen),.after = WeekEnding) %>% 
  mutate(WeekBeginning=str_remove_all(WeekBeginning,"-"),
         WeekEnding=str_remove_all(WeekEnding,"-")) %>% 
  arrange(WeekBeginning,Pathogen,SIMD)


##write csv----
message("Writing Cases_all_respiratory_pathogens_by_SIMD file")
write_csv(all_resp_by_simd_cases,
          paste0(file_paths$Outputs$Output_folder,
                 "Cases_all_respiratory_pathogens_by_SIMD_",
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
         LAcode=ca2019,NumberCasesPerWeek)

covid_19_la_cases_b<-covid_19_scotland_cases %>% 
  merge(season_week,by=c("WeekBeginning","WeekEnding")) %>% 
  mutate(LAname="Scotland",
         LAcode="S92000003")

covid_19_la_cases<-rbind(covid_19_la_cases_a,covid_19_la_cases_b) %>% 
  #add missing LAs
  merge(df_template_la %>% 
          rename(LAname=local_authority,
                 LAcode=ca2019) %>% 
          filter(WeekBeginning >= min(covid_19_la_cases_a$WeekBeginning) ),
        all=TRUE) %>% 
  mutate(NumberCasesPerWeek=if_else(is.na(NumberCasesPerWeek),
                                    0,NumberCasesPerWeek),
         LAQF=if_else(LAname=="Scotland","d",""),
         LAcode=if_else(LAname=="Unknown","Unknown",LAcode)) %>% 
  relocate(LAQF,.after = LAcode) %>% 
  arrange(WeekBeginning,LAQF) %>% 
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
   noncovid_simd_cases,covid_19_la_cases_a,covid_19_la_cases_b,covid_19_la_cases)

rm(resp_filenames,resp_files,resp_files2,i_non_covid_agegp_sex,
   i_non_covid_hb,i_non_covid_scotland,
   i_non_covid_simd_curr_season,i_non_covid_simd_past,
   i_covid_cases_raw)

gc()
