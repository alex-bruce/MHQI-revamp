#This script creates "TESTS" open data outputs
#09/10/2025
#Nipuni Rajapaksha

#data sourced from 0_Raw_data_TESTS.R

################## TESTS ##########################

#1.Scotland level--------------------------------------------------------

##1a.RSV flu-----------------------

FLU_scotland_tests<-FLU_scotland_tests_a %>% 
  merge(season_week %>% 
          filter(ISOyear>FLU_start_date$ISOyear |
                   (ISOyear==FLU_start_date$ISOyear &
                      ISOweek >=FLU_start_date$ISOweek )),
        by=c("ISOyear","ISOweek"), all=TRUE)  %>% 
  select(Season,ISOyear,ISOweek,WeekBeginning,WeekEnding,
         Pathogen,PositiveCount=Positive,TotalSamples=Samples,
         PositivityPercentage=`%_positive`) %>% 
  arrange(WeekBeginning)


RSV_scotland_tests<-RSV_scotland_tests_a %>% 
  merge(season_week %>% 
          filter(ISOyear>RSV_start_date$ISOyear |
                   (ISOyear==RSV_start_date$ISOyear &
                      ISOweek >=RSV_start_date$ISOweek )),
        by=c("ISOyear","ISOweek"), all=TRUE) %>% 
  select(Season,ISOyear,ISOweek,WeekBeginning,WeekEnding,
         Pathogen,PositiveCount=Positive,TotalSamples=Samples,
         PositivityPercentage=`%_positive`) %>% 
  arrange(WeekBeginning)


##1b.COVID 19----
# covid_allsamples_scotland_tests<-i_covid19_PCR_data_raw %>% 
#   group_by(ISOweek_beginning,ISOweek_ending) %>% 
#   summarise(TotalSamples=n())
# 
# covid_positives_scotland_tests<-i_covid19_PCR_data_raw %>% 
#   filter(test_result=="Positive") %>% 
#   group_by(ISOweek_beginning,ISOweek_ending) %>% 
#   summarise(PositiveCount=n())
# 
# COVID19_scotland_tests<-
#   merge(covid_positives_scotland_tests,
#         covid_allsamples_scotland_tests,
#         all = TRUE) %>% 
#   rename(WeekBeginning=ISOweek_beginning, WeekEnding=ISOweek_ending) %>% 
#   merge(season_week %>% 
#           filter(WeekBeginning >= COVID_19_start_date$WeekBeginning),
#         by=c("WeekBeginning","WeekEnding")) %>% 
#   mutate(Pathogen="COVID-19",
#          PositivityPercentage=as.numeric(round((PositiveCount/TotalSamples)*100,digits = 1)),
#          PositivityPercentage=if_else(is.na(PositivityPercentage),0,PositivityPercentage))

# Scotland
COVID19_scotland_tests <- od_tests_file$Scotland

##final df : flu,rsv, covid tests scotland level----

COVID_FLU_RSV_scotland_tests<-rbind(COVID19_scotland_tests,RSV_scotland_tests,
                                    FLU_scotland_tests) %>% 
  arrange(WeekBeginning) %>% 
  mutate(Country="S92000003") %>% 
  relocate(Country,.before = Pathogen) %>% 
  select(Season,ISOyear,ISOweek,WeekBeginning,WeekEnding,Country,Pathogen,
         PositiveCount,TotalSamples,
         PositivityPercentage)

##write csv----
message("Writing Tests_COVID19_FLU_RSV_Scotland file")
write_csv(COVID_FLU_RSV_scotland_tests %>% 
            mutate(WeekBeginning=str_remove_all(WeekBeginning,"-"),
                   WeekEnding=str_remove_all(WeekEnding,"-")),
          paste0(file_paths$Outputs$Output_folder,
                 "Tests_COVID19_FLU_RSV_Scotland_",
                 od_report_date,".csv"))



###############################################################################-

#2.Age group and sex--------------------------------------------------

##2a.Non_covid----

FLU_agGp_tests_b<-FLU_agGp_tests_a %>%
  merge(season_week %>%
          filter(WeekBeginning >=FLU_start_date$WeekBeginning),
        by=c("ISOyear","ISOweek"),
        all=TRUE) %>% 
  select(Season,ISOyear,ISOweek,WeekBeginning,WeekEnding,Pathogen,ReportAgeBand,
         PositiveCount=Positive,TotalSamples=Samples,
         PositivityPercentage=`%_positive`)

RSV_agGp_tests_b<-RSV_agGp_tests_a %>%
  merge(season_week %>%
          filter(WeekBeginning >=RSV_start_date$WeekBeginning),
        by=c("ISOyear","ISOweek"),
        all=TRUE) %>% 
  select(Season,ISOyear,ISOweek,WeekBeginning,WeekEnding,Pathogen,ReportAgeBand,
         PositiveCount=Positive,TotalSamples=Samples,
         PositivityPercentage=`%_positive`)

flu_rsv_agGP_tests_a<-rbind(RSV_agGp_tests_b,
                            FLU_agGp_tests_b)

flu_rsv_agGP_tests_b<-flu_rsv_agGP_tests_a %>%
  mutate(AgeGroup = recode(ReportAgeBand,
                           "Under 1" = "<1",
                           "1-4" = "1 to 4",
                           "5-14" = "5 to 14",
                           "15-44" = "15 to 44",
                           "45-64" = "45 to 64",
                           "65-74" = "65 to 74",
                           "Over 75" = "75+")) %>% 
  select(Season,ISOyear,ISOweek,WeekBeginning,WeekEnding,Pathogen,AgeGroup,
         PositiveCount,TotalSamples,
         PositivityPercentage)

##2b.COVID-19----
# covid_allsamples_agegroup_tests<-i_covid19_PCR_data_raw %>% 
#   group_by(ISOweek_beginning,ISOweek_ending,AgeGroup) %>% 
#   summarise(TotalSamples=n())
# 
# covid_positives_agegroup_tests<-i_covid19_PCR_data_raw %>%
#   filter(test_result=="Positive") %>% 
#   group_by(ISOweek_beginning,ISOweek_ending,AgeGroup) %>% 
#   summarise(PositiveCount=n())
# 
# covid19_agegroup_tests<-merge(covid_positives_agegroup_tests,
#                               covid_allsamples_agegroup_tests,
#                               all = TRUE) %>% 
#   rename(WeekBeginning=ISOweek_beginning, WeekEnding=ISOweek_ending) %>% 
#   merge(season_week %>% 
#           filter(WeekBeginning>=COVID_19_start_date$WeekBeginning),
#         all=TRUE) %>% 
#   mutate(PositiveCount=if_else(is.na(PositiveCount),0,PositiveCount),
#          TotalSamples=if_else(is.na(TotalSamples),0,TotalSamples)) %>% 
#   mutate(PositivityPercentage=as.numeric(round((PositiveCount/TotalSamples)*100,digits = 1)),
#          PositivityPercentage=if_else(is.na(PositivityPercentage),0,PositivityPercentage)) %>% 
#   mutate(Pathogen="COVID-19")

covid19_agegroup_tests <- od_tests_file$Agegroup_Sex

##final df: covid,rsv,flu by agegroup----
covid19_flu_rsv_ageGroup_tests<-
  rbind(covid19_agegroup_tests,flu_rsv_agGP_tests_b) %>% 
  mutate(Country="S92000003") %>% 
  rbind(COVID_FLU_RSV_scotland_tests %>% 
          mutate(AgeGroup="Total")) %>% 
  mutate(AgeGroupQF=if_else(AgeGroup=="Total","d",""),
         AgeGroup = factor(
           AgeGroup,
           levels = c("<1", "1 to 4", "5 to 14", "15 to 44", 
                      "45 to 64", "65 to 74", "75+","Unknown","Total"))
         #Sex=factor(Sex,levels=c("Female","Male","Unknown","Total"))
  )%>% 
  arrange(WeekBeginning,Pathogen,AgeGroup) %>% 
  select(Season,ISOyear,ISOweek,WeekBeginning,WeekEnding,Country,Pathogen,
         AgeGroup,AgeGroupQF,PositiveCount,TotalSamples,PositivityPercentage) %>% 
  arrange(WeekBeginning)

##write csv----
message("Writing Tests_COVID19_FLU_RSV_Agegroup file")
write_csv(covid19_flu_rsv_ageGroup_tests %>% 
            mutate(WeekBeginning=str_remove_all(WeekBeginning,"-"),
                   WeekEnding=str_remove_all(WeekEnding,"-")),
          paste0(file_paths$Outputs$Output_folder,
                 "Tests_COVID19_FLU_RSV_by_Agegroup_",
                 od_report_date,".csv"))

#3.HB--------------------------------------------------

##3b. COVID-19----
# covid_allsamples_hb_tests<-i_covid19_PCR_data_raw %>% 
#   group_by(ISOweek_beginning,ISOweek_ending,reporting_health_board) %>% 
#   summarise(TotalSamples=n())
# 
# covid_positives_hb_tests<-i_covid19_PCR_data_raw %>%
#   filter(test_result=="Positive") %>% 
#   group_by(ISOweek_beginning,ISOweek_ending,reporting_health_board) %>% 
#   summarise(PositiveCount=n())
# 
# covid19_hb_tests_a<-merge(covid_positives_hb_tests,
#                           covid_allsamples_hb_tests,
#                           all = TRUE) %>% 
#   filter(ISOweek_beginning>=min(covid_positives_hb_tests$ISOweek_beginning)) %>% 
#   mutate(HBcode = recode(reporting_health_board,
#                          "Ayrshire and Arran" = "S08000015","Borders" = "S08000016",
#                          "Dumfries and Galloway" = "S08000017",
#                          "Fife" = "S08000029",
#                          "Forth Valley" = "S08000019",
#                          "Greater Glasgow and Clyde" = "S08000031",
#                          "Grampian" = "S08000020","Highland" = "S08000022",
#                          "Lanarkshire" = "S08000032","Lothian" = "S08000024",
#                          "Orkney" = "S08000025","Shetland" = "S08000026",
#                          "Tayside" = "S08000030","Western Isles" = "S08000028"),
#          HBName = paste0("NHS ", phsmethods::match_area(HBcode))) %>% 
#   select(-reporting_health_board) %>% 
#   rename(WeekBeginning=ISOweek_beginning,
#          WeekEnding=ISOweek_ending)
# 
# covid19_hb_tests_b<-covid19_hb_tests_a %>% 
#   merge(season_week %>% 
#           filter(WeekBeginning>=COVID_19_start_date$WeekBeginning),
#         all=TRUE) %>% 
#   mutate(PositiveCount=if_else(is.na(PositiveCount),0,PositiveCount),
#          TotalSamples=if_else(is.na(TotalSamples),0,TotalSamples)) %>% 
#   mutate(PositivityPercentage=as.numeric(round((PositiveCount/TotalSamples)*100,digits = 1)),
#          PositivityPercentage=if_else(is.na(PositivityPercentage),0,PositivityPercentage)) %>% 
#   mutate(Pathogen="COVID-19")
# 
# ##final df: covid hb----
# covid19_hb_tests<-covid19_hb_tests_b%>% 
#   rbind(COVID_FLU_RSV_scotland_tests %>% 
#           filter(Pathogen=="COVID-19") %>% 
#           mutate(HBName="Scotland") %>%
#           rename(HBcode=Country) ) %>% 
#   mutate(HBQF=if_else(HBName=="Scotland","d","")) %>% 
#   select(Season,ISOyear,ISOweek,WeekBeginning,WeekEnding,Pathogen,
#          HBName,HBcode,HBQF,PositiveCountPCR=PositiveCount,
#          TotalSamplesPCR=TotalSamples,PositivityPercentage) %>% 
#   arrange(WeekBeginning,HBQF)

covid19_hb_tests <- od_tests_file$HB

##write csv----
message("Writing Tests_COVID19_by_HB file")
write_csv(covid19_hb_tests %>% 
            mutate(WeekBeginning=str_remove_all(WeekBeginning,"-"),
                   WeekEnding=str_remove_all(WeekEnding,"-")),
          paste0(file_paths$Outputs$Output_folder,
                 "Tests_COVID19_by_HB_",
                 od_report_date,".csv"))


#4.SIMD-------------------------------------------------------------------------

##4b. COVID-19----

# i_covid19_SIMD_tests<-
#   i_covid19_PCR_data_raw %>% 
#   rename(PostCode=postcode) %>% 
#   merge(ref_simd_lookup,by="PostCode",all.x = TRUE) %>% 
#   mutate(SIMD=if_else(is.na(SIMD),"Unknown",SIMD))
# 
# 
# covid_allsamples_simd_tests<-i_covid19_SIMD_tests %>% 
#   group_by(ISOweek_beginning,ISOweek_ending,SIMD) %>% 
#   summarise(TotalSamples=n())
# 
# covid_positives_simd_tests<-i_covid19_SIMD_tests %>%
#   filter(test_result=="Positive") %>% 
#   group_by(ISOweek_beginning,ISOweek_ending,SIMD) %>% 
#   summarise(PositiveCount=n())
# 
# 
# covid19_simd_tests<-merge(covid_positives_simd_tests,
#                           covid_allsamples_simd_tests,
#                           all = TRUE) %>% 
#   rename(WeekBeginning=ISOweek_beginning, WeekEnding=ISOweek_ending) %>% 
#   merge(season_week %>% 
#           filter(WeekBeginning>=COVID_19_start_date$WeekBeginning),
#         all=TRUE) %>% 
#   mutate(PositiveCount=if_else(is.na(PositiveCount),0,PositiveCount),
#          TotalSamples=if_else(is.na(TotalSamples),0,TotalSamples)) %>% 
#   mutate(PositivityPercentage=as.numeric(round((PositiveCount/TotalSamples)*100,digits = 1)),
#          PositivityPercentage=if_else(is.na(PositivityPercentage),0,PositivityPercentage)) %>% 
#   mutate(Pathogen="COVID-19",
#          Country="S92000003") %>% 
#   select(Season,ISOyear,ISOweek,WeekBeginning,WeekEnding,Country,Pathogen,
#          SIMD,PositiveCountPCR=PositiveCount,
#          TotalSamplesPCR=TotalSamples,PositivityPercentage) %>% 
#   arrange(WeekBeginning)

covid19_simd_tests <- od_tests_file$SIMD

##write csv----
message("Writing Tests_COVID19_by_SIMD file")
write_csv(covid19_simd_tests %>% mutate(WeekBeginning=str_remove_all(WeekBeginning,"-"),
                                        WeekEnding=str_remove_all(WeekEnding,"-")),
          paste0(file_paths$Outputs$Output_folder,
                 "Tests_COVID19_by_SIMD_",
                 od_report_date,".csv"))

#############################################################################-

#5.LA--------------------------------------------------

##3b. COVID-19----
# covid_allsamples_la_tests<-i_covid19_PCR_data_raw %>% 
#   group_by(ISOweek_beginning,ISOweek_ending,local_authority) %>% 
#   summarise(TotalSamples=n())
# 
# covid_positives_la_tests<-i_covid19_PCR_data_raw %>%
#   filter(test_result=="Positive") %>% 
#   group_by(ISOweek_beginning,ISOweek_ending,local_authority) %>% 
#   summarise(PositiveCount=n())
# 
# covid19_la_tests_a<-merge(covid_positives_la_tests,
#                           covid_allsamples_la_tests,
#                           all = TRUE) %>% 
#   filter(ISOweek_beginning>=min(covid_positives_hb_tests$ISOweek_beginning))%>% 
#   merge(CA_lookup,by="local_authority",all=TRUE) %>% 
#   rename(WeekBeginning=ISOweek_beginning,
#          WeekEnding=ISOweek_ending) %>% 
#   rename(LAname=local_authority,
#          LAcode=ca2019)
# 
# covid19_la_tests_b<-covid19_la_tests_a %>% 
#   merge(season_week %>% 
#           filter(WeekBeginning>=COVID_19_start_date$WeekBeginning),
#         all=TRUE) %>% 
#   mutate(PositiveCount=if_else(is.na(PositiveCount),0,PositiveCount),
#          TotalSamples=if_else(is.na(TotalSamples),0,TotalSamples)) %>% 
#   mutate(PositivityPercentage=as.numeric(round((PositiveCount/TotalSamples)*100,digits = 1)),
#          PositivityPercentage=if_else(is.na(PositivityPercentage),0,PositivityPercentage)) %>% 
#   mutate(Pathogen="COVID-19") %>% 
#   select(Season,ISOyear,ISOweek,WeekBeginning,WeekEnding,Pathogen,
#          LAname,LAcode,PositiveCount,TotalSamples,PositivityPercentage) %>% 
#   arrange(WeekBeginning)
# 
# 
# covid19_la_tests<-rbind(covid19_la_tests_b,
#                         COVID19_scotland_tests %>% 
#                           mutate(LAname="Scotland",
#                                  LAcode="S92000003")) %>% 
#   mutate(LAQF=if_else(LAname=="Scotland","d",""),
#          LAcode=if_else(LAname=="Unknown","Unknown",LAcode)) %>% 
#   arrange(WeekBeginning,LAQF) %>% 
#   relocate(LAQF,.after=LAcode) %>% 
#   rename(PositiveCountPCR=PositiveCount,
#          TotalSamplesPCR=TotalSamples)

covid19_la_tests <- od_tests_file$LA

##write csv----
message("Writing Tests_COVID19_by_LA file")
write_csv(covid19_la_tests %>% 
            mutate(WeekBeginning=str_remove_all(WeekBeginning,"-"),
                   WeekEnding=str_remove_all(WeekEnding,"-")),
          paste0(file_paths$Outputs$Output_folder,
                 "Tests_COVID19_by_LA_",
                 od_report_date,".csv"))



############################################################################---
rm(RSV_agGp_tests_a,RSV_agGp_tests_b,RSV_scotland_tests,
   RSV_scotland_tests_a,
   RSV_all_AgeGp_pos,Flu_AorB_AgeGp_pos,Flu_AorB_Scotland_pos,
   FLU_agGp_tests_a,FLU_scotland_tests_a,
   #covid_19_simd_cases,
   #covid_allsamples_agegroup_tests,covid_allsamples_hb_tests,
   #covid_allsamples_la_tests,
   #covid_allsamples_scotland_tests,
   #covid_allsamples_simd_tests,covid_positives_agegroup_tests,
   covid19_flu_rsv_ageGroup_tests,
   #covid_positives_hb_tests,
   #covid_positives_la_tests,
   #covid_positives_simd_tests,
   covid19_agegroup_tests,covid19_hb_tests,
   #covid19_hb_tests_a, covid19_hb_tests_b,
   covid19_la_tests,
   #covid19_la_tests_a,
   #covid19_la_tests_b,
   covid19_simd_tests,COVID19_scotland_tests)

rm(od_tests_file)

gc()



