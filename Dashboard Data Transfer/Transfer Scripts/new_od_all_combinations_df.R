# Create dfs agegroups, sex, weekending, weekbeginning, health board

Pathogen<-c("Adenovirus","HMPV",
            "Influenza (All)","Influenza A",
            "Influenza B","Mycoplasma pneumoniae",
            "Parainfluenza (Any Type)","RSV","Rhinovirus",
            "Seasonal coronavirus",
            "COVID-19")

Sex<-c("Male","Female","Unknown","Total")

AgeGroup<-c("<1", "1 to 4", "5 to 14", "15 to 44", "45 to 64", 
            "65 to 74", "75+", 
            "Unknown","Total")


season_week<-date_reference %>% 
  filter(date<=od_sunday &
           date>="2016-10-03") %>% 
  select(ISOyear,Season=flu_season_V2,ISOweek,WeekBeginning=ISOweek_beginning,
         WeekEnding=ISOweek_ending) %>% 
  distinct()

date_season_week<-date_reference %>% 
  filter(date<=od_sunday &
           date>="2016-10-03") %>% 
  select(Date=date,ISOyear,Season=flu_season_V2,ISOweek,WeekBeginning=ISOweek_beginning,
         WeekEnding=ISOweek_ending) %>% 
  distinct()



#HB<-readRDS("Data/HB_lookup.RDS")

HB_admissions<-HB %>% 
  add_row(HBcode="SB0801",HBName="Golden Jubilee National Hospital") %>% 
  add_row(HBcode="S92000003",HBName="Scotland")

HB_tests<-HB %>% #tests data doesn't have NHS prefix
  mutate(reporting_health_board=
           str_replace_all(string=HBName, pattern="NHS ", repl=""))

SIMD<-c("1","2","3","4","5","Unknown")

#CA_lookup<-readRDS("Data/CA_lookup.rds")

#1. Create df with all date variables
#season_week
#date_season_week

#2.agegroup and Sex----

df_template_agegroup_sex <- crossing(
  season_week,Pathogen,Sex,AgeGroup)

#3.HB ----

df_template_hb <- crossing(
  season_week,Pathogen,HB)

#for admissions, with Golden Jubilee added 
df_template_hb_admissions <- crossing(
  season_week,Pathogen,HB_admissions)

#4.SIMD----
df_template_simd <- crossing(
  season_week,Pathogen,SIMD)

#5.Local authority---- (only covid 19 currently)
df_template_la <- crossing(
  season_week,CA_lookup) %>% 
  mutate(Pathogen="COVID-19")


