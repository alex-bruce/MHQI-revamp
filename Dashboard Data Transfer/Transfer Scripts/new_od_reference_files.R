#This script reads refernce files required for open data
#06/10/2025
#Nipuni Rajapaksha

#1. Deprivation----

ref_simd_lookup <- 
  read_rds(file_paths$Reference_files$Deprivation)%>%
  mutate(PostCode=str_replace_all(string=pc7, pattern=" ", repl=""),
         SIMD=as.character(simd2020v2_sc_quintile)) %>%
  select(PostCode,SIMD)


#2. PostCode----

#lookups
ref_SPD <- readRDS(file_paths$Reference_files$PostCode_directory)

# CA_lookup <-ref_SPD %>%
#   select(ca2019,ca2019name)%>%
#   distinct(ca2019,ca2019name)%>%
#   rename(local_authority=ca2019name)
#saveRDS(CA_lookup,"Data/CA_lookup.rds")

#3. Population----
# All other population references are extracte from the input files by Respi team

#get year and flu season
ISOyears<-date_reference %>% 
  filter(ISOyear>=2016 &
           (date<=today()-days(7)) &
           ISOweek == 40) %>% 
  distinct(ISOyear)

year_season_start<-date_reference %>% 
  filter(ISOyear>=2016 &
           (date<=today()-days(7)) &
           ISOweek == 40) %>% 
  distinct(ISOyear,Season=flu_season_V2)

##3.1 SIMD----
ref_pop_simd_a <- 
  readRDS("/conf/linkage/output/lookups/Unicode/Populations/Estimates/DataZone2011_pop_est_2011_2022.rds") %>%
  select(ISOyear=year, total_pop, simd2020v2_sc_quintile) %>%
  filter(ISOyear >= 2016) %>%
  group_by(ISOyear,simd2020v2_sc_quintile) %>%
  summarise(Population = sum(total_pop)) %>%
  ungroup() %>%
  mutate(simd2020v2_sc_quintile = as.character(simd2020v2_sc_quintile)) %>%
  mutate(SIMD= case_when(simd2020v2_sc_quintile == 1 ~ "1 (most deprived)",
                                            simd2020v2_sc_quintile == 5 ~ "5 (least deprived)",
                                            TRUE ~ simd2020v2_sc_quintile)) %>%
  select(-simd2020v2_sc_quintile) %>% 
  ungroup()

#add the last year population for all years beyond that
no_pop_estimate_years<-anti_join(ISOyears,
                                   ref_pop_simd_a %>% select(ISOyear))

last_pop_estimate<-ref_pop_simd_a %>% 
  filter(ISOyear==max(ISOyear)) %>% 
  select(-ISOyear)

pop_estimate_for_absent_years<-crossing(no_pop_estimate_years,last_pop_estimate)

ref_pop_simd<-rbind(ref_pop_simd_a,pop_estimate_for_absent_years) %>% 
  mutate(SIMD = factor(SIMD, 
                       levels = c("1 (most deprived)","2","3","4",
                                  "5 (least deprived)"))) %>% 
  #add flu seasons
merge(year_season_start,by="ISOyear")


##3.2 LA----
ref_pop_la_a <- 
  readRDS("/conf/linkage/output/lookups/Unicode/Populations/Estimates/CA2019_pop_est_1981_2024.rds") %>% 
  #limit from 2016
  filter(year>=2016) %>% 
  #whole population
  group_by(year,ca2019,ca2019name) %>% 
  summarise(Population=sum(pop)) %>% 
  select(ISOyear=year,LAcode=ca2019,Population) %>% 
  ungroup()

no_LA_pop_years<-anti_join(ISOyears,ref_pop_la_a %>% select(ISOyear))

La_last_year_pop<-ref_pop_la_a %>% 
  filter(ISOyear==max(ISOyear)) %>% 
  select(-ISOyear)

LA_absent_year_pop<-crossing(no_LA_pop_years,La_last_year_pop)

ref_pop_la<-rbind(ref_pop_la_a,LA_absent_year_pop) %>% 
  merge(year_season_start)
  
  


rm(ISOyears,no_pop_estimate_years,no_pop_years,
   last_pop_estimate,ref_pop_simd_a,pop_estimate_for_absent_years,
   year_season_start,ref_pop_la_a,LA_absent_year_pop,La_last_year_pop,
   no_LA_pop_years)
