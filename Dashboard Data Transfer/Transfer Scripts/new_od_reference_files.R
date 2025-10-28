#This script reads refernce files required for open data
#06/10/2025
#Nipuni Rajapaksha

#1. Deprivation

ref_simd_lookup <- 
  read_rds(file_paths$Reference_files$Deprivation)%>%
  mutate(PostCode=str_replace_all(string=pc7, pattern=" ", repl=""),
         SIMD=as.character(simd2020v2_sc_quintile)) %>%
  select(PostCode,SIMD)


#2. PostCode

#lookups
ref_SPD <- readRDS(file_paths$Reference_files$PostCode_directory)

# CA_lookup <-ref_SPD %>%
#   select(ca2019,ca2019name)%>%
#   distinct(ca2019,ca2019name)%>%
#   rename(local_authority=ca2019name)
#saveRDS(CA_lookup,"Data/CA_lookup.rds")

