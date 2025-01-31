# Dashboard data transfer for Wastewater CA data
# Sourced from ../dashboard_data_transfer.R

i_councilarea=  read_csv_with_options(glue(input_data, "CAtable_NHSLothian{report_date-1}.csv")) %>% 
  select(-1)

ca_avg <- i_councilarea %>% 
  select(1,2,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65) %>% 
  pivot_longer(cols = c(3:34), names_to = "council_area", values_to = "average") 

ca_cov <- i_councilarea %>% 
  select(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66) %>% 
  pivot_longer(cols = c(3:34), names_to = "council_area", values_to = "coverage")%>% 
  mutate(council_area = gsub(" Coverage", "", council_area))

g_councilarea <- ca_avg %>% 
  left_join(ca_cov)


write_csv(g_councilarea,
          glue(output_folder, "COVID_Wastewater_CA_table.csv"))

# open data section

ca2019_id<-"967937c4-8d67-4f39-974f-fd58c4acfda5"

# CA2019 codes will be referred to as LA to be consistent with the wastewater 
#report and dashboard
ca_code <- get_resource(res_id = ca2019_id) %>%
  as_tibble() %>%
  clean_names() %>%
  filter(is.na(ca_date_archived)& is.na(hb_date_archived)) %>%
  select(council_area= ca_name, LA=ca)

g_councilarea_od  <- g_councilarea %>%
  left_join(ca_code, by = "council_area") %>%
  mutate(Average=round_half_up(average,2),
         PercentCoverage= round_half_up(coverage*100,0)) %>% 
  od_qualifiers(., "Average",":") %>%   
  mutate(WeekStartDate = as.Date(Start)) %>% 
  mutate(WeekStartDate = format(strptime(WeekStartDate, format = "%Y-%m-%d"), "%Y%m%d")) %>% 
  mutate(WeekEndDate = as.Date(End)) %>% 
  mutate(WeekEndDate  = format(strptime(WeekEndDate , format = "%Y-%m-%d"), "%Y%m%d")) %>% 
  select(WeekStartDate, WeekEndDate, LA,  Average, AverageQF, PercentCoverage)

write_csv(g_councilarea_od,
          glue(od_folder, "covid19_wastewater_LA_{od_report_date}.csv"),na = "")


rm(i_councilarea, g_councilarea, ca_avg, ca_cov, g_councilarea_od, ca2019_id)
