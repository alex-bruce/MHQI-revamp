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

g_councilarea_od <- g_councilarea %>%
  rename(WeekStartDate = Start,
         WeekEndDate = End,
         LocalAuthority = council_area,
         Average = average,
         Coverage = coverage)%>%
  mutate(
    Average = ifelse(is.na(Average), "", Average),
    Coverage = ifelse(is.na(Coverage), "", Coverage),
    AverageQF = if_else(Average == "", ":", ""),
    CoverageQF = if_else(Coverage == "", ":", "")
  ) %>%
  select(WeekStartDate, WeekEndDate, LocalAuthority, 
         Average, 
         !!if(any(.$AverageQF != "")) "AverageQF" else NULL, 
         Coverage, 
         !!if(any(.$CoverageQF != "")) "CoverageQF" else NULL)

write_csv(g_councilarea_od,
          glue(od_folder, "COVID_Wastewater_CA_{od_report_date}.csv"),na = "")

write_csv(g_councilarea,
          glue(output_folder, "COVID_Wastewater_CA_table.csv"))

rm(i_councilarea, g_councilarea, ca_avg, ca_cov, g_councilarea_od)
