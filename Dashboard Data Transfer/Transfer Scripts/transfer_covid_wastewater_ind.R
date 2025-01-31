# Dashboard data transfer for Wastewater IND data
# Sourced from ../dashboard_data_transfer.R

i_individualsite=  read_csv_with_options(glue(input_data, "INDtable_NHSLothian{report_date-1}.csv")) %>% 
  select(-1)

avg_cols <- names(i_individualsite)[grepl("Avg", names(i_individualsite))]
cov_cols <- names(i_individualsite)[grepl("Coverage", names(i_individualsite))]

ind_avg <- i_individualsite %>%
  select(1:2, all_of(avg_cols)) %>%
  pivot_longer(
    cols = all_of(avg_cols), 
    names_to = "individual_site", 
    values_to = "average"
  )%>%
  mutate(individual_site = gsub(" Avg", "", individual_site))

ind_cov <- i_individualsite %>%
  select(1:2, all_of(cov_cols)) %>%
  pivot_longer(
    cols = all_of(cov_cols), 
    names_to = "individual_site", 
    values_to = "coverage"
  )%>%
  mutate(individual_site = gsub(" Coverage", "", individual_site))

g_individualsite <- ind_avg %>%
  left_join(ind_cov, by = c("Start", "End", "individual_site"))

# open data section

g_individualsite_od <- g_individualsite %>%
  mutate(Average=round_half_up(average,2),
         PercentCoverage= round_half_up(coverage*100,0)) %>% 
  od_qualifiers(., "Average",":") %>%   
  mutate(WeekStartDate = as.Date(Start)) %>% 
  mutate(WeekStartDate = format(strptime(WeekStartDate, format = "%Y-%m-%d"), "%Y%m%d")) %>% 
  mutate(WeekEndDate = as.Date(End)) %>% 
  mutate(WeekEndDate  = format(strptime(WeekEndDate , format = "%Y-%m-%d"), "%Y%m%d")) %>% 
  select(WeekStartDate, WeekEndDate,  WastewaterTreatmentWork=individual_site,
         Average, AverageQF, PercentCoverage)
  

write_csv(g_individualsite_od,
          glue(od_folder, "covid19_wastewater_wwtw_{od_report_date}.csv"),na = "")


rm(i_individualsite, avg_cols, cov_cols, ind_cov, ind_avg, g_individualsite, g_individualsite_od)
