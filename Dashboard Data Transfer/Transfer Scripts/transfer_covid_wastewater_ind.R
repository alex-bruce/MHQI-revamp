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


g_individualsite_od <- g_individualsite %>%
  rename(WeekStartDate = Start,
         WeekEndDate = End,
         WastewaterTreatmentWork = individual_site,
         Average = average,
         Coverage = coverage)%>%
  mutate(
    Average = ifelse(is.na(Average), "", Average),
    Coverage = ifelse(is.na(Coverage), "", Coverage),
    AverageQF = if_else(Average == "", ":", ""),
    CoverageQF = if_else(Coverage == "", ":", "")
  ) %>%
  select(WeekStartDate, WeekEndDate, WastewaterTreatmentWork, 
         Average, 
         !!if(any(.$AverageQF != "")) "AverageQF" else NULL, 
         Coverage, 
         !!if(any(.$CoverageQF != "")) "CoverageQF" else NULL)

write_csv(g_individualsite_od,
          glue(od_folder, "COVID_Wastewater_IND_table_{od_report_date}.csv"),na = "")


rm(i_individualsite, avg_cols, cov_cols, ind_cov, ind_avg, g_individualsite, g_individualsite_od)
