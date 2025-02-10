# Dashboard data transfer for Wastewater HB data
# Sourced from ../dashboard_data_transfer.R

i_healthboard <- read_csv_with_options(glue(input_data, "HBtable_NHSLothian{report_date-1}.csv")) %>% 
  select(-1)

hb_avg <- i_healthboard %>% 
  select(1,2,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33) %>% 
  pivot_longer(cols = c(3:18), names_to = "health_board", values_to = "average") %>% 
  mutate(health_board = gsub(" Avg", "", health_board))

hb_cov <- i_healthboard %>% 
  select(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34) %>% 
  pivot_longer(cols = c(3:18), names_to = "health_board", values_to = "coverage")%>% 
  mutate(health_board = gsub(" Coverage", "", health_board))

g_healthboard <- hb_avg %>% 
  left_join(hb_cov)

write_csv(g_healthboard,
          glue(output_folder, "COVID_Wastewater_HB_table.csv"))

## Open Data section

sites <- c("28Sites","AllSites")

hb2019_id<-"652ff726-e676-4a20-abda-435b98dd7bdc"

hb_code <- get_resource(res_id = hb2019_id) %>%
  as_tibble() %>%
  clean_names() %>%
  filter(is.na(hb_date_archived)) %>%
  select(hb_name, HB=hb)

g_healthboard_od <- g_healthboard %>%
  filter(health_board != sites) %>%
  rename( hb_name = health_board ) %>%
  left_join(hb_code, by = "hb_name") %>%
  mutate("Average(Mgc)"=round_half_up(average,2),
         PercentCoverage= round_half_up(coverage*100,0)) %>% 
  od_qualifiers(., "Average(Mgc)",":") %>%   #od_qualifiers(., "coverage",":") %>%  #not needed
  mutate(WeekStartDate = as.Date(Start)) %>% 
  mutate(WeekStartDate = format(strptime(WeekStartDate, format = "%Y-%m-%d"), "%Y%m%d")) %>% 
  mutate(WeekEndDate = as.Date(End)) %>% 
  mutate(WeekEndDate  = format(strptime(WeekEndDate , format = "%Y-%m-%d"), "%Y%m%d")) %>% 
  select(WeekStartDate, WeekEndDate, HB, "Average(Mgc)", "Average(Mgc)QF", PercentCoverage)

write_csv(g_healthboard_od,
          glue(od_folder, "covid19_wastewater_HB_{od_report_date}.csv"),na = "")


rm(i_healthboard, g_healthboard, hb_avg, hb_cov, g_healthboard_od, 
   sites, hb2019_id, hb_code)
