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

sites <- c("28Sites","AllSites")

hb2019_id<-"652ff726-e676-4a20-abda-435b98dd7bdc"

hb_code <- get_resource(res_id = hb2019_id) %>%
  as_tibble() %>%
  clean_names() %>%
  filter(is.na(hb_date_archived)) %>%
  select(HealthBoardName=hb_name, HealthBoard=hb)

g_healthboard_od <- g_healthboard %>%
  filter(health_board != sites) %>%
  rename(
    WeekStartDate = Start,
    WeekEndDate = End,
    HealthBoard = health_board,
    Average = average,
    Coverage = coverage
  ) %>%
  left_join(hb_code, by = c("HealthBoard" = "HealthBoardName")) %>%
  mutate(
    Average = ifelse(is.na(Average), "", Average),
    Coverage = ifelse(is.na(Coverage), "", Coverage),
    AverageQF = if_else(Average == "", ":", ""),
    CoverageQF = if_else(Coverage == "", ":", "")
  ) %>%
  select(WeekStartDate, WeekEndDate, HealthBoard = HealthBoard.y, 
         Average, 
         !!if(any(.$AverageQF != "")) "AverageQF" else NULL, 
         Coverage, 
         !!if(any(.$CoverageQF != "")) "CoverageQF" else NULL)

write_csv(g_healthboard_od,
          glue(od_folder, "COVID_Wastewater_HB_{od_report_date}.csv"),na = "")

write_csv(g_healthboard,
          glue(output_folder, "COVID_Wastewater_HB_table.csv"))

rm(i_healthboard, g_healthboard, hb_avg, hb_cov, g_healthboard_od)
