get_scotland_population <- function(pop_year) {
  gpd_base_path <- "/conf/linkage/output/lookups/Unicode/"
  
  base_hb_population <- readRDS(
    glue("{gpd_base_path}Populations/Estimates/HB2019_pop_est_5year_agegroups_1981_{pop_year}.rds")
  )
  
  pop_total <- base_hb_population %>%
    filter(year == pop_year) %>%
    summarise(pop = sum(pop)) %>%
    pull(pop)
  
  return(pop_total)
}

population_2023 <- get_scotland_population(2023)
population_2024 <- get_scotland_population(2024)
#Update when the new season hits with population_2025 <- get_scotland_population(2025)


SeasonStartWeek <- "40"  
covid_cases_weekly <- Cases_Weekly %>%
  mutate(
    WeekEnding = as.Date(as.character(WeekEnding), format = "%Y%m%d"),
    ISOWeekFull = ISOweek::ISOweek(WeekEnding),
    Year = str_sub(ISOWeekFull, 1, 4),
    ISOWeek = str_sub(ISOWeekFull, -2, -1),
    Flu_Season = case_when(
      str_sub(ISOWeekFull, -2, -1) < SeasonStartWeek ~ paste(as.numeric(Year) - 1, "/", Year, sep = ""),
      TRUE ~ paste(Year, "/", as.numeric(Year) + 1, sep = "")
    ),
    #When the new season hits, this logic line needs to be updated:
    RatePer100000 = case_when(
      Year >= "2024" ~ (NumberCasesPerWeek / population_2024) * 100000,
      TRUE ~ (NumberCasesPerWeek / population_2023) * 100000
    ),
    Weekord = case_when(
      as.numeric(str_sub(ISOWeekFull, -2, -1)) >= 40 ~ as.numeric(str_sub(ISOWeekFull, -2, -1)) - 39,
      TRUE ~ as.numeric(str_sub(ISOWeekFull, -2, -1)) + (52 - 39)
    )
  ) %>%
  #This filters to the previous 3 seasons from this year (it may potentially catch another season, but it is what it is)
  filter(as.numeric(Year) >= (as.numeric(format(Sys.Date(), "%Y")) - 3)) %>% 
  rename(Season = Flu_Season)




