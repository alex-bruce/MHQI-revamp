# Dashboard data transfer for Cases
# Sourced from ../dashboard_data_transfer.R

##### Cases

i_cases <- read_all_excel_sheets(glue(input_data, "Lab_Data_New_{format(report_date-2, format='%Y-%m-%d')}.xlsx"))

g_cases <- i_cases$`Cumulative confirmed cases`

g_cases %<>%
  dplyr::rename(NumberCasesPerDay = `Number of cases per day`) %>%
  mutate(NumberCasesPerDay = as.numeric(NumberCasesPerDay),
         Cumulative = as.numeric(Cumulative))

pop_grandtotal <- i_population_v2 %>%
  filter(AgeGroup == "Total", Sex == "Total") %>%
  .$PopNumber

g_cases %<>%
  mutate(SevenDayAverage = round_half_up(zoo::rollmean(NumberCasesPerDay, k = 7, fill = NA, align="right"),0),
         SevenDayAverageQF = ifelse(is.na(SevenDayAverage), "z", ""),
         CumulativeRatePer100000 = round_half_up(100000 * Cumulative / pop_grandtotal,1),
         Date = format(Date, "%Y%m%d"))


write_csv(g_cases, glue(output_folder, "Cases.csv"))

g_cases_weekly <- i_cases$`Cumulative confirmed cases`

g_cases_weekly %<>%
  mutate(WeekEnding = ceiling_date(
    as.Date(Date),unit="week",week_start=7, change_on_boundary=FALSE)) %>%
  dplyr::rename(NumberCasesPerWeek = `Number of cases per day`) %>%
  mutate(NumberCasesPerWeek = as.numeric(NumberCasesPerWeek),
         Cumulative = as.numeric(Cumulative)) %>%
  group_by(WeekEnding) %>%
  summarise(NumberCasesPerWeek = sum(NumberCasesPerWeek)) %>%
  mutate(Cumulative = cumsum(NumberCasesPerWeek)) %>%
  mutate(WeekEnding = format(WeekEnding, "%Y%m%d"))

write_csv(g_cases_weekly, glue(output_folder, "Cases_Weekly.csv"))


rm(i_cases, g_cases, pop_grandtotal)

get_scotland_population <- function(population_year) {
  gpd_base_path <- "/conf/linkage/output/lookups/Unicode/"
  
  base_hb_population <- readRDS(
    glue("{gpd_base_path}Populations/Estimates/HB2019_pop_est_5year_agegroups_1981_{pop_year}.rds")
  )
  
  pop_total <- base_hb_population %>%
    filter(year == population_year) %>%
    summarise(pop = sum(pop)) %>%
    pull(pop)
  
  return(pop_total)
}

population_this_season <- get_scotland_population(pop_year)
population_last_season <- get_scotland_population(pop_year - 1)
population_two_seasons_ago <- get_scotland_population(pop_year - 2)


SeasonStartWeek <- "40"  
covid_cases_weekly <- g_cases_weekly %>%
  mutate(
    WeekEnding = as.Date(as.character(WeekEnding), format = "%Y%m%d"),
    ISOWeekFull = ISOweek::ISOweek(WeekEnding),
    Year = str_sub(ISOWeekFull, 1, 4),
    ISOWeek = str_sub(ISOWeekFull, -2, -1),
    Flu_Season = case_when(
      str_sub(ISOWeekFull, -2, -1) < SeasonStartWeek ~ paste(as.numeric(Year) - 1, "/", Year, sep = ""),
      TRUE ~ paste(Year, "/", as.numeric(Year) + 1, sep = "")
    ),
    RatePer100000 = case_when(
      as.numeric(Year) >= pop_year ~ (NumberCasesPerWeek / population_this_season) * 100000,
      as.numeric(Year) >= (pop_year - 1) ~ (NumberCasesPerWeek / population_last_season) * 100000,
      as.numeric(Year) >= (pop_year - 2) ~ (NumberCasesPerWeek / population_two_seasons_ago) * 100000,
      TRUE ~ NA_real_  
    ),
    Weekord = case_when(
      as.numeric(str_sub(ISOWeekFull, -2, -1)) >= 40 ~ as.numeric(str_sub(ISOWeekFull, -2, -1)) - 39,
      TRUE ~ as.numeric(str_sub(ISOWeekFull, -2, -1)) + (52 - 39)
    )
  ) %>%
  #This filters to the previous 3 seasons from this year (it may potentially catch another season, but it is what it is)
  filter(as.numeric(Year) >= (as.numeric(format(Sys.Date(), "%Y")) - 3)) %>% 
  rename(Season = Flu_Season)

write_csv(covid_cases_weekly, glue(output_folder, "covid_cases_weekly.csv"))







