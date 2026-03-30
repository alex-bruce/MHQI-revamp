# Dashboard data transfer for Cases
# Sourced from ../dashboard_data_transfer.R

##### Cases

i_cases <- read_all_excel_sheets(glue(input_data, "Lab_Data_New_{format(report_date-2, format='%Y-%m-%d')}.xlsx"))

# g_cases <- i_cases$`Cumulative confirmed cases`
# 
# g_cases %<>%
#   dplyr::rename(NumberCasesPerDay = `Number of cases per day`) %>%
#   mutate(NumberCasesPerDay = as.numeric(NumberCasesPerDay),
#          Cumulative = as.numeric(Cumulative))
# 
# pop_grandtotal <- i_population_v2 %>%
#   filter(AgeGroup == "Total", Sex == "Total") %>%
#   .$PopNumber
# 
# g_cases %<>%
#   mutate(SevenDayAverage = round_half_up(zoo::rollmean(NumberCasesPerDay, k = 7, fill = NA, align="right"),0),
#          SevenDayAverageQF = ifelse(is.na(SevenDayAverage), "z", ""),
#          CumulativeRatePer100000 = round_half_up(100000 * Cumulative / pop_grandtotal,1),
#          Date = format(Date, "%Y%m%d"))


#write_csv(g_cases, glue(output_folder, "Cases.csv"))

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


rm(i_cases)#, g_cases, pop_grandtotal)

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


#### CASES BY AGE AND SEX ----

# Read in input data
i_cases_agesex <- read_csv_with_options(glue(input_data, 
                                      "{format(report_date-1, format='%Y-%m-%d')}_covid_cases_agesex_season.csv"))

# Latest season and year
season <- max(i_cases_agesex$season)
season_year <- substr(season,1,4)

# Read in population lookup

pop_year = 2024# use to filter through entire script, only need to update 1 line when Pop Est files updated

gpd_base_path<-"/conf/linkage/output/lookups/Unicode/"

base_hb_population <- readRDS(glue(gpd_base_path,"Populations/Estimates/HB2019_pop_est_1981_{pop_year}.rds"))%>%
  mutate(sex=if_else(sex_name=="F", "Female", "Male"))

# Create age groups and calc population sizes
pop_agegroup_sex <- base_hb_population %>%
  mutate(age_group = case_when(
    age < 1 ~ "<1 years",
    age >= 1 & age <= 4 ~ "1-4 years",
    age >= 5 & age <= 14 ~ "5-14 years",
    age >= 15 & age <= 44 ~ "15-44 years",
    age >= 45 & age <= 64 ~ "45-64 years",
    age >= 65 & age <= 74 ~ "65-74 years",
    age >= 75 ~ "75+ years",
  )) %>%
  group_by(year, age_group, sex) %>%
  summarise(pop = sum(pop)) %>%
  ungroup()

# Latest year in pop lookup
latest_year <- max(pop_agegroup_sex$year)

# If latest_year < season_year, duplicate rows for each missing year
if (latest_year < season_year) {
  
  # Filter rows for the latest year
  latest_data <- pop_agegroup_sex %>% 
    filter(year == latest_year)
  
  # Create new rows for each missing year up to season_year
  new_rows <- lapply((latest_year + 1):season_year, function(y) {
    latest_data %>% mutate(year = y)
  }) %>% bind_rows()
  
  # Append new rows to the original dataset
  pop_agegroup_sex <- bind_rows(pop_agegroup_sex, new_rows)
  
  message("New season data added for years: ", paste((latest_year + 1):season_year, collapse = ", "))
  
} else {
  message("No new season data needed. Either already present or not yet due.")
}

# Create season variable
pop_agegroup_sex <- pop_agegroup_sex %>%
  mutate(season = paste0(year, "/", year+1)) %>%
  select(season, sex, age_group, pop)

# Join on populations
# Calculate rates
g_cases_agesex <- i_cases_agesex %>%
  left_join(pop_agegroup_sex) %>%
  mutate(rate = round_half_up((cases/pop)*100000,1))

# Save
write_csv(g_cases_agesex, glue(output_folder, "covid_cases_agesex_season.csv"))

