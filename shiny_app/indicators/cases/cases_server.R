
metadataButtonServer(id="cases",
                     panel="COVID-19 cases",
                     parent = session)

jumpToTabButtonServer(id="cases_from_summary",
                      location="cases",
                      parent = session)



## Added from here...
altTextServer("covid_positivity_modal",
              title = "COVID-19 test positivity",
              content = tags$ul(tags$li("This is a plot showing the test positivity rate of COVID-19 testing across Scotland."),
                                tags$li("The x axis shows the ISO week of sample, from week 40 to week 39. Week 40 is typically the start of October and when the winter respiratory season starts."),
                                tags$li("The y axis is test positivity rate."),
                                tags$li("There are several traces representing the test positivity rate across multiple seasons."),
                                tags$li("Each trace can be hidden/unhidden by clicking on the relevant season from the legend on the right of the chart.")
              )
)

output$covid_positivity_table <- renderDataTable({
  Respiratory_Pathogens_Test_Positivity %>%
    filter(pathogen == "Covid-19") %>%
    dplyr::rename(`Year` = year,
                  `Season` = season,
                  `ISO Week` = ISOweek,
                  `Total Samples` = total_samples,
                  `Positive Samples` = positive_count,
                  `Test Positivity (%)` = positivity_percentage) %>%
    select(`Year`, `ISO Week`, `Total Samples`, `Positive Samples`, `Test Positivity (%)`) %>%
    arrange(desc(`Year`), desc(`ISO Week`)) %>%
    make_table(add_separator_cols = c(2), order_by_firstcol = "desc")
})

output$covid_positivity_plot <- renderPlotly({
  Respiratory_Pathogens_Test_Positivity %>%
    create_test_pos_seasons_linechart(., "Covid-19")

})

#### to here...



altTextServer("ons_cases_modal",
              title = "Estimated COVID-19 infection rate",
              content = tags$ul(tags$li("This is a plot of the estimated COVID-19 infection rate in the population",
                                        "from the Office for National Statistics."),
                                tags$li("The x axis shows week ending, starting from 06 November 2020."),
                                tags$li("The y axis shows the official positivity estimate, as a percentage",
                                        "of the population in Scotland. "),
                                tags$li("There is one trace which includes error bars denoting confidence intervals."),
                                tags$li("The positivity estimate peaked at 1 in 11 for week ending 20 Mar 2022."),
                                tags$li("The latest positivity estimate in week ending",
                                        glue("{ONS %>% tail(1) %>% .$EndDate %>% convert_opendata_date() %>% format('%d %b %y')}"),
                                        glue("is {ONS %>% tail(1) %>% .$EstimatedRatio}")
                                )
              )
)



# altTextServer("wastewater_modal",
#               title = "Seven day average trend in wastewater COVID-19",
#               content = tags$ul(tags$li("This is a plot showing the running average trend in wastewater COVID-19."),
#                                 tags$li("The x axis shows date of sample, starting from 28 May 2020."),
#                                 tags$li("The y axis shows the wastewater viral level in million gene copies per person per day."),
#                                 tags$li("There is one trace which shows the 7 day average of the watewater viral level."),
#                                 tags$li("There have been peaks throughout the pandemic, notably in",
#                                         "Sep 2021, Dec 2021, Mar 2022 and Jun 2022")))

#altTextServer("reported_cases_modal",
              #title = "Reported COVID-19 cases",
              #content = tags$ul(tags$li("This is a plot of the number of reported COVID-19 cases each week."),
                               # tags$li("The x axis is the week ending date"),
                                #tags$li("The y axis is the number of reported cases"),
                                #tags$li("There is a navy blue trace which shows the number of reported cases each week."),
                               # tags$li("There are two vertical lines: the first denotes that prior to 5 Jan 2022 ",
                                        #"reported cases are PCR only, and since then they include PCR and LFD cases; ",
                                        #"the second marks the change in testing policy on 1 May 2022.")
            #  )
#)

#output$reported_cases_table <- renderDataTable({
  #Cases_Weekly %>%
    #mutate(WeekEnding = convert_opendata_date(WeekEnding)) %>%
    #dplyr::rename(`Reported cases` = NumberCasesPerWeek) %>%
    #select(WeekEnding, `Reported cases`) %>%
    #arrange(desc(WeekEnding)) %>%
    #make_table(add_separator_cols = c(2), order_by_firstcol = "desc")
#})

#output$reported_cases_plot <- renderPlotly({
  #Cases_Weekly %>%
    #make_weekly_reported_cases_plot()

#})


output$ons_cases_plot <- renderPlotly({
  ONS %>%
    make_ons_cases_plot()

})

output$covid_line_plot <- renderPlotly({

  covid_cases_weekly %>%
    create_covid_line_chart()
})

# COVID Rates per 100,000 table
output$covid_cases_table <- renderDataTable({
  covid_cases_weekly %>%
    arrange(desc(WeekEnding)) %>%
    select(Season, ISOWeek, RatePer100000) %>%
    mutate(Season = factor(Season),
           ISOWeek = factor(ISOWeek)) %>%
    rename(`ISO Week` = ISOWeek,
           `Rate per 100,000` = RatePer100000) %>%
    make_table(add_separator_cols_1dp = c(3),
               filter_cols = c(1,2))
})

altTextServer("reported_cases_per_100k",
title = "COVID-19 incidence rate per 100,000 population in Scotland",
content = tags$ul(tags$li("This is a plot showing the rate of laboratory-confirmed COVID-19 infection per 100,000 population in Scotland."),
 tags$li("The x axis shows the ISO week of sample, from week 40 to week 39. Week 40 is typically the start of October and when the winter respiratory season starts."),
 tags$li("The y axis shows the rate of laboratory-confirmed COVID-19 infection per 100,000 population."),
 tags$li("There is a trace for the three most recent seasons: 2023/2024, 2024/2025 and 2025/26.")
  )
)

# output$wastewater_plot <- renderPlotly({
#   Wastewater %>%
#     make_wastewater_plot()
# 
# })
# 
# output$wastewater_table <- renderDataTable({
#   Wastewater %>%
#     mutate(Date = convert_opendata_date(Date)) %>%
#            #WastewaterSevenDayAverageMgc = round_half_up(WastewaterSevenDayAverageMgc, 1)) %>%
#     dplyr::rename('7 day average (Mgc/p/d)' = WastewaterSevenDayAverageMgc) %>%
#     arrange(desc(Date)) %>%
#     make_table(add_separator_cols_2dp = 2, order_by_firstcol = "desc")
# })

###########
### MEM ###
###########

# Low threshold
covid_low_threshold <- Respiratory_Pathogens_MEM_Scot %>%
  filter(Pathogen == "Influenza") %>%
  select(LowThreshold) %>%
  distinct() %>%
  .$LowThreshold %>%
  round_half_up(2)

# Moderate threshold
covid_moderate_threshold <- Respiratory_Pathogens_MEM_Scot %>%
  filter(Pathogen == "Influenza") %>%
  select(MediumThreshold) %>%
  distinct() %>%
  .$MediumThreshold %>%
  round_half_up(2)

# High threshold
covid_high_threshold <- Respiratory_Pathogens_MEM_Scot %>%
  filter(Pathogen == "Influenza") %>%
  select(HighThreshold) %>%
  distinct() %>%
  .$HighThreshold %>%
  round_half_up(2)

# Extraordinary
covid_extraordinary_threshold <- Respiratory_Pathogens_MEM_Scot %>%
  filter(Pathogen == "Influenza") %>%
  select(ExtraordinaryThreshold) %>%
  distinct() %>%
  .$ExtraordinaryThreshold %>%
  round_half_up(2)

### National ----

altTextServer("covid_mem_modal",
              title = "Laboratory-confirmed COVID-19 incidence per 100,000 population",
              content = tags$ul(tags$li("This is a plot showing the rate of laboratory-confirmed COVID-19 infection per 100,000 population in Scotland."),
                                tags$li("The x axis shows the ISO week of sample, from week 40 to week 39. ",
                                        "Week 40 is typically the start of October and when the winter respiratory season starts."),
                                tags$li("The y axis shows the rate of laboratory-confirmed COVID-19 infection per 100,000 population."),
                                tags$li(glue("There is a trace for each of the following seasons: 2023/2024, 2024/2025, and 2025/2026.")),
                                tags$li(glue("Activity levels for COVID-19 based on MEM thresholds are represented by different coloured panels on the plot. ",
                                             "The activity levels and MEM thresholds for COVID-19 are: ",
                                             "Baseline (< ", covid_low_threshold, "), ",
                                             "Low (", covid_low_threshold, "-", covid_moderate_threshold-0.01, "), ",
                                             "Medium (", covid_moderate_threshold, "-", covid_high_threshold-0.01, "), ",
                                             "High (", covid_high_threshold, "-", covid_extraordinary_threshold-0.01, "), and ",
                                             "Very High (>= ", covid_extraordinary_threshold, ").")),
                                tags$li("By November 2023, all Community Acute Respiratory Infection (CARI) data were removed from the",
                                        "overall number of laboratory-confirmed episodes.")))



# Covid MEM table
output$covid_mem_table <- renderDataTable({
  Respiratory_Pathogens_MEM_Scot %>%
    filter(Pathogen == "COVID-19") %>%
    filter(Season >= "2023/2024") %>%
    arrange(desc(WeekEnding)) %>%
    select(Season, ISOWeek, RatePer100000, ActivityLevel) %>%
    mutate(ActivityLevel = case_when(
      ActivityLevel == "Moderate" ~ "Medium",
      ActivityLevel == "Extraordinary" ~ "Very High",
      TRUE ~ ActivityLevel
    )) %>%
    mutate(Season = factor(Season),
           ISOWeek = factor(ISOWeek),
           ActivityLevel = factor(ActivityLevel, levels = activity_levels)) %>%
    rename(`ISO Week` = ISOWeek,
           `Rate per 100,000` = RatePer100000,
           `Activity Level` = ActivityLevel) %>%
    make_table(add_separator_cols_1dp = c(3),
               filter_cols = c(1,2,4))
})

# Covid MEM plot
output$covid_mem_plot <- renderPlotly({
  Respiratory_Pathogens_MEM_Scot %>%
    filter(Pathogen == "COVID-19") %>%
    filter(Season >= "2023/2024") %>%
    mutate(ActivityLevel = case_when(
      ActivityLevel == "Moderate" ~ "Medium",
      ActivityLevel == "Extraordinary" ~ "Very High",
      TRUE ~ ActivityLevel
    )) %>%
    mutate(ActivityLevel = factor(ActivityLevel, levels = activity_levels)) %>%
    create_mem_linechart()
  
})

### HB ----

altTextServer("covid_mem_hb_modal",
              title = "Laboratory-confirmed COVID-19 incidence per 100,000 population by NHS Health Board",
              content = tags$ul(tags$li(glue("This is a plot showing the rate of laboratory-confirmed COVID-19 infection per 100,000 population by NHS Health Board for seasons ",
                                             "2024/2025 and  2025/2026.")),
                                tags$li("The x axis shows the ISO week of sample, from week 40 to week 39. ",
                                        "Week 40 is typically the start of October and when the winter respiratory season starts."),
                                tags$li("The y axis shows the NHS Health Board."),
                                tags$li("Each cell is coloured according to the activity level: Baseline, Low, Medium, High, or Very High ."),
                                tags$li("Caution should be taken when interpreting the activity levels (and MEM thresholds) for smaller NHS Health Boards. ",
                                        "The incidence rate shows greater fluctuation as a result of the lower number of samples taken relative ",
                                        "to the population size; this has the effect of generating small or large incidence rates compared to NHS Health Boards ",
                                        "with larger populations."),
                                tags$li("MEM thresholds for each NHS Health Board are available in the metadata of the",
                                        tags$a("accompanying report.",
                                               href="https://www.publichealthscotland.scot/publications/show-all-releases?id=102486")),
                                tags$li("By November 2023, all Community Acute Respiratory Infection (CARI) data were removed from the",
                                        "overall number of laboratory-confirmed episodes.")))


# Influenza MEM by HB table
output$covid_mem_hb_table <- renderDataTable({
  Respiratory_Pathogens_MEM_HB %>%
    filter(Pathogen == "COVID-19") %>%
    filter(Season >= "2023/2024") %>%
    arrange(desc(WeekEnding)) %>%
    select(Season, ISOWeek, HBName, RatePer100000, ActivityLevel) %>%
    mutate(ActivityLevel = case_when(
      ActivityLevel == "Moderate" ~ "Medium",
      ActivityLevel == "Extraordinary" ~ "Very High",
      TRUE ~ ActivityLevel
    )) %>%
    mutate(Season = factor(Season),
           ISOWeek = factor(ISOWeek),
           HBName = factor(HBName),
           ActivityLevel = factor(ActivityLevel, levels = activity_levels)) %>%
    rename(`ISO Week` = ISOWeek,
           `NHS Health Board`= HBName,
           `Rate per 100,000` = RatePer100000,
           `Activity Level` = ActivityLevel) %>%
    make_table(add_separator_cols_1dp = c(4),
               filter_cols = c(1,2,3,5))
})

# Influenza MEM by HB plot
output$covid_mem_hb_plot <- renderPlotly({
  Respiratory_Pathogens_MEM_HB %>%
    filter(Pathogen == "COVID-19") %>%
    filter(Season >= "2023/2024") %>%
    mutate(ActivityLevel = case_when(
      ActivityLevel == "Moderate" ~ "Medium",
      ActivityLevel == "Extraordinary" ~ "Very High",
      TRUE ~ ActivityLevel
    )) %>%
    mutate(ActivityLevel = factor(ActivityLevel, levels = activity_levels)) %>%
    create_mem_heatmap(breakdown_variable = "HBCode")
  
})


### Age ----

altTextServer("covid_mem_age_modal",
              title = "Laboratory-confirmed COVID-19 incidence per 100,000 population by age group",
              content = tags$ul(tags$li(glue("This is a plot showing the rate of laboratory-confirmed COVID-19 infection per 100,000 population by age group for seasons ",
                                             "2024/2025 and  2025/2026.")),
                                tags$li("The x axis shows the ISO week of sample, from week 40 to week 39. ",
                                        "Week 40 is typically the start of October and when the winter respiratory season starts."),
                                tags$li("The y axis shows the age group."),
                                tags$li("Each cell is coloured according to the activity level: Baseline, Low, Medium, High, or Very High ."),
                                tags$li("Caution should be taken when interpreting the activity levels (and MEM thresholds) for smaller age groups. ",
                                        "The incidence rate shows greater fluctuation as a result of the lower number of samples taken relative ",
                                        "to the population size; this has the effect of generating small or large incidence rates compared to age groups ",
                                        "with larger populations."),
                                tags$li("MEM thresholds for each age group are available in the metadata of the",
                                        tags$a("accompanying report.",
                                               href="https://www.publichealthscotland.scot/publications/show-all-releases?id=102486")),
                                tags$li("By November 2023, all Community Acute Respiratory Infection (CARI) data were removed from the",
                                        "overall number of laboratory-confirmed episodes.")))

# Influenza MEM by Age table
output$covid_mem_age_table <- renderDataTable({
  Respiratory_Pathogens_MEM_Age %>%
    filter(Pathogen == "COVID-19") %>%
    filter(Season >= "2023/2024") %>%
    arrange(desc(WeekEnding)) %>%
    select(Season, ISOWeek, AgeGroup, RatePer100000, ActivityLevel) %>%
    mutate(ActivityLevel = case_when(
      ActivityLevel == "Moderate" ~ "Medium",
      ActivityLevel == "Extraordinary" ~ "Very High",
      TRUE ~ ActivityLevel
    )) %>%
    mutate(Season = factor(Season),
           ISOWeek = factor(ISOWeek),
           AgeGroup = factor(AgeGroup, levels = mem_age_groups_full),
           ActivityLevel = factor(ActivityLevel, levels = activity_levels)) %>%
    rename(`ISO Week` = ISOWeek,
           `Age Group`= AgeGroup,
           `Rate per 100,000` = RatePer100000,
           `Activity Level` = ActivityLevel) %>%
    make_table(add_separator_cols_1dp = c(4),
               filter_cols = c(1,2,3,5))
})

# Influenza MEM by Age plot
output$covid_mem_age_plot <- renderPlotly({
  Respiratory_Pathogens_MEM_Age %>%
    filter(Pathogen == "COVID-19") %>%
    filter(Season >= "2023/2024") %>%
    mutate(ActivityLevel = case_when(
      ActivityLevel == "Moderate" ~ "Medium",
      ActivityLevel == "Extraordinary" ~ "Very High",
      TRUE ~ ActivityLevel
    )) %>%
    mutate(ActivityLevel = factor(ActivityLevel, levels = activity_levels)) %>%
    create_mem_heatmap(breakdown_variable = "AgeGroup")
  
})


