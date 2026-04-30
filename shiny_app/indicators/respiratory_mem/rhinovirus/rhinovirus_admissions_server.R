## Organise rhinovirus admissions data into the right format for the plot and table


# rhino_admissions <- age_rate_data_all_path %>% 
#   filter(age_band == "All Ages") %>% 
#   add_season() %>% 
#   select(week_ending, rhino, rhino_rate, Season) %>% 
#   rename(Date = week_ending,
#          Admissions = rhino,
#          RatePer100000 = rhino_rate) %>% 
#   mutate(Year = year(Date),
#          ISOWeek = isoweek(Date)) %>% 
#   mutate(Season = paste0(substr(Season, 1, 4), "/", substr(Season, 6, 9)),
#          Weekord = case_when(ISOWeek >= 40 ~ ISOWeek - 39,
#                              ISOWeek < 40 ~ ISOWeek + 13))

rhino_admissions <- admissions_scotland_TEST %>% 
  filter(Pathogen == "Rhinovirus") %>% 
  mutate(ISOweek = as.numeric(ISOweek)) %>% 
  mutate(Weekord = case_when(ISOweek >= 40 ~ ISOweek - 39,
                             ISOweek < 40 ~ ISOweek + 13)) %>% 
  rename(Date = WeekEnding,
         Admissions = NumberAdmissionsPerWeek,
         RatePer100000 = RateAdmissionsPerWeek,
         Year = ISOyear,
         ISOWeek = ISOweek)

rhino_adm_seasons <- tail(sort(unique(rhino_admissions$Season)), 6)

## Plot descriptions

metadataButtonServer(id="respiratory_rhino_admissions",
                     panel="Respiratory infection activity",
                     parent = session)


altTextServer("rhino_admissions_modal",
              title = "Weekly rate of rhinovirus hospital admissions in Scotland",
              content = tags$ul(tags$li("This is a plot showing the weekly rate of rhinovirus hospital admissions in Scotland."),
                                tags$li("The x axis shows the ISO week of admission, from week 40 to week 39. ",
                                        "Week 40 is typically the start of October and when the winter respiratory season starts."),
                                tags$li("The y axis shows the rate of hospital admissions per 100,000."),
                                tags$li(glue("There is a trace for each of the following seasons from ", 
                                             rhino_adm_seasons[1], " to ", rhino_adm_seasons[6], ".")),
                                tags$li("Hospital admissions for the most recent week may be incomplete, and should be treated as provisional and interpreted with caution")))

altTextServer("rhino_admissions_age_modal",
              title = "Rhinovirus hospital admission rate per 100,000 population by age group",
              content = tags$ul(tags$li("This is a plot showing the rate of rhinovirus hospital admission per 100,000 population by age group."),
                                tags$li("The x axis shows the ISO week of admission, from week 40 to week 39. ",
                                        "Week 40 is typically the start of October and when the winter respiratory season starts."),                                 tags$li("The y axis shows the hospital admission rate per 100,000 population."),
                                tags$li("By default, the plot contains a trace showing the admission rate per 100,000 across all age groups."),
                                tags$li("Traces can be added for each of the following age groups: <1 years, 1-4 years, 5-14 years, 15-44 years, 45-64 years, 65-74 years, and 75+ years."),                                tags$li("Each trace can be hidden/unhidden by clicking on the relevant age group from the legend on the right of the chart.")))


# rhinovirus admissions table
output$rhino_admissions_table <- renderDataTable({
  rhino_admissions %>%
    filter(Season %in% rhino_adm_seasons) %>%
    arrange(desc(Date)) %>%
    select(Season, ISOWeek, Admissions, RatePer100000) %>%
    mutate(Season = factor(Season),
           RatePer100000 = round(RatePer100000, 1),
           ISOWeek = factor(ISOWeek)) %>%
    rename(`ISO Week` = ISOWeek,
           `Number of Admissions` = Admissions,
           `Admission Rate per 100k` = RatePer100000) %>%
    make_table(filter_cols = c(1,2))
})

# rhinovirus admissions by age table
output$rhino_admissions_age_table <- renderDataTable({
  age_rate_data_all_path %>%
    add_season() %>% 
    select(week_ending, age_band, Season,
           Admissions = rhino, rate = rhino_rate) %>% 
    mutate(Season = paste0(substr(Season, 1, 4), "/", substr(Season, 6, 9))) %>% 
    filter(Season %in% rhino_adm_seasons) %>% 
    make_admissions_age_table()
  
})


# rhinovirus Adms plot

output$rhino_admissions_plot <- renderPlotly({
  rhino_admissions %>%
    
    create_pathogen_adms_linechart()
  
})

# rhinovirus Adms by age plot
output$rhino_admissions_age_plot <- renderPlotly({
  age_rate_data_all_path %>%
    add_season() %>%    
    mutate(Season = paste0(substr(Season, 1, 4), "/", substr(Season, 6, 9))) %>% 
    filter(Season %in% rhino_adm_seasons) %>% 
    #mutate(week_ending = dmy(week_ending)) %>%
    #filter(age_band != "All Ages") %>% 
    select(week_ending, age_band,
           rate = rhino_rate, Season) %>%
    mutate(age_band = factor(age_band, levels = c("<1",  "1-4", "5-14", "15-44", "45-64",
                                                  "65-74", "75+", "All Ages"))) %>% 
    arrange(week_ending, age_band) %>%
    mutate(week = isoweek(week_ending)) %>% 
    filter(Season == input$adm_season_rhino_age) %>%
    #filter(Season == "2024/2025") %>% 
    create_pathogen_adms_age_linechart()
  
})

# observeEvent(input$respiratory_season,
#              {
#                updatePickerInput(session, inputId = "respiratory_date",
#                                  choices = {Respiratory_AllData %>% filter(Season == input$respiratory_season) %>%
#                                      .$Date %>% unique() %>% as.Date() %>% format("%d %b %y")},
#                                  selected = {Respiratory_AllData %>% filter(Season == input$respiratory_season) %>%
#                                      .$Date %>% max() %>% as.Date() %>% format("%d %b %y")})
# 
#              }


