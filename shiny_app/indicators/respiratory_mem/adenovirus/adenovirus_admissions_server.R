## Organise adenovirus admissions data into the right format for the plot and table


adeno_admissions <- age_rate_data_all_path %>% 
  filter(age_band == "All Ages") %>% 
  add_season() %>% 
  select(week_ending, adeno, adeno_rate, Season) %>% 
  rename(Date = week_ending,
         Admissions = adeno,
         RatePer100000 = adeno_rate) %>% 
  mutate(Year = year(Date),
         ISOWeek = isoweek(Date)) %>% 
  mutate(Season = paste0(substr(Season, 1, 4), "/", substr(Season, 6, 9)),
         Weekord = case_when(ISOWeek >= 40 ~ ISOWeek - 39,
                             ISOWeek < 40 ~ ISOWeek + 13))

adeno_adm_seasons <- tail(sort(unique(adeno_admissions$Season)), 6)

## Plot descriptions

metadataButtonServer(id="respiratory_adenovirus_admissions",
                     panel="Respiratory infection activity",
                     parent = session)


altTextServer("adenovirus_admissions_modal",
              title = "Weekly rate of adenovirus hospital admissions in Scotland",
              content = tags$ul(tags$li("This is a plot showing the weekly rate of adenovirus hospital admissions in Scotland."),
                                tags$li("The x axis shows the ISO week of admission, from week 40 to week 39. ",
                                        "Week 40 is typically the start of October and when the winter respiratory season starts."),
                                tags$li("The y axis shows the rate of hospital admissions per 100,000."),
                                tags$li(glue("There is a trace for each of the following seasons from ", 
                                             adeno_adm_seasons[1], " to ", adeno_adm_seasons[6], ".")),
                                tags$li("Hospital admissions for the most recent week may be incomplete, and should be treated as provisional and interpreted with caution")))

altTextServer("adenovirus_admissions_age_modal",
              title = "Adenovirus hospital admission rate per 100,000 population by age group",
              content = tags$ul(tags$li("This is a plot showing the rate of adenovirus hospital admission per 100,000 population by age group."),
                                tags$li("The x axis is the week ending date."),
                                tags$li("The y axis shows the hospital admission rate per 100,000 population."),
                                tags$li("The plot contains a trace showing the admission rate per 100k for each of the following age groups: <1 years, 1-4 years, 5-14 years, 15-44 years, 45-64 years, 65-74 years, and 75+ years."),
                                tags$li("Each trace can be hidden/unhidden by clicking on the relevant age group from the legend on the right of the chart.")))


# Adenovirus admissions table

output$adenovirus_admissions_table <- renderDataTable({
  adeno_admissions %>%
    filter(Season %in% adeno_adm_seasons) %>%
    arrange(desc(Date)) %>%
    select(Season, ISOWeek, RatePer100000) %>%
    mutate(Season = factor(Season),
           RatePer100000 = round(RatePer100000, 1),
           ISOWeek = factor(ISOWeek)) %>%
    rename(`ISO Week` = ISOWeek,
           `Admission Rate per 100k` = RatePer100000) %>%
    make_table(filter_cols = c(1,2))
})

# Adenovirus admissions by age table
output$adenovirus_admissions_age_table <- renderDataTable({
  age_rate_data_all_path %>%
    select(week_ending, age_band,
           rate = adeno_rate) %>% 
    #mutate(week_ending = dmy(week_ending)) %>%
    filter(age_band != "All Ages") %>%
    mutate(week_ending = as_date(week_ending)) %>% 
    arrange(desc(week_ending)) %>%
    rename(`Week Ending` = week_ending,
           `Age Group` = age_band,
           `Admission Rate per 100k` = rate) %>%
    make_table(add_separator_cols_1dp = c(3),
               filter_cols = c(1,2))
})


# Adenovirus Adms plot

output$adenovirus_admissions_plot <- renderPlotly({
  adeno_admissions %>%
    
    create_pathogen_adms_linechart()
  
})

# output$adenovirus_admissions_plot <- renderPlotly({
#   all_pathogen_admissions %>%
#     select(Date, Year, ISOWeek, Weekord, Season, Admissions = adeno) %>% 
#     create_pathogen_adms_linechart()
# 
# })



# Adenovirus Adms by age plot
output$adenovirus_admissions_age_plot <- renderPlotly({
  age_rate_data_all_path %>%
    #mutate(week_ending = dmy(week_ending)) %>%
    filter(age_band != "All Ages") %>% 
    select(week_ending, age_band,
           rate = adeno_rate) %>%
    mutate(age_band = factor(age_band, levels = c("<1",  "1-4", "5-14", "15-44", "45-64",
                                                   "65-74", "75+"))) %>% 
    arrange(week_ending, age_band) %>%
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


