
metadataButtonServer(id="respiratory_para_admissions",
                     panel="Respiratory infection activity",
                     parent = session)


altTextServer("para_admissions_modal",
              title = "Parainfluenza hospital admissions in Scotland",
              content = tags$ul(tags$li("This is a plot showing the number of parainfluenza hospital admissions in Scotland."),
                                tags$li("The x axis shows the ISO week of admission, from week 40 to week 39. ",
                                        "Week 40 is typically the start of October and when the winter respiratory season starts."),
                                tags$li("The y axis shows the number of hospital admissions.")))

altTextServer("para_admissions_age_modal",
              title = "Parainfluenza hospital admission rate per 100,000 population by age group",
              content = tags$ul(tags$li("This is a plot showing the rate of parainfluenza hospital admission per 100,000 population by age group."),
                                tags$li("The x axis is the week ending date."),
                                tags$li("The y axis shows the hospital admission rate per 100,000 population."),
                                tags$li("The plot contains a trace showing the admission rate per 100k for each of the following age groups: <1 years, 1-4 years, 5-14 years, 15-44 years, 45-64 years, 65-74 years, and 75+ years."),
                                tags$li("Each trace can be hidden/unhidden by clicking on the relevant age group from the legend on the right of the chart.")))


# parainfluenza admissions table
output$para_admissions_table <- renderDataTable({
    all_pathogen_admissions %>%
    arrange(desc(Date)) %>%
    select(Season, ISOWeek, Admissions = para) %>%
    mutate(Season = factor(Season),
           ISOWeek = factor(ISOWeek)) %>%
    rename(`ISO Week` = ISOWeek) %>%
    make_table(filter_cols = c(1,2))
})

# parainfluenza admissions by age table
output$para_admissions_age_table <- renderDataTable({
  age_rate_data_all_path %>%
    select(week_ending, age_band,
           rate = para_rate) %>% 
    mutate(week_ending = dmy(week_ending)) %>%
    filter(age_band != "All Ages") %>%
    mutate(week_ending = as_date(week_ending)) %>% 
    arrange(desc(week_ending)) %>%
    rename(`Week Ending` = week_ending,
           `Age Group` = age_band,
           `Admission Rate per 100k` = rate) %>%
    make_table(add_separator_cols_1dp = c(3),
               filter_cols = c(1,2))
})


# parainfluenza Adms plot
output$para_admissions_plot <- renderPlotly({
  all_pathogen_admissions %>%
    select(Date, Year, ISOWeek, Weekord, Season, Admissions = para) %>% 
    create_pathogen_adms_linechart()

})

# parainfluenza Adms by age plot
output$para_admissions_age_plot <- renderPlotly({
  age_rate_data_all_path %>%
    mutate(week_ending = dmy(week_ending)) %>%
    filter(age_band != "All Ages") %>% 
    select(week_ending, age_band,
           rate = para_rate) %>%
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


