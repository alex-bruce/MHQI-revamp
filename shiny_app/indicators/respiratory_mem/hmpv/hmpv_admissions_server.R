
metadataButtonServer(id="respiratory_hmpv_admissions",
                     panel="Respiratory infection activity",
                     parent = session)


altTextServer("hmpv_admissions_modal",
              title = "HMPV hospital admissions in Scotland",
              content = tags$ul(tags$li("This is a plot showing the number of HMPV hospital admissions in Scotland."),
                                tags$li("The x axis shows the ISO week of admission, from week 40 to week 39. ",
                                        "Week 40 is typically the start of October and when the winter respiratory season starts."),
                                tags$li("The y axis shows the number of hospital admissions.")))

altTextServer("hmpv_admissions_age_modal",
              title = "HMPV hospital admission rate per 100,000 population by age group",
              content = tags$ul(tags$li("This is a plot showing the rate of HMPV hospital admission per 100,000 population by age group."),
                                tags$li("The x axis is the week ending date."),
                                tags$li("The y axis shows the hospital admission rate per 100,000 population."),
                                tags$li("The plot contains a trace showing the admission rate per 100k for each of the following age groups: <1 years, 1-4 years, 5-14 years, 15-44 years, 45-64 years, 65-74 years, and 75+ years."),
                                tags$li("Each trace can be hidden/unhidden by clicking on the relevant age group from the legend on the right of the chart.")))


# HMPV admissions table
output$hmpv_admissions_table <- renderDataTable({
    all_pathogen_admissions %>%
    arrange(desc(Date)) %>%
    select(Season, ISOWeek, Admissions = hmpv) %>%
    mutate(Season = factor(Season),
           ISOWeek = factor(ISOWeek)) %>%
    rename(`ISO Week` = ISOWeek) %>%
    make_table(filter_cols = c(1,2))
})

# HMPV admissions by age table
output$hmpv_admissions_age_table <- renderDataTable({
  age_rate_data_all_path %>%
    select(week_ending, age_band,
           rate = hmpv_rate) %>% 
    filter(age_band != "All Ages") %>%
    mutate(week_ending = as_date(week_ending)) %>% 
    arrange(desc(week_ending)) %>%
    rename(`Week Ending` = week_ending,
           `Age Group` = age_band,
           `Admission Rate per 100k` = rate) %>%
    make_table(add_separator_cols_1dp = c(3),
               filter_cols = c(1,2))
})


# MPN Adms plot
output$hmpv_admissions_plot <- renderPlotly({
  all_pathogen_admissions %>%
    select(Date, Year, ISOWeek, Weekord, Season, Admissions = hmpv) %>% 
    create_pathogen_adms_linechart()

})

# MPN Adms by age plot
output$hmpv_admissions_age_plot <- renderPlotly({
  age_rate_data_all_path %>%
    filter(age_band != "All Ages") %>% 
    select(week_ending, age_band,
           rate = hmpv_rate) %>%
    mutate(age_band = factor(age_band, levels = c("<1",  "0-4", "5-14", "15-44", "45-64",
                                                   "65-74", "75+"))) %>% 
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


