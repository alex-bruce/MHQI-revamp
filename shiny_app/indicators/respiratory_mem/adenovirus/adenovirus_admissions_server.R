
metadataButtonServer(id="respiratory_adenovirus_admissions",
                     panel="Respiratory infection activity",
                     parent = session)


altTextServer("adenovirus_admissions_modal",
              title = "Adenovirus hospital admissions in Scotland",
              content = tags$ul(tags$li("This is a plot showing the number of adenovirus hospital admissions in Scotland."),
                                tags$li("The x axis shows the ISO week of admission, from week 40 to week 39. ",
                                        "Week 40 is typically the start of October and when the winter respiratory season starts."),
                                tags$li("The y axis shows the number of hospital admissions."),
                                tags$li("There is a trace for each of the following season from 2016/2017 to 2022/2023")))

altTextServer("adenovirus_admissions_age_modal",
              title = "Adenovirus hospital admission rate per 100,000 population by age group",
              content = tags$ul(tags$li("This is a plot showing the rate of adenovirus hospital admission per 100,000 population by age group."),
                                tags$li("The x axis shows the ISO week of amission, from week 40 to week 39. ",
                                        "Week 40 is typically the start of October and when the winter respiratory season starts."),
                                tags$li("The y axis shows the hospital admission rate per 100,000 population.")))


# Adenovirus admissions table
output$adenovirus_admissions_table <- renderDataTable({
  all_pathogen_admissions %>%
    arrange(desc(Date)) %>%
    select(Season, ISOWeek, Admissions = adeno) %>%
    mutate(Season = factor(Season),
           ISOWeek = factor(ISOWeek)) %>%
    rename(`ISO Week` = ISOWeek) %>%
    make_table(filter_cols = c(1,2))
})

# Adenovirus admissions by age table
output$adenovirus_admissions_age_table <- renderDataTable({
  age_rate_data_all_path %>%
    select(week_ending, age_band,
           rate = adeno_rate) %>% 
    filter(age_band != "All Ages") %>% 
    arrange(desc(week_ending)) %>%
    rename(`Week Ending` = week_ending,
           `Age Group` = age_band,
           `Rate per 100k` = rate) %>%
    make_table(filter_cols = c(1,2))
})


# Adenovirus Adms plot
output$adenovirus_admissions_plot <- renderPlotly({
  all_pathogen_admissions %>%
    select(Date, Year, ISOWeek, Weekord, Season, Admissions = adeno) %>% 
    create_pathogen_adms_linechart()

})

# # Adenovirus Adms by age plot
# output$adenovirus_admissions_age_plot <- renderPlotly({
#   age_rate_data_all_path %>%
#     select(Date, Year, ISOWeek, Weekord, Season, Admissions = adeno) %>% 
#     create_pathogen_adms_linechart()
#   
# })

# observeEvent(input$respiratory_season,
#              {
#                updatePickerInput(session, inputId = "respiratory_date",
#                                  choices = {Respiratory_AllData %>% filter(Season == input$respiratory_season) %>%
#                                      .$Date %>% unique() %>% as.Date() %>% format("%d %b %y")},
#                                  selected = {Respiratory_AllData %>% filter(Season == input$respiratory_season) %>%
#                                      .$Date %>% max() %>% as.Date() %>% format("%d %b %y")})
# 
#              }
)

