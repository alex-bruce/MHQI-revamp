
metadataButtonServer(id="respiratory_influenza_admissions",
                     panel="Respiratory infection activity",
                     parent = session)




altTextServer("influenza_admissions_modal",
              title = "Influenza hospital admissions in Scotland",
              content = tags$ul(tags$li("This is a plot showing the number of influenza hospital admissions in Scotland."),
                                tags$li("The x axis shows the ISO week of sample, from week 40 to week 39. ",
                                        "Week 40 is typically the start of October and when the winter respiratory season starts."),
                                tags$li("The y axis shows the number of hospital admissions."),
                                tags$li("There is a trace for each of the following season from 2016/2017 to 2022/2023")))


altTextServer("flu_los_modal",
              title = "Length of stay of acute influenza hospital admissions",
              content = tags$ul(
                tags$li("This is a plot of the distribution of lengths of stay in hospital",
                        "for acute influenza hospital admissions by respiratory season."),
                tags$li("There is a drop down above the chart which allows you to select",
                        "the respiratory season for plotting. The default is the current season."),
                tags$li("The legend shows five categories for length of stay: 1 day or less;",
                        "2-3 days, 4-5 days, 6-7 days, 8+ days. See the metadata tab for further detail."),
                tags$li("The x axis shows a break down of admissions by age groups: 0-17, 18-64, 65-74, 75+ and finally by All ages."),
                tags$li("The y axis is the percentage of admissions in a given age group category."),
                tags$li("The plot is a stacked bar chart, where the",
                        "sections of vertical bars correspond to different length of stay categories.",
                        "The bar sections are ordered from smallest length of stay to largest",
                        "length of stay from bottom to top.") ))


# Influenza admissions table
output$influenza_admissions_table <- renderDataTable({
  Influenza_admissions %>%
    filter(FluType == "Influenza A & B") %>%
    arrange(desc(Date)) %>%
    select(Season, ISOWeek, Admissions) %>%
    mutate(Season = factor(Season),
           ISOWeek = factor(ISOWeek)) %>%
    rename(`ISO Week` = ISOWeek) %>%
    make_table(filter_cols = c(1,2))
})


# Influenza Adms plot
output$influenza_admissions_plot <- renderPlotly({
  Influenza_admissions %>%
    filter(FluType == "Influenza A & B") %>%
    create_flu_adms_linechart()

})


observeEvent(input$respiratory_season,
             {
               updatePickerInput(session, inputId = "respiratory_date",
                                 choices = {Respiratory_AllData %>% filter(Season == input$respiratory_season) %>%
                                     .$Date %>% unique() %>% as.Date() %>% format("%d %b %y")},
                                 selected = {Respiratory_AllData %>% filter(Season == input$respiratory_season) %>%
                                     .$Date %>% max() %>% as.Date() %>% format("%d %b %y")})

             }
)

# HB Table
output$flu_admissions_hb_table <- renderDataTable({
  Flu_Admissions_HB_3wks %>%
    #filter(WeekEnding %in% adm_hb_dates) %>%
    mutate(WeekEnding = format(WeekEnding, format = "%d %b %y")) %>%
    pivot_wider(names_from = WeekEnding,
                values_from = TotalInfections) %>%
    mutate(HealthBoardOfTreatment = factor(HealthBoardOfTreatment,
                                levels = c("NHS Ayrshire and Arran", "NHS Borders", "NHS Dumfries and Galloway", "NHS Fife", "NHS Forth Valley", "NHS Grampian",
                                           "NHS Greater Glasgow and Clyde", "NHS Highland", "NHS Lanarkshire", "NHS Lothian", "NHS Orkney", "NHS Shetland",
                                           "NHS Tayside", "NHS Western Isles", "Golden Jubilee National Hospital","Scotland"))) %>%
    arrange(HealthBoardOfTreatment) %>%
    dplyr::rename(`Health Board of treatment` = HealthBoardOfTreatment) %>%
    make_summary_table(maxrows = 16)
})


### LENGTH OF STAY ### ----

# los plot reactive title
output$flu_los_title <- renderUI({h3(glue("Influenza length of stay by age group in Season ",
                                          input$los_season_flu))})
# Plot
output$flu_los_plot<- renderPlotly({
  flu_los_weekly_plot<-Length_of_Stay_Season %>%
    filter(admission_type == "flu") %>% 
    filter(Season == input$los_season_flu) %>%
        make_hospital_admissions_los_plot() #function in "/...../indicators/hospital_admissions/hospital_admissions_functions.R"
})
# Table
output$flu_los_table <- renderDataTable({
  flu_los_weekly_table<- Length_of_Stay_Weekly %>% 
    filter(admission_type == "flu") %>% 
    filter(Season == input$los_season_flu) %>%
    mutate(`Length of stay` = factor(LengthOfStay,
                                     levels = c("1 day or less",
                                                "2-3 days", "4-5 days",
                                                "6-7 days", "8+ days"))) %>% 
    select(Season,
           'Week ending' = AdmissionWeekEnding, 
           'Age group' = AgeGroup,
           'Length of stay',
           'Percent' = PercentageOfAdmissions) })


