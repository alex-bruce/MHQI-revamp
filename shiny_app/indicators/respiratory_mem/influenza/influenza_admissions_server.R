
metadataButtonServer(id="respiratory_influenza_admissions",
                     panel="Respiratory infection activity",
                     parent = session)


# Get recent seasons
flu_adm_seasons <- tail(sort(unique(Influenza_admissions$Season)), 6)


altTextServer("influenza_admissions_modal",
              title = "Weekly rate of influenza hospital admissions in Scotland",
              content = tags$ul(tags$li("This is a plot showing the weekly rate of influenza hospital admissions in Scotland."),
                                tags$li("The x axis shows the ISO week of admission, from week 40 to week 39. ",
                                        "Week 40 is typically the start of October and when the winter respiratory season starts."),
                                tags$li("The y axis shows the rate of hospital admissions per 100,000."),
                                tags$li(glue("There is a trace for each of the following seasons from ", 
                                             flu_adm_seasons[1], " to ", flu_adm_seasons[6], ".")),
                                tags$li("Hospital admissions for the most recent week may be incomplete, and should be treated as provisional and interpreted with caution")
              )
)

altTextServer("influenza_admissions_age_modal",
              title = "Influenza hospital admission rate per 100,000 population by age group",
              content = tags$ul(tags$li("This is a plot showing the rate of influenza hospital admission per 100,000 population by age group."),
                                tags$li("The x axis is the week ending date."),
                                tags$li("The y axis shows the hospital admission rate per 100,000 population."),
                                tags$li("The plot contains a trace showing the admission rate per 100k for each of the following age groups: <1 years, 1-4 years, 5-14 years, 15-44 years, 45-64 years, 65-74 years, and 75+ years."),
                                tags$li("Each trace can be hidden/unhidden by clicking on the relevant age group from the legend on the right of the chart.")))


altTextServer("flu_adm_age_sex",
              title = glue("Acute influenza admissions by age and sex in Scotland"),
              content = tags$ul(
                tags$li(glue("This is a pyramid plot of rate per 100,000 people of acute influenza hospital admissions in Scotland by age and sex.")),
                tags$li("The information is displayed for a selected  winter respiratory season.",
                        "Respiratory seasons start at ISO week 40 and finish at ISO week 39."),
                tags$li("Weekly rate data for age and sex on a weekly basis are available in ",
                        "the PHS Open Data platform ",
                        tags$a(href="https://www.opendata.nhs.scot/dataset/viral-respiratory-diseases-including-influenza-and-covid-19-data-in-scotland",
                               "Viral Respiratory Diseases (Including Influenza and COVID-19) Data in Scotland page (external website).",
                               target="_blank")),
                tags$li("The y axis shows the age group. The left side of the y axis corresponds to females (F) and the right side to males (M)."),
                tags$li("For the x axis the plot shows rate per 100,000 people.")
                # tags$li("The youngest and oldest groups have the highest rates of illness.")
              )
)



altTextServer("flu_los_modal",
              title = "Length of stay of acute influenza hospital admissions",
              content = tags$ul(
                tags$li("This is a plot of the distribution of lengths of stay in hospital",
                        "for acute influenza hospital admissions by respiratory season.",
                        "The information is displayed for a selected  winter respiratory season.",
                        "Respiratory seasons start at ISO week 40 and finish at ISO week 390"),
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

altTextServer("influenza_admissions_simd_modal",
              title = "Influenza hospital admission rate per 100,000 population by deprivation category (SIMD)",
              content = tags$ul(tags$li("This is a plot showing the rate of influenza hospital admission per 100,000 population by SIMD deprivation category."),
                                tags$li("SIMD is a relative measure of deprivation across small areas in Scotland.",
                                        "There are equal numbers of data zones in each of the five categories.",
                                        "SIMD 1 contains the 20% most deprived zones and SIMD 5 contains the 20%",
                                        "least deprived zones. See the",
                                        tags$a("Scottish government website (external link)",
                                               href="https://www.gov.scot/collections/scottish-index-of-multiple-deprivation-2020/"),
                                        "for more information."),
                                tags$li("The x axis is the week ending, starting 03 Jan 2021."),
                                tags$li("The y axis shows the hospital admission rate per 100,000 population."),
                                tags$li("The plot contains a trace for each of the SIMD categories. SIMD 1 is",
                                        "highlighted in red and SIMD 5 in blue. The other categories are in grey."),
                                # tags$li("There have been several peaks throughout the pandemic, notably in",
                                #         "Apr 2020, Oct 2020, Jan 2021, Jul 2021, Sep 2021,",
                                #         "Jan 2022, Mar 2022, Jun 2022, Jan 2023 and Mar 2023.")
              )
)

# Influenza admissions table
output$influenza_admissions_table <- renderDataTable({
  Influenza_admissions %>%
    filter(FluType == "Influenza A & B") %>%
    filter(Season %in% flu_adm_seasons) %>%
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


# Influenza Adms plot
output$influenza_admissions_plot <- renderPlotly({
  Influenza_admissions %>%
    filter(FluType == "Influenza A & B") %>%
    create_pathogen_adms_linechart()

})

# Influenza admissions by age table
output$influenza_admissions_age_table <- renderDataTable({
  age_rate_data_all_path %>%
    select(week_ending, age_band,
           rate = flu_rate) %>% 
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


# Influenza Adms by age plot
output$influenza_admissions_age_plot <- renderPlotly({
  age_rate_data_all_path %>%
    #mutate(week_ending = dmy(week_ending)) %>%
    filter(age_band != "All Ages") %>% 
    select(week_ending, age_band,
           rate = flu_rate) %>%
    mutate(age_band = factor(age_band, levels = c("<1",  "1-4", "5-14", "15-44", "45-64",
                                                  "65-74", "75+"))) %>% 
    arrange(week_ending, age_band) %>%
    create_pathogen_adms_age_linechart()
  
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

### WEEKLY ADMISSIONS BY SIMD ### ----


### Modal links
observeEvent(input$btn_modal_simd, { showModal(simd_modal) })

# Table
output$influenza_admissions_simd_table <- renderDataTable({
  admissions_simd_Cov_flu_RSV %>% 
    filter(Pathogen == "Influenza (All)") %>%
    arrange(desc(WeekEnding)) %>%
    mutate(WeekEnding = convert_opendata_date(WeekEnding),
           SIMD = factor(SIMD),
           ProvisionalFlag = factor(recode(ProvisionalFlag, "1" = "p", "0" = ""))) %>%
    select(WeekEnding, SIMD, NumberOfAdmissions, RateOfAdmissions, ProvisionalFlag) %>%
    dplyr::rename(`Week ending` = WeekEnding,
                  `Number of admissions` = NumberOfAdmissions,
                  `Admission Rate per 100k` = RateOfAdmissions,
                  `Is data provisional (p)?` = ProvisionalFlag) %>%
    make_table(add_separator_cols = c(3),
               filter_cols = c(2,5))
})



# Plot
output$influenza_admissions_simd_plot <- renderPlotly({
  admissions_simd_Cov_flu_RSV %>% 
    filter(Pathogen == "Influenza (All)") %>%
    make_hospital_admissions_simd_plot()
  
})

#-----------------------#
#### Flu adm pyramid ####
#-----------------------#
# 
# output$flu_adm_pyr_title <- renderUI({h3(glue("Acute influenza hospital admissions by age and sex in Scotland; ",
#                                               input$flu_age_sex_adm_season))})
# 
# # pyramid plot that shows the breakdown by age and sex
# output$flu_adm_age_sex_pyramid_plot = renderPlotly({
#   Admissions_AgeSex_Season %>%
#     filter(Pathogen == "flu",
#            Sex %in% c("M", "F"),
#            Season == input$flu_age_sex_adm_season) %>%
#     make_age_sex_adm_pyramid_plot # hospital_admissions_functions  
# })
# 
# 
# output$flu_adm_age_sex_pyramid_table = renderDataTable({
#   
#   flu_adm_age_sex_pyramid_table <- Admissions_AgeSex_Season %>%
#     filter(Pathogen == "flu",
#            Season == input$flu_age_sex_adm_season) %>%
#     select(Season, AgeGroup, Sex, Rate) %>%
#     mutate(Season = factor(Season)) %>%
#     arrange(desc(Season), AgeGroup, Sex) %>%
#     dplyr::rename("Season" = "Season",
#                   "Age group" = "AgeGroup",
#                   "Rate per 100,000" = "Rate") %>%
#     mutate(Sex = factor(Sex, levels = c("All", "F", "M")),
#            `Age group` = factor(`Age group`, levels =
#                                   c("All","Under 18","18-64","65-74","75+"))) %>%
#     arrange(desc(`Season`), `Age group`, Sex) %>%
#     make_table(add_separator_cols_1dp = c(4),
#                filter_cols = c(1,2,3))
#   
# })
# 
# ### LENGTH OF STAY ### ----
# 
# # los plot reactive title
# output$flu_los_title <- renderUI({h3(glue("Influenza length of stay by age group in Season ",
#                                           input$los_season_flu))})
# # Plot
# output$flu_los_plot<- renderPlotly({
#   flu_los_weekly_plot<-Length_of_Stay_Season %>%
#     filter(admission_type == "flu") %>% 
#     filter(Season == input$los_season_flu) %>%
#         make_hospital_admissions_los_plot() #function in "/...../indicators/hospital_admissions/hospital_admissions_functions.R"
# })
# # Table
# output$flu_los_table <- renderDataTable({
#   flu_los_weekly_table<- Length_of_Stay_Weekly %>% 
#     filter(admission_type == "flu") %>% 
#     filter(Season == input$los_season_flu) %>%
#     mutate(`Length of stay` = factor(LengthOfStay,
#                                      levels = c("1 day or less",
#                                                 "2-3 days", "4-5 days",
#                                                 "6-7 days", "8+ days"))) %>% 
#     select(Season,
#            'Week ending' = AdmissionWeekEnding, 
#            'Age group' = AgeGroup,
#            'Length of stay',
#            'Percent' = PercentageOfAdmissions) })


