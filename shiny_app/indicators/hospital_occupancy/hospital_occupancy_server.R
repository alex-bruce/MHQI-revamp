

metadataButtonServer(id="hospital_occupancy",
                     panel="COVID-19 hospital occupancy",
                     parent = session)


jumpToTabButtonServer(id="hospital_occupancy_from_summary",
                      location="hospital_occupancy",
                      parent = session)


# Hospital occupancy ----

altTextServer("hospital_occupancy_modal",
              title = "Number of patients with COVID-19 in hospital",
              content = tags$ul(
                tags$li("This is a plot of the number of patients in hospital with COVID-19."),
                tags$li("The number of patients are seven day averages taken as a snapshot each Sunday."),
                tags$li("The x axis is the week number."),
                tags$li("The y axis is the seven day average number of patients in hospital."),
                
              )
)

altTextServer("hospital_occupancy_hb_modal",
              title = "Number of patients with COVID-19 in hospital by NHS Health Board of treatment",
              content = tags$ul(
                tags$li("This is a plot of the number of patients in hospital with COVID-19 by NHS Health Board of treatment."),
                tags$li("The number of patients are seven day averages taken as a snapshot each Sunday."),
                tags$li("The x axis is the week number."),
                tags$li("The y axis is the seven day average number of patients in hospital.")
                
              )
)


altTextServer("icu_occupancy_modal",
              title = "Number of patients with COVID-19 in ICU",
              content = tags$ul(
                tags$li("This is a plot of the 7 day average of the number of",
                        "patients with COVID-19 in hospital intensive care units (ICU)."),
                tags$li("The x axis is the date, commencing 13 Sep 2020."),
                tags$li("The y axis is the 7 day average number of people in ICU."),
                tags$li("There are two traces broken down by length of stay in ICU:",
                        "one for length of stay 28 days or less (pink; trace commences 13 Sep 2020);",
                        "the other for length of stay greater than 28 days (purple; trace commences 27 Jan 2021)."),
                tags$li("Since Oct 2021 the overarching trend has been a decrease in the number of",
                        "patients with COVID-19 in ICU.")
              )
)

# 
# # Figures for last three weeks
# covid_occupancy_recent_week <- occupancy_covid %>%
#   arrange(WeekEnding) %>% 
#   mutate(pathogen = "COVID-19") %>% 
#   tail(3) %>%
#   #select(-Rate_per_100000) %>%
#   pivot_wider(names_from = pathogen,
#               values_from = SevenDayAverage) %>%
#   mutate(DateTwoWeek = .$WeekEnding[1],
#          DateLastWeek = .$WeekEnding[2],
#          DateThisWeek = .$WeekEnding[3],
#          OccupancyTwoWeek = .$`COVID-19`[1],
#          OccupancyLastWeek = .$`COVID-19`[2],
#          OccupancyThisWeek = .$`COVID-19`[3]) %>%
#   select(DateTwoWeek, DateLastWeek, DateThisWeek, OccupancyTwoWeek, OccupancyLastWeek, OccupancyThisWeek) %>%
#   head(1)



# make data table with all the hospital occupancy data in it
# the Occupancy_Weekly_Hospital_HB has two dates, an numeric 'open data' version, formatted as a number, 
# and a date-formatted WeekEnding
output$hospital_occupancy_table <- renderDataTable({
  occupancy_rapid_new %>%
    filter(Pathogen == "COVID-19") %>% 
    arrange(desc(WeekEnding)) %>% 
    mutate(ISOweek = factor(ISOweek),
           Season = factor(Season)) %>%
    select('Season' = Season,
           'Week number' = ISOweek,
           #'Number of patients in hospital as at Sunday' = bed_occupancy,
           `7 day average of number of patients in hospital as at Sunday`= SevenDayAverageInpatients) %>%
    make_table(.,
               add_separator_cols=c(3), # Column indices to add thousand separators to
               add_percentage_cols = NULL, # with % symbol and 2dp
               maxrows=10,
               order_by_firstcol="desc",
               filter_cols = c(1, 2)
    )
  
})

output$hospital_occupancy_hb_table <- renderDataTable({
  occupancy_rapid_hb_new %>%
    filter(Pathogen == "COVID-19") %>% 
    arrange(desc(WeekEnding)) %>% 
    mutate(ISOweek = factor(ISOweek),
           Season = factor(Season),
           HBName = factor(HBName)) %>%
    select('Season' = Season,
           'Week number' = ISOweek,
           'NHS Health Board' = HBName,
           `7 day average of number of patients in hospital as at Sunday`= SevenDayAverageInpatients) %>%
    make_table(.,
               add_separator_cols=c(4), # Column indices to add thousand separators to
               add_percentage_cols = NULL, # with % symbol and 2dp
               maxrows=15,
               order_by_firstcol="desc",
               filter_cols = c(1,2, 3)
    )
  
})

# make data table with all the hospital occupancy health board data in it
# HB Table (uses weekly values)
# output$hospital_occupancy_hb_table <- renderDataTable({
#   Occupancy_Weekly_Hospital_HB %>%
#     select(WeekEnding, HealthBoard=HealthBoardName, SevenDayAverage) %>%
#     filter(WeekEnding %in% adm_hb_dates) %>%
#     #mutate(Date = format(Date, format = "%d %b %y")) %>%
#     pivot_wider(names_from = WeekEnding,
#                 values_from = SevenDayAverage) %>%
#     mutate(HealthBoard = factor(HealthBoard,
#                                 levels = c("NHS Ayrshire and Arran", "NHS Borders", "NHS Dumfries and Galloway", "NHS Fife", "NHS Forth Valley", "NHS Grampian",
#                                            "NHS Greater Glasgow and Clyde", "NHS Highland", "NHS Lanarkshire", "NHS Lothian", "NHS Orkney", "NHS Shetland",
#                                            "NHS Tayside", "NHS Western Isles", "Golden Jubilee National Hospital", "Scotland"))) %>%
#     arrange(HealthBoard) %>%
#     dplyr::rename(`Health Board of treatment` = HealthBoard) %>%
#     make_summary_table(maxrows = 16)
# })

# make data table with all the ICU occupancy data in it
# output$ICU_occupancy_table <- renderDataTable({
#   Occupancy_ICU %>%
#     mutate(Date = convert_opendata_date(Date),
#            ICULengthOfStay = factor(ICULengthOfStay)) %>%
#     filter(Date <= floor_date(today(), "week")) %>%
#     dplyr::rename(`ICU occupancy` = ICUOccupancy,
#     `ICU length of stay` = ICULengthOfStay,
#     `7 day average` = SevenDayAverage) %>%
#     select(Date, `ICU length of stay`, `ICU occupancy`, `7 day average`) %>%
#     arrange(desc(Date)) %>%
#     make_table(.,
#                 add_separator_cols=NULL, # Column indices to add thousand separators to
#                 add_percentage_cols = NULL, # with % symbol and 2dp
#                 maxrows=10,
#                 order_by_firstcol="desc",
#                 filter_cols = 2)
# 
# })

output$hospital_occupancy_plot <- renderPlotly({
  occupancy_rapid_new %>%
    filter(Pathogen == "COVID-19") %>%
    create_pathogen_occupancy_linechart()
  
})

output$hospital_occupancy_hb_plot <- renderPlotly({
  occupancy_rapid_hb_new %>%
    filter(Pathogen == "COVID-19") %>%
    filter(Season %in% input$hospital_occupancy_selected_seasons) %>%
    create_pathogen_occupancy_hb_linechart()

})

# output$icu_occupancy_plot <- renderPlotly({
# 
#   make_occupancy_plots(Occupancy_ICU, occupancy = "icu")
# 
# })





