
metadataButtonServer(id="cases",
                     panel="COVID-19 cases",
                     parent = session)

jumpToTabButtonServer(id="cases_from_summary",
                      location="cases",
                      parent = session)

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
title = "Reported COVID-19 cases per 100,0000 people",
content = tags$ul(tags$li("This is a plot shows the past 3 seasons of COVID-19 cases per 100,000 people."),
 tags$li("The x axis is the week number"),
tags$li("The 3 lines show a separate season respectively. The legend indicates which line corresponds to which season.")
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
