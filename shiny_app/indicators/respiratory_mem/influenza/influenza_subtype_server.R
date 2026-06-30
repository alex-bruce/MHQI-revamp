
# Alt text ----
altTextServer("respiratory_over_time_modal",
              title = glue("Laboratory-confirmed influenza cases by subtype"),
              content = tags$ul(
                tags$li(glue("This is a stacked bar chart plot of laboratory-confirmed influenza cases",
                             " by subtype within a given NHS Health Board",
                             " for a selected respiratory season.")),
                tags$li("The x axis shows the ISO week of sample, from week 40 to week 39. ",
                        "Week 40 is typically the start of October and when the winter respiratory season starts."),
                tags$li(glue("The laboratory-confirmed cases are presented as a rate, i.e. the number of people with ",
                        "influenza for every 100,000 people in that NHS Health Board.")),
                tags$li("For Scotland there is an option to view the absolute number of cases."),
                tags$li("The y axis is either the rate of cases or the number of cases."),
              )
)

altTextServer("respiratory_by_season_modal",
              title =  glue("Laboratory-confirmed influenza cases over time by season"),
              content = tags$ul(
                tags$li(glue("This is a plot of the laboratory-confirmed influenza cases for a given subtype",
                             " over each season.")),
                tags$li(glue("There is a trace for each season, starting in ", recent_six_seasons[1], ".")),
                tags$li("The x axis is the isoweek. Week 40 is typically the start of October and when the winter respiratory season starts"),
                tags$li(glue("The y axis is the rate of laboratory-confirmed cases of the chosen influenza subtype in a given NHS health board.")),
                tags$li("For Scotland there is an option to view the absolute number of cases."))
)


# Headline figures ----
output$respiratory_headline_figures_subtype_count <- renderValueBox ({

  organism_summary_total <- Flu_Subtype_Cases %>% 
    arrange(WeekEnding) %>% 
    filter(Pathogen == input$respiratory_headline_subtype,
           HBName == "Scotland") %>% 
    tail(1) %>%
    .$Count %>% format(big.mark=",")
  

  valueBox(value = organism_summary_total,
           subtitle = glue("laboratory-confirmed cases of {input$respiratory_headline_subtype} in Scotland"),
           color = "navy",
           icon = icon_no_warning_fn("virus"),
           width = NULL)

})

output$respiratory_headline_figures_healthboard_count <- renderValueBox ({

  organism_summary_total <- 
    Flu_Subtype_Cases %>% 
    filter(HBName == input$respiratory_headline_healthboard) %>%
    filter(Pathogen == input$respiratory_headline_subtype) %>%
    tail(1) %>%
    .$RatePer100000 %>%
    format(big.mark=",")

  valueBox(value = organism_summary_total,
           subtitle = glue("{input$respiratory_headline_subtype} laboratory-confirmed cases per 100,000 people in {input$respiratory_headline_healthboard}"),
           color = "navy",
           icon = icon_no_warning_fn("house-medical"),
           width = NULL)

})


# Plots ----
# make trend over time plot.
# Plot shows the rate/number of cases by subtype over time for the whole dataset.

output$respiratory_over_time_plot <- renderPlotly({

  Flu_Subtype_Cases %>% 
    filter(Pathogen != "Type A or B" & Pathogen != "Type A (any subtype)") %>% 
    filter(Season== input$respiratory_select_season,
           HBName == input$respiratory_select_healthboard) %>%
    group_by(WeekEnding) %>% 
    mutate(total_cases  = sum(Count)) %>% 
    ungroup() %>% 
    select_y_axis(., yaxis = input$respiratory_y_axis_plots) %>%
    arrange(WeekEnding) %>%
    make_respiratory_trend_over_time_plot(., y_axis_title = input$respiratory_y_axis_plots) # respiratory functions

})


output$respiratory_over_time_title <- renderUI({h3(glue("Laboratory-confirmed influenza cases by subtype in ",
                                                        input$respiratory_select_healthboard, " in Season ",
                                                        input$respiratory_select_season))})

# plot showing the number/rate of flu cases by season. Can filter by organism selected by the user
output$respiratory_by_season_plot = renderPlotly({

  Flu_Subtype_Cases %>%
    #filter(FluOrNonFlu == "flu") %>%
    filter(Season %in% recent_six_seasons) %>%
    select_y_axis(., yaxis = input$respiratory_y_axis_plots) %>%
    filter(Pathogen == input$respiratory_select_subtype,
           HBName == input$respiratory_select_healthboard) %>%
    make_respiratory_trend_by_season_plot_function(., y_axis_title = input$respiratory_y_axis_plots)# respiratory functions
  
})


output$respiratory_by_season_title <- renderUI({h3(glue("Laboratory-confirmed influenza cases over time by season in ",
                                                        input$respiratory_select_healthboard))})


# Data tables ----

output$respiratory_over_time_table <- renderDataTable ({

  Flu_Subtype_Cases %>% 
    filter(HBName == input$respiratory_select_healthboard) %>% 
    filter(Pathogen != "Type A or B" & Pathogen != "Type A (any subtype)") %>% 
    select(Season, ISOWeek, HBName, Pathogen, Count, RatePer100000) %>% 
    mutate(ISOWeek = factor(ISOWeek, levels=c(40:53, 1:39))) %>% 
    arrange(desc(Season), desc(ISOWeek), Pathogen, HBName) %>% 
    rename(`ISO week` = ISOWeek,
           "NHS Health Board" = HBName,
           "Number of cases" = Count,
           "Subtype" = Pathogen,
           "Rate per 100,000" = RatePer100000) %>%
    make_table(filter_cols = c(1,2,3,4),
               add_separator_cols = c(5),
               add_separator_cols_1dp = c(6),
               maxrows = 12)
  
})

# Flu by season table
output$respiratory_by_season_table <- renderDataTable ({

  Flu_Subtype_Cases %>% 
    #filter(Pathogen != "Type A or B" & Pathogen != "Type A (any subtype)") %>% 
    filter(HBName == input$respiratory_select_healthboard) %>% 
    select(Season, ISOWeek, HBName, Pathogen, Count, RatePer100000) %>% 
    mutate(ISOWeek = factor(ISOWeek, levels=c(40:53, 1:39))) %>% 
    arrange(desc(Season), desc(ISOWeek), Pathogen) %>% 
    rename(`ISO week` = ISOWeek,
           "Number of cases" = Count,
           "Subtype" = Pathogen,
           "Rate per 100,000" = RatePer100000) %>%
    make_table(filter_cols = c(1,2,3, 4),
               add_separator_cols = c(5),
               add_separator_cols_1dp = c(6),
               maxrows = 12)  

})



# Update dataset choices based off indicator choice
observeEvent(input$respiratory_select_healthboard,
             {

               if(input$respiratory_select_healthboard == "Scotland"){

                 updatePickerInput(session, inputId = "respiratory_y_axis_plots",
                                   choices = c("Number of cases", "Rate per 100,000"),
                                   selected = "Number of cases"
                 )

               } else {

                 updatePickerInput(session, inputId = "respiratory_y_axis_plots",
                                   choices = c("Rate per 100,000"),
                                   selected = "Rate per 100,000"

                 )

               }

             }
)
