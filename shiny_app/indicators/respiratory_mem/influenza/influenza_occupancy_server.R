

metadataButtonServer(id="hospital_occupancy",
                     panel="Influenza hospital occupancy",
                     parent = session)


jumpToTabButtonServer(id="hospital_occupancy_from_summary",
                      location="hospital_occupancy",
                      parent = session)


# Hospital occupancy ----

altTextServer("influenza_occupancy_modal",
              title = "Number of patients with influenza in hospital",
              content = tags$ul(
                tags$li("This is a plot of the number of patients in hospital with influenza."),
                tags$li("The number of patients are seven day averages taken as a snapshot each Sunday."),
                tags$li("The x axis is the week ending date."),
                tags$li("The y axis is the average number of people in hospital.")))
              
altTextServer("influenza_occupancy_hb_modal",
              title = "Number of patients with influenza in hospital by Health Board",
              content = tags$ul(
                tags$li("This is a plot of the number of patients in hospital with influenza by Health Board."),
                tags$li("The number of patients are seven day averages taken as a snapshot each Sunday."),
                tags$li("The x axis is the week ending date."),
                tags$li("The y axis is the average number of people in hospital.")))


# make data table with all the hospital occupancy data in it
output$influenza_occupancy_table <- renderDataTable({
  occupancy_rapid %>%
    filter(pathogen == "Influenza") %>% 
    arrange(desc(Date)) %>% 
    select('Season' = Season,
           'Week ending' = Date,
           'Number of patients in hospital' = bed_occupancy,
           `7 day average`= sevenday_ave_inpatients) %>%
    make_table(.,
                add_separator_cols=NULL, # Column indices to add thousand separators to
                add_percentage_cols = NULL, # with % symbol and 2dp
                maxrows=10,
                order_by_firstcol="desc",
                filter_cols = c(1,2, 3)
               )

})

output$influenza_occupancy_hb_table <- renderDataTable({
  occupancy_rapid_hb %>%
    filter(pathogen == "Influenza") %>% 
    filter(health_board != "Golden Jubilee National Hospital") %>%
    filter(Season %in% input$influenza_occupancy_selected_seasons) %>%
    arrange(desc(Date)) %>% 
    select('Season' = Season,
           'Week' = week,
           'Health board' = health_board,
           `7 day average of number of patients in hospital`= sevenday_ave_inpatients) %>%
    make_table(.,
               add_separator_cols=NULL, # Column indices to add thousand separators to
               add_percentage_cols = NULL, # with % symbol and 2dp
               maxrows=15,
               order_by_firstcol="desc",
               filter_cols = c(1,2, 3)
    )
  
})




output$influenza_occupancy_plot <- renderPlotly({
  occupancy_rapid %>%
    filter(pathogen == "Influenza") %>%
    filter(Season %in% tail(sort(unique(occupancy_rapid_hb$Season)), 3)) %>%
    create_pathogen_occupancy_linechart()
  
})

output$influenza_occupancy_hb_plot <- renderPlotly({
  occupancy_rapid_hb %>%
    filter(pathogen == "Influenza") %>%
    filter(health_board != "Golden Jubilee National Hospital") %>%
    filter(Season %in% input$influenza_occupancy_selected_seasons) %>%
    create_pathogen_occupancy_hb_linechart()
  
})






