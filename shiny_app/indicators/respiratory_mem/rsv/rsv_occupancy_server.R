

metadataButtonServer(id="hospital_occupancy",
                     panel="RSV hospital occupancy",
                     parent = session)


jumpToTabButtonServer(id="hospital_occupancy_from_summary",
                      location="hospital_occupancy",
                      parent = session)


# Hospital occupancy ----

altTextServer("rsv_occupancy_modal",
              title = "Number of patients with RSV in hospital",
              content = tags$ul(
                tags$li("This is a plot of the number of patients in hospital with RSV."),
                tags$li("The number of patients are seven day averages taken as a snapshot each Sunday."),
                tags$li("The x axis is the week ending date."),
                tags$li("The y axis is the average number of people in hospital.")))
              

# make data table with all the hospital occupancy data in it
output$rsv_occupancy_table <- renderDataTable({
  occupancy_rapid %>%
    filter(pathogen == "RSV") %>% 
    arrange(desc(Date)) %>% 
    select('Week ending' = Date,
           'Number of patients in hospital' = bed_occupancy,
           `7 day average`= sevenday_ave_inpatients) %>%
    make_table(.,
                add_separator_cols=NULL, # Column indices to add thousand separators to
                add_percentage_cols = NULL, # with % symbol and 2dp
                maxrows=10,
                order_by_firstcol="desc"
               )

})

output$rsv_occupancy_hb_table <- renderDataTable({
  occupancy_rapid_hb %>%
    filter(pathogen == "RSV") %>% 
    # mutate(Date = as_date("dd/mm/yyyy")) %>% 
    arrange(desc(Date)) %>% 
    select('Week ending' = Date,
           'Health board' = health_board,
           'Number of patients in hospital' = bed_occupancy,
           `7 day average`= sevenday_ave_inpatients) %>%
    make_table(.,
               add_separator_cols=NULL, # Column indices to add thousand separators to
               add_percentage_cols = NULL, # with % symbol and 2dp
               maxrows=10,
               order_by_firstcol="desc"
    )
  
})



output$rsv_occupancy_plot <- renderPlotly({
  occupancy_rapid %>%
    filter(pathogen == "RSV") %>%
    create_pathogen_occupancy_linechart()
  
})







