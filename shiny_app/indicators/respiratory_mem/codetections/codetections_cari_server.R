
metadataButtonServer(id="respiratory_codetections_cari",
                     panel="Co-detections",
                     parent = session)

altTextServer("codetections_cari_modal",
              title = "CARI - Proportion of two pathogen co-detections",
              content = tags$ul(tags$li("This is a plot showing the proportion of samples positive for two pathogens for each pathogen in the Community Acute Respiratory Infection (CARI) surveillance programme."),
                                tags$li("The x axis is the week ending date, starting 09 October 2022."),
                                tags$li("The y axis is the percentage of two pathogen co-detections.")))

# CARI - Overall Rhinovirus swabpos table
output$codetections_cari_table <- renderDataTable({
  Respiratory_Pathogens_CARI_duodetections %>%
    arrange(desc(WeekEnding)) %>%
    select(WeekEnding, pathogen, perc) %>%
    mutate(perc = paste0(round_half_up(perc,1), "%")) %>%
    rename(`Week Ending` = WeekEnding,
           `Pathogen` = pathogen,
           `Percentage of two pathogen co-detections (%)` = perc) %>%
    make_table(filter_cols = c(1,2))
})


# CARI - Overall Rhinovirus swabpos plot
output$codetections_cari_plot <- renderPlotly({
  Respiratory_Pathogens_CARI_duodetections %>%
    create_cari_duodetection_chart()

})
