
metadataButtonServer(id="respiratory_codetections_cari",
                     panel="Co-detections",
                     parent = session)

altTextServer("duodetections_cari_modal",
              title = "CARI - Relative frequency (%) of each pathogen in all samples with two-pathogen co-detections",
              content = tags$ul(tags$li("This is a plot showing how often each individual pathogen is identified (expressed as a percentage of all pathogens identified) in all samples with two-pathogen co-detections in the Community Acute Respiratory Infection (CARI) surveillance programme."),
                                tags$li("The x axis is the week ending date, starting 09 October 2022."),
                                tags$li("The y axis is the percentage of all pathogens identified.")))

altTextServer("codetections_cari_modal",
              title = "CARI - Proportion of positive samples that are co-detections by age group",
              content = tags$ul(tags$li("This is a plot showing the proportion of positive samples that are co-detections in the Community Acute Respiratory Infection (CARI) surveillance programme, by age group."),
                                tags$li("The x axis is the four-week ending date, starting October 2022."),
                                tags$li("The y axis is the percentage of positive samples.")))


# CARI - Overall Rhinovirus swabpos table
output$duodetections_cari_table <- renderDataTable({
  Respiratory_Pathogens_CARI_duodetections %>%
    arrange(desc(WeekEnding), desc(pathogen)) %>%
    select(WeekEnding, pathogen, perc) %>%
    mutate(pathogen = as.character(pathogen)) %>%
    mutate(pathogen = factor(pathogen)) %>%
    mutate(perc = round_half_up(perc,1)) %>%
    rename(`Week Ending` = WeekEnding,
           `Pathogen` = pathogen,
           `Percentage (%)` = perc) %>%
    make_table(filter_cols = c(1,2))
})


# CARI - Overall Rhinovirus swabpos plot
output$duodetections_cari_plot <- renderPlotly({
  Respiratory_Pathogens_CARI_duodetections %>%
    create_cari_duodetection_chart()

})


# CARI - Overall Rhinovirus swabpos table
output$codetections_cari_table <- renderDataTable({
  Respiratory_Pathogens_CARI_codetections %>%
    mutate(AgeGroup = factor(AgeGroup, levels = c("All ages", "0-4 years", "5-14 years",
                                                  "15-44 years", "45-64 years", "65+ years"))) %>%
    filter(AgeGroup %in% input$codetection_cari_selected_age) %>%
    arrange(desc(FourWeekEnding), AgeGroup) %>%
    mutate(AgeGroup = factor(AgeGroup)) %>%
    select(FourWeekEnding, AgeGroup, perc) %>%
    mutate(perc = round_half_up(perc,1)) %>%
    rename(`Four-Week Ending` = FourWeekEnding,
           `Age Group` = AgeGroup,
           `Percentage of positive samples (%)` = perc) %>%
    make_table(filter_cols = c(1,2))
})


# CARI - Overall Rhinovirus swabpos plot
output$codetections_cari_plot <- renderPlotly({
  Respiratory_Pathogens_CARI_codetections %>%
    mutate(AgeGroup = factor(AgeGroup, levels = c("All ages", "0-4 years", "5-14 years",
                                                  "15-44 years", "45-64 years", "65+ years"))) %>%
    filter(AgeGroup %in% input$codetection_cari_selected_age) %>%
    create_cari_codetection_age_linechart()
  
})

