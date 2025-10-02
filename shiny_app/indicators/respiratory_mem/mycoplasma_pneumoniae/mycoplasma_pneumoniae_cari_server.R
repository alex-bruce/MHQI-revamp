
metadataButtonServer(id="respiratory_mycoplasma_pneumoniae_cari",
                     panel="Mycoplasma Pneumoniae",
                     parent = session)

altTextServer("mycoplasma_pneumoniae_cari_modal",
              title = "CARI - Test positivity for Mycoplasma Pneumoniae",
              content = tags$ul(tags$li("This is a plot showing the test positivity rate of Mycoplasma pneumoniae infection in the Community Acute Respiratory Infection (CARI) surveillance programme."),
                                tags$li("The x axis is the week ending date, starting 09 October 2022."),
                                tags$li("The y axis is the test positivity rate."),
                                tags$li("The solid purple line is the specified test positivity rate and the lighter purple area around the line indicates the confidence interval."),
                                tags$li("The bottom of the light purple shaded area represents the lower confidence interval and the top of the area represents the upper confidence interval.")))

altTextServer("mpn_cari_age_modal",
              title = "CARI - Test positivity for MPN by age group",
              content = tags$ul(tags$li("This is a plot showing the test positivity rate of MPN infection by age group in the Community Acute Respiratory Infection (CARI) surveillance programme."),
                                tags$li("The x axis is the week ending date, starting 09 October 2022."),
                                tags$li("The y axis is the test positivity rate."),
                                tags$li("The plot contains a trace showing the test positivity rate for for the selected age group(s)."),
                                tags$li("Each trace can be hidden/unhidden by clicking on the relevant age group from the legend on the right of the chart.")))

altTextServer("mpn_cari_hb_modal",
              title = "CARI - Test positivity for MPN by NHS Health Board",
              content = tags$ul(tags$li("This is a plot showing the test positivity rate of MPN infection by NHS Health Board in the Community Acute Respiratory Infection (CARI) surveillance programme."),
                                tags$li("The x axis is the week ending date, starting 09 October 2022."),
                                tags$li("The y axis is the test positivity rate."),
                                tags$li("The plot contains a trace showing the test positivity rate for the selected NHS Health Board(s)."),
                                tags$li("Each trace can be hidden/unhidden by clicking on the relevant age group from the legend on the right of the chart.")))

# CARI - Overall Mycoplasma Pneumoniae swabpos table
output$mycoplasma_pneumoniae_cari_table <- renderDataTable({
  Respiratory_Pathogens_CARI_Scot %>%
    filter(Pathogen == "Mycoplasma Pneumoniae") %>%
    arrange(desc(WeekEnding)) %>%
    select(WeekEnding, TotalSamples, PositiveSamples, SwabPositivity, SwabPositivityLCL, SwabPositivityUCL) %>%
    rename(`Week Ending` = WeekEnding,
           `Total Samples` = TotalSamples,
           `Positive Samples` = PositiveSamples,
           `Test Positivity (%)` = SwabPositivity,
           `Lower Confidence Limit (%)` = SwabPositivityLCL,
           `Upper Confidence Limit (%)` = SwabPositivityUCL) %>%
    make_table()
})

# # CARI - Mycoplasma Pneumoniae swabpos by age table
# output$mycoplasma_pneumoniae_cari_age_table <- renderDataTable({
#   Respiratory_Pathogens_CARI_Age %>%
#     filter(Pathogen == "Mycoplasma Pneumoniae") %>%
#     arrange(desc(WeekEnding)) %>%
#     select(WeekEnding, AgeGroup, TotalSamples, PositiveSamples, SwabPositivity, SwabPositivityLCL, SwabPositivityUCL) %>%
#     mutate(AgeGroup = factor(AgeGroup, levels = c("0-4 years", "5-14 years", "15-44 years", "45-64 years",
#                                                   "65-74 years", "75+ years"))) %>%
#     rename(`Week Ending` = WeekEnding,
#            `Age Group`= `AgeGroup`,
#            `Total Samples` = TotalSamples,
#            `Positive Samples` = PositiveSamples,
#            `Test Positivity (%)` = SwabPositivity,
#            `Lower Confidence Limit (%)` = SwabPositivityLCL,
#            `Upper Confidence Limit (%)` = SwabPositivityUCL) %>%
#     make_table(filter_cols = c(2))
# })

# CARI - Overall Mycoplasma Pneumoniae swabpos plot
output$mycoplasma_pneumoniae_cari_plot <- renderPlotly({
  Respiratory_Pathogens_CARI_Scot %>%
    filter(Pathogen == "Mycoplasma Pneumoniae") %>%
    create_cari_linechart()
  
})

# # CARI - Mycoplasma Pneumoniae swabpos by age plot
# output$mycoplasma_pneumoniae_cari_age_plot <- renderPlotly({
#   Respiratory_Pathogens_CARI_Age %>%
#     filter(Pathogen == "Mycoplasma Pneumoniae") %>%
#     mutate(AgeGroup = factor(AgeGroup, levels = c("0-4 years", "5-14 years", "15-44 years", "45-64 years",
#                                                   "65-74 years", "75+ years"))) %>%
#     create_cari_age_linechart()
#   
# })

# CARI - mpn swabpos by age table
output$mpn_cari_age_table <- renderDataTable({
  mpn_cari_age %>%
    arrange(desc(WeekEnding), AgeGroup) %>%
    filter(AgeGroup %in% input$mpn_cari_selected_age) %>%
    select(WeekEnding, AgeGroup, TotalSamples, PositiveSamples, SwabPositivity, SwabPositivityLCL, SwabPositivityUCL) %>%
    mutate(AgeGroup = factor(AgeGroup)) %>%
    rename(`Week Ending` = WeekEnding,
           `Age Group`= `AgeGroup`,
           `Total Samples` = TotalSamples,
           `Positive Samples` = PositiveSamples,
           `Test Positivity (%)` = SwabPositivity,
           `Lower Confidence Limit (%)` = SwabPositivityLCL,
           `Upper Confidence Limit (%)` = SwabPositivityUCL) %>%
    make_table(filter_cols = c(1,2))
})

# CARI - mpn swabpos by age plot
output$mpn_cari_age_plot <- renderPlotly({
  mpn_cari_age %>%
    filter(AgeGroup %in% input$mpn_cari_selected_age) %>%
    create_cari_age_linechart2()
  
})

# CARI - Overall mpn HB swabpos plot
output$mpn_cari_hb_plot <- renderPlotly({
  mpn_cari_hb %>%
    filter(HBName %in% input$mpn_cari_selected_boards) %>%
    create_cari_hb_linechart()
  
})

# CARI - mpn swabpos by hb table
output$mpn_cari_hb_table <- renderDataTable({
  mpn_cari_hb %>%
    arrange(desc(WeekEnding), HBName) %>%
    filter(HBName %in% input$mpn_cari_selected_boards) %>%
    mutate(HBName = factor(HBName)) %>%
    select(WeekEnding, HBName, TotalSamples, PositiveSamples, SwabPositivity, SwabPositivityLCL, SwabPositivityUCL) %>%
    rename(`Week Ending` = WeekEnding,
           `NHS Health Board`= `HBName`,
           `Total Samples` = TotalSamples,
           `Positive Samples` = PositiveSamples,
           `Test Positivity (%)` = SwabPositivity,
           `Lower Confidence Limit (%)` = SwabPositivityLCL,
           `Upper Confidence Limit (%)` = SwabPositivityUCL) %>%
    make_table(filter_cols = c(1,2))
})



