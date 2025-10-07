# create values for headline boxes

rsv_cases_recent_week <- Respiratory_Scot %>%
  filter(Pathogen == "Respiratory syncytial virus") %>%
  mutate(WeekEnding = convert_opendata_date(WeekEnding)) %>%
  tail(2) %>%
  select(-WeekBeginning) %>%
  rename(Date = WeekEnding) %>%
  #pivot_wider(names_from = FluType,
  #            values_from = Admissions) %>%
  mutate(DateLastWeek = .$Date[1],
         DateThisWeek = .$Date[2],
         CasesLastWeek = .$`NumberCasesPerWeek`[1],
         CasesThisWeek = .$`NumberCasesPerWeek`[2],
         PercentageDifference = round((CasesThisWeek/CasesLastWeek - 1)*100, digits = 2)) %>%
  mutate(ChangeFactor = case_when(
    PercentageDifference < 0 ~ "Decrease",
    PercentageDifference > 0 ~ "Increase",
    TRUE                     ~ "No change"),
    icon= case_when(ChangeFactor == "Decrease"~"arrow-down",
                    ChangeFactor == "Increase"~ "arrow-up",
                    ChangeFactor == "No change"~"equals")
  ) %>%
  select(DateLastWeek, DateThisWeek, CasesLastWeek, CasesThisWeek, PercentageDifference, ChangeFactor, icon) %>%
  head(1)
###

tagList(
  fluidRow(width = 12,
           metadataButtonUI("respiratory_rsv_mem"),
           linebreaks(1),
           #h1("RSV Incidence Rates"),
           #p("Respiratory syncytial virus (RSV) is a virus that generally causes mild cold like",
            # "symptoms but may occasionally result in severe lower respiratory infection such as",
           #  "bronchiolitis or pneumonia, particularly in infants and young children or in adults",
           #  "with compromised cardiac, pulmonary, or immune systems. RSV has an annual seasonality",
           #  "with peaks of activity in the winter months. Additional information can be found on the PHS page for RSV."),
           #linebreaks(1)
           ),


    fluidRow(width = 12,
             tabPanel(stringr::str_to_sentence("influenza"),
                      # headline figures for the week in Scotland
                      tagList(h2(glue("Summary of laboratory-confirmed RSV cases in Scotland")),
                              tags$div(class = "headline",
                                       br(),
                                      # h3(glue("Total number of RSV cases in Scotland over the last two weeks")),
                                       # previous week total number
                                        valueBox(value = {rsv_cases_recent_week %>% .$CasesLastWeek %>% format(big.mark=",")},
                                           subtitle = glue("Week ending {rsv_cases_recent_week %>% .$DateLastWeek%>% format('%d %b %y')}"),
                                           color = "navy",
                                           icon = icon_no_warning_fn("calendar-week")),
                                       # this week total number
                                       valueBox(value = {rsv_cases_recent_week %>% .$CasesThisWeek %>% format(big.mark=",")},
                                                subtitle = glue("Week ending {rsv_cases_recent_week %>% .$DateThisWeek%>% format('%d %b %y')}"),
                                                color = "navy",
                                                icon = icon_no_warning_fn("calendar-week")),
                                       # percentage difference between the previous weeks
                                       valueBox(value = glue("{rsv_cases_recent_week%>% .$PercentageDifference}%"),
                                          subtitle = glue("{rsv_cases_recent_week %>%.$ChangeFactor %>%  str_to_sentence()} in the last week"),
                                           color = "navy",
                                           icon = icon_no_warning_fn({rsv_cases_recent_week %>%  .$icon})),
                                        # This text is hidden by css but helps pad the box at the bottom
                                       h6("hidden text for padding page")
                                                                 )))), # headline
  
  fluidRow(width = 12,
           tagList(h2("RSV percentage test positivity"),
                   tabBox(width = NULL,
                          type = "pills",
                          tabPanel("Plot",
                                   tagList(linebreaks(1),
                                           altTextUI("rsv_positivity_modal"),
                                           swabposDefinitionUI("rsv_swabpos"),
                                           withNavySpinner(plotlyOutput("rsv_positivity_plot")),
                                           fluidRow(
                                             width=12, linebreaks(1)))),
                          tabPanel("Data",
                                   tagList(
                                     withNavySpinner(dataTableOutput("rsv_positivity_table"))
                                   ) # tagList
                          ) # tabPanel
                   ) # tabBox
           ) # tagList
  ), #fluidrow

  fluidRow(width = 12,
           tagList(h2("Laboratory-confirmed RSV incidence per 100,000 population in Scotland"))),

  fluidRow(
    tabBox(width = NULL,
           type = "pills",
           tabPanel("Plot",
                    tagList(linebreaks(1),
                            altTextUI("rsv_mem_modal"),
                            withNavySpinner(plotlyOutput("rsv_mem_plot")),
                            )),
           tabPanel("Data",
                    tagList(linebreaks(1),
                            withNavySpinner(dataTableOutput("rsv_mem_table"))
                    ) # tagList
           ) # tabPanel

    ), # tabBox
    linebreaks(1)
  ), # fluidRow

fluidRow(width = 12,
         tagList(h2("Laboratory-confirmed RSV incidence per 100,000 population by NHS Health Board"))),

fluidRow(
  p("Public Health Scotland have paused reporting of NHS Board-specific activity data as we investigate the ",
    "impact of different testing practices by board on incidence rates and implications for smaller board areas ",
    "specifically as they relate to the calculation of activity threshold levels."),
  linebreaks(1)
),

  # fluidRow(
  #   tabBox(width = NULL,
  #          type = "pills",
  #          tabPanel("Plot",
  #                   tagList(linebreaks(1),
  #                           altTextUI("rsv_mem_hb_modal"),
  #                           withNavySpinner(plotlyOutput("rsv_mem_hb_plot")),
  #                   )),
  #          tabPanel("Data",
  #                   tagList(linebreaks(1),
  #                           withNavySpinner(dataTableOutput("rsv_mem_hb_table"))
  #                   ) # tagList
  #          ) # tabPanel
  # 
  #   ), # tabBox
  #   linebreaks(1)
  # ), # fluidRow


  fluidRow(width = 12,
           tagList(h2("Laboratory-confirmed RSV incidence per 100,000 population by age group"))),

  fluidRow(
    tabBox(width = NULL,
           type = "pills",
           tabPanel("Plot",
                    tagList(linebreaks(1),
                            altTextUI("rsv_mem_age_modal"),
                            withNavySpinner(plotlyOutput("rsv_mem_age_plot")),
                    )),
           tabPanel("Data",
                    tagList(linebreaks(1),
                            withNavySpinner(dataTableOutput("rsv_mem_age_table"))
                    ) # tagList
           ) # tabPanel

    ), # tabBox
    linebreaks(1)
  )#, # fluidRow

)
