cov_cases_seasons <- 
  Respiratory_Pathogens_Test_Positivity_by_Age %>% 
  filter(pathogen == "Covid-19") %>% 
  select(season) %>% 
  unique() %>% 
  tail(3)

# create values for headline boxes
pop_scot_total <- i_population_v2 %>%
  filter(AgeGroup == "Total", Sex == "Total") %>%
  .$PopNumber

cov_cases_recent_week <- Respiratory_Scot %>%
  filter(Pathogen == "Covid-19") %>%
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
         PercentageDifference = round((CasesThisWeek/CasesLastWeek - 1)*100, digits = 2),
         RateLastWeek = round_half_up(100000 *  .$`NumberCasesPerWeek`[1]/pop_scot_total,1),
         RateThisWeek = round_half_up(100000 *  .$`NumberCasesPerWeek`[2]/pop_scot_total,1)) %>%
  mutate(ChangeFactor = case_when(
    PercentageDifference < 0 ~ "Decrease",
    PercentageDifference > 0 ~ "Increase",
    TRUE                     ~ "No change"),
    icon= case_when(ChangeFactor == "Decrease"~"arrow-down",
                    ChangeFactor == "Increase"~ "arrow-up",
                    ChangeFactor == "No change"~"equals")
  ) %>%
  select(DateLastWeek, DateThisWeek, CasesLastWeek, CasesThisWeek, PercentageDifference, RateLastWeek, RateThisWeek, ChangeFactor, icon) %>%
  head(1)

###
tagList(
  # fluidRow(width = 12,
  #          
  #          metadataButtonUI("cases"),
  #          linebreaks(1),
  #          # h1("COVID-19 cases"),
  #          # p("Coronavirus disease (COVID-19) is an infectious disease caused by the SARS-CoV-2 virus.",
  #          #   "The most common symptoms are fever, chills, and sore throat. Anyone can get sick with",
  #          #   "COVID-19 but most people will recover without treatment. As yet, COVID-19 has not been",
  #          #   "shown to follow the same seasonal patterns as other respiratory pathogens.",
  #          #   "Additional information can be found on the PHS page for" , tags$a(href = "https://publichealthscotland.scot/our-areas-of-work/conditions-and-diseases/covid-19/",
  #          #                                                                      "COVID-19"), "."),
  #          # linebreaks(1)
  # ),
  
  #   fluidRow(width = 12,
  #            tagList(h2("Seven day average trend in wastewater COVID-19"),
  #                    tags$div(class = "headline",
  #                             h3(glue("Figure from week ending {Wastewater %>% tail(1) %>%
  #                 .$Date %>% convert_opendata_date() %>% format('%d %b %y')}")),
  #                 valueBox(value = {Wastewater %>% tail(1) %>%
  #                     .$WastewaterSevenDayAverageMgc %>%
  #                     signif(3) %>%
  #                     paste("Mgc/p/d")},
  #                     subtitle = "COVID-19 wastewater level",
  #                     color = "navy",
  #                     icon = icon_no_warning_fn("faucet-drip")),
  #                 # This text is hidden by css but helps pad the box at the bottom
  #                 h6("hidden text for padding page"))),
  #            linebreaks(1)),
  # 
  # 
  #   fluidRow(
  #     tabBox(width = NULL,
  #            type = "pills",
  #            tabPanel("Plot",
  #                     tagList(linebreaks(1),
  #                             altTextUI("wastewater_modal"),
  #                             withNavySpinner(plotlyOutput("wastewater_plot")),
  #                             fluidRow(column(
  #                               width=12, linebreaks(5),
  #                               p("Wastewater data analyses for COVID-19 are produced by",
  #                                 "PHS Wastewater Analysis Group for the Wastewater Monitoring Programme in Scotland,",
  #                                 "which is operated by Scottish Government in partnership with",
  #                                 "Scottish Water and the Scottish Environment Protection Agency."),
  # 
  #                             )))),
  #            tabPanel("Data",
  #                     tagList(linebreaks(1),
  #                             withNavySpinner(dataTableOutput("wastewater_table"))
  #                     ) # tagList
  #            ) # tabPanel
  # 
  #     ) # tabBox
  #   ), # fluidRow
  # 
  #   fluidRow(
  #     width = 12, br()),
  
  fluidRow(width = 12,
           tabPanel(stringr::str_to_sentence("influenza"),
                    # headline figures for the week in Scotland
                    tagList(h2(glue("Summary of laboratory-confirmed COVID-19 cases in Scotland")),
                            tags$div(class = "headline",
                                     br(),
                                     # h3(glue("Total number of Covid-19 cases in Scotland over the last two weeks")),
                                     # previous week total number
                                     valueBox(value = tagList({cov_cases_recent_week %>% .$CasesLastWeek %>% format(big.mark=",")},
                                                              br(),
                                                              glue("({format(cov_cases_recent_week %>% .$RateLastWeek, nsmall = 1)} per 100,000)")),
                                              subtitle = glue("Week ending {cov_cases_recent_week %>% .$DateLastWeek%>% format('%d %b %y')}"),
                                              color = "navy",
                                              icon = icon_no_warning_fn("calendar-week")),
                                     # this week total number
                                     valueBox(value = tagList({cov_cases_recent_week %>% .$CasesThisWeek %>% format(big.mark=",")},
                                                              tags$br(),
                                                              glue("({format(cov_cases_recent_week %>% .$RateThisWeek, nsmall = 1)} per 100,000)")),
                                              subtitle = glue("Week ending {cov_cases_recent_week %>% .$DateThisWeek%>% format('%d %b %y')}"),
                                              color = "navy",
                                              icon = icon_no_warning_fn("calendar-week")),
                                     # percentage difference between the previous weeks
                                     valueBox(value = glue("{cov_cases_recent_week%>% .$PercentageDifference}%"),
                                              subtitle = glue("{cov_cases_recent_week %>%.$ChangeFactor %>%  str_to_sentence()} in the last week"),
                                              color = "navy",
                                              icon = icon_no_warning_fn({cov_cases_recent_week %>%  .$icon})),
                                     # This text is hidden by css but helps pad the box at the bottom
                                     h6("hidden text for padding page")
                            )))), # headline
  
  fluidRow(width = 12,
           tagList(h2("COVID-19 percentage test positivity"),
                   tabBox(width = NULL,
                          type = "pills",
                          tabPanel("Plot",
                                   tagList(linebreaks(1),
                                           altTextUI("covid_positivity_modal"),
                                           swabposDefinitionUI("covid_swabpos"),
                                           withNavySpinner(plotlyOutput("covid_positivity_plot")),
                                           )),
                          tabPanel("Data",
                                   tagList(
                                     withNavySpinner(dataTableOutput("covid_positivity_table"))
                                   ) # tagList
                          ) # tabPanel
                   ) # tabBox
           ) # tagList
  ), #fluidrow
  
  
  fluidRow(width = 12,
           tagList(h2("COVID-19 percentage test positivity by age"),
                   tabBox(width = NULL,
                          type = "pills",
                          tabPanel("Plot",
                                   br(),
                                   pickerInput(inputId = "test_pos_cov_age",
                                               label = "Select season",
                                               choices = {cov_cases_seasons %>% tail(3) },
                                               selected = {cov_cases_seasons %>% tail(1)}),
                                   tagList(linebreaks(1),
                                           altTextUI("covid_positivity_age_modal"),
                                           swabposDefinitionUI("covid_age_swabpos"),
                                           withNavySpinner(plotlyOutput("covid_positivity_age_plot")),
                                           )),
                          tabPanel("Data",
                                   tagList(
                                     withNavySpinner(dataTableOutput("covid_positivity_age_table"))
                                   ) # tagList
                          ) # tabPanel
                   ) # tabBox
           ) # tagList
  ), #fluidrow
  
  
  
  # fluidRow(width = 12,
  #        tagList(h2("Laboratory-confirmed COVID-19 incidence per 100,000 population in Scotland"))),
  # 
  # fluidRow(
  #   tabBox(width = NULL,
  #          type = "pills",
  #          tabPanel("Plot",
  #                   tagList(linebreaks(1),
  #                           altTextUI("reported_cases_per_100k"),
  #                           withNavySpinner(plotlyOutput("covid_line_plot")),
  #                   )),
  #          tabPanel("Data",
  #                   tagList(
  #                     withNavySpinner(dataTableOutput("covid_cases_table"))
  #                   ) # tagList
  #          )
  #          
  #   ), # tabBox
  #   linebreaks(1)
  # ), # fluidRow
  
  fluidRow(width = 12,
           tagList(h2("Laboratory-confirmed COVID-19 incidence per 100,000 population in Scotland"))),
  
  fluidRow(
    tabBox(width = NULL,
           type = "pills",
           tabPanel("Plot",
                    tagList(linebreaks(1),
                            altTextUI("covid_mem_modal"),
                            withNavySpinner(plotlyOutput("covid_mem_plot")),
                    )),
           tabPanel("Data",
                    tagList(linebreaks(1),
                            withNavySpinner(dataTableOutput("covid_mem_table"))
                    ) # tagList
           ) # tabPanel
           
    ), # tabBox
    linebreaks(1)
  ), # fluidRow
  
  fluidRow(width = 12,
           tagList(h2("Laboratory-confirmed COVID-19 incidence per 100,000 population by NHS Health Board"))),
  
  fluidRow(
    tabBox(width = NULL,
           type = "pills",
           tabPanel("Plot",
                    tagList(linebreaks(1),
                            altTextUI("covid_mem_hb_modal"),
                            withNavySpinner(plotlyOutput("covid_mem_hb_plot", height = "500px")),
                    )),
           tabPanel("Data",
                    tagList(linebreaks(1),
                            withNavySpinner(dataTableOutput("covid_mem_hb_table"))
                    ) # tagList
           ) # tabPanel
           
    ), # tabBox
    linebreaks(1)
  ), # fluidRow
  
  fluidRow(width = 12,
           tagList(h2("Laboratory-confirmed COVID-19 incidence per 100,000 population by age group"))),
  
  fluidRow(
    tabBox(width = NULL,
           type = "pills",
           tabPanel("Plot",
                    tagList(linebreaks(1),
                            altTextUI("covid_mem_age_modal"),
                            withNavySpinner(plotlyOutput("covid_mem_age_plot")),
                    )),
           tabPanel("Data",
                    tagList(linebreaks(1),
                            withNavySpinner(dataTableOutput("covid_mem_age_table"))
                    ) # tagList
           ) # tabPanel
           
    ), # tabBox
    linebreaks(1)
  ), # fluidRow
  
  fluidRow(
    tagList(h2(glue("Laboratory-confirmed COVID-19 cases by age and sex in Scotland")),
            
            tabBox(width = NULL,
                   type = "pills",
                   tabPanel("Plot",
                            tagList(
                              linebreaks(1),
                              # adding selection for flu subtype
                              fluidRow(
                                column(4, pickerInput("covid_respiratory_season",
                                                      label = "Select a season",
                                                      choices = {covid_cases_agesex_season %>% 
                                                          filter(season >= "2023/2024") %>%
                                                          .$season %>% unique()},
                                                      selected = {covid_cases_agesex_season %>% 
                                                          filter(season >= "2023/2024") %>%
                                                          .$season %>% unique()})
                                )
                              ),
                              altTextUI("covid_age_sex"),
                              withNavySpinner(plotlyOutput("covid_age_sex_pyramid_plot"))
                            ) # tagList
                   ), # tabPanel
                   tabPanel("Data",
                            withNavySpinner(dataTableOutput("covid_age_sex_pyramid_table")))
            ) # tabbox
    ), # tagList
    linebreaks(1)
  ),
  
  # Padding out the bottom of the page
  fluidRow(
    width=12, linebreaks(5))
  
)#taglist


