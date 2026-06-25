tagList(
  fluidRow(width = 12,
           tabPanel(stringr::str_to_sentence("influenza"),
                    tags$div(class = "headline",
                             h3(glue("Laboratory-confirmed influenza cases by NHS Health Board and subtype")),
                             h4(glue("during week {this_week_iso} (ending {Resp_cases_recent_weeks %>% .$DateThisWeek %>% tail(1) %>%  format('%d %b %y')})")),
                             linebreaks(1),
                             column(6,
                                    tagList(
                                      pickerInput("respiratory_headline_subtype",
                                                  label = glue("Select subtype"),
                                                  choices = {Flu_Subtype_Cases %>%  select(Pathogen) %>%  unique() %>%  pull()}
                                                  ),
                                      withNavySpinner(valueBoxOutput("respiratory_headline_figures_subtype_count", width = NULL))
                                    )
                             ),
                             column(6,
                                    tagList(
                                      pickerInput("respiratory_headline_healthboard",
                                                  label = "Select a NHS Health Board",
                                                  choices = {Flu_Subtype_Cases %>%
                                                      .$HBName %>% unique() %>% sort()},
                                                  selected = "Scotland"
                                      ),  # pickerInput
                                      withNavySpinner(valueBoxOutput("respiratory_headline_figures_healthboard_count", width = NULL))
                                    ) # tagList
                             ), # column
                             # This text is hidden by css but helps pad the box at the bottom
                             h6("hidden text for padding page")
                    ) # headline
           ),
           linebreaks(1)
  ),


  fluidRow(width = 12,
           tagList(h2("Trends of laboratory-confirmed influenza cases in Scotland"))),
  
  fluidRow(width = 12,
           tagList(uiOutput("respiratory_over_time_title"),
                   tabBox(width = NULL,
                          type = "pills",
                          tabPanel("Plot",
                                   tagList(
                                     linebreaks(1),
                                     fluidRow(column(4, pickerInput("respiratory_select_healthboard",
                                                                    label = "Select a NHS Health Board",
                                                                    choices = {Flu_Subtype_Cases %>%
                                                                        .$HBName %>% unique() %>% sort()},
                                                                    selected = "Scotland") # pickerInput
                                                                                        ), # column
                                              column(4, pickerInput("respiratory_select_season",
                                                                    label = "Select a season",
                                                                    choices = recent_six_seasons, # found in setup
                                                                    selected = tail(recent_six_seasons,1))), # column
                                                     column(4, pickerInput("respiratory_y_axis_plots",
                                                                           label =     "Select number or rate",
                                                                           choices = c("Number of cases", "Rate per 100,000"),
                                                                           selected = "Number of cases") )
                                              ), #filters fluid row
                                     altTextUI("respiratory_over_time_modal"),
                                     withNavySpinner(plotlyOutput("respiratory_over_time_plot")))),
                          tabPanel("Data",
                                   withNavySpinner(dataTableOutput("respiratory_over_time_table")))
                   ) # tabbox
           ) # taglist)
  ),

  fluidRow(width = 12,
           linebreaks(2)),

  fluidRow(width = 12,
           tagList(uiOutput("respiratory_by_season_title"),
                   tabBox(width = NULL,
                          type = "pills",
                          tabPanel("Plot",
                                   tagList(
                                     linebreaks(1),
                                     # adding selection forsubtype
                                     fluidRow(
                                       column(6, pickerInput("respiratory_select_subtype",
                                                             label = glue("Select which subtype you would like to see"),
                                                             choices = {Flu_Subtype_Cases %>%  select(Pathogen) %>%  unique() %>%  pull()
                                                                }) # pickerInput
                                       ) # column
                                     ), # fluidRow
                                     altTextUI("respiratory_by_season_modal"),
                                     withNavySpinner(plotlyOutput("respiratory_by_season_plot")),
                                     br()
                                   ) # tagList
                          ), # tabPanel
                          tabPanel("Data",
                                   withNavySpinner(dataTableOutput("respiratory_by_season_table")))
                   ) # tabbox
           )#, # tagList
  ),

  fluidRow(width = 12,
           linebreaks(2))

  )
