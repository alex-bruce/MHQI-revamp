
admissions_seasons <- age_rate_data_all_path %>%
  add_season() %>%
  mutate(Season = paste0(substr(Season, 1, 4), "/", substr(Season, 6, 9))) %>%
  select(Season) %>%
  unique() %>%
  unlist(., use.names=FALSE)

## Organise Covid data into the right format for the plot and table


cov_admissions <- age_rate_data_all_path %>% 
  filter(age_band == "All Ages") %>% 
  add_season() %>% 
  select(week_ending, cov, cov_rate, Season) %>% 
  rename(Date = week_ending,
         Admissions = cov,
         RatePer100000 = cov_rate) %>% 
  mutate(Year = year(Date),
         ISOWeek = isoweek(Date)) %>% 
  mutate(Season = paste0(substr(Season, 1, 4), "/", substr(Season, 6, 9)),
         Weekord = case_when(ISOWeek >= 40 ~ ISOWeek - 39,
                             ISOWeek < 40 ~ ISOWeek + 13)) %>% 
  filter(Season %in% tail(sort(unique(Season)), 3))

cov_adm_seasons <- tail(sort(unique(cov_admissions$Season)), 3)

cov_admissions_recent_week <- cov_admissions %>%
  tail(3) %>%
  mutate(DateTwoWeek = .$Date[1],
         DateLastWeek = .$Date[2],
         DateThisWeek = .$Date[3],
         AdmissionsTwoWeek = .$Admissions[1],
         AdmissionsLastWeek = .$Admissions[2],
         AdmissionsThisWeek = .$Admissions[3],
         RateTwoWeek = .$RatePer100000[1],
         RateLastWeek = .$RatePer100000[2],
         RateThisWeek = .$RatePer100000[3]) %>%
  select(DateTwoWeek, DateLastWeek, DateThisWeek, AdmissionsTwoWeek, AdmissionsLastWeek, AdmissionsThisWeek, RateTwoWeek, RateLastWeek, RateThisWeek) %>%
  head(1)

tagList(
  # fluidRow(width = 12,
  #          metadataButtonUI("hospital_admissions"),
  #          linebreaks(1),
  #          #h1("Acute COVID-19 hospital admissions"),
  #          #linebreaks(1)
  #          ),

  fluidRow(width = 12,
                  tabPanel("Acute hospital admissions",
                           tagList(h2("Number of acute COVID-19 admissions to hospital in Scotland"),
                                   tags$div(class = "headline",
                                            linebreaks(1),
                                            #h3("Weekly totals from last three weeks"),

                                            valueBox(value = {cov_admissions_recent_week %>%
                                                .$AdmissionsTwoWeek %>% format(big.mark=",")},
                                                subtitle = tagList(tags$strong(glue("({format(round(cov_admissions_recent_week %>% .$RateTwoWeek,1), nsmall = 1)} per 100,000)")),
                                                tags$br(),
                                                glue("Week ending {cov_admissions_recent_week %>%
                                                .$DateTwoWeek %>% format('%d %b %y')}")),
                                                color = "navy",
                                                icon = icon_no_warning_fn("calendar-week")),
                                            valueBox(value = {cov_admissions_recent_week %>%
                                                .$AdmissionsLastWeek %>% format(big.mark=",")},
                                                subtitle = tagList(tags$strong(glue("({format(round(cov_admissions_recent_week %>% .$RateLastWeek,1), nsmall = 1)} per 100,000)")),
                                                tags$br(),
                                                glue("Week ending {cov_admissions_recent_week %>%
                                                .$DateLastWeek %>% format('%d %b %y')}")),
                                                color = "navy",
                                                icon = icon_no_warning_fn("calendar-week")),
                                            valueBox(value = glue("{cov_admissions_recent_week %>%
                                         .$AdmissionsThisWeek %>% format(big.mark=",")}*"),
                                                     subtitle = tagList(tags$strong(glue("({format(round(cov_admissions_recent_week %>% .$RateThisWeek,1), nsmall = 1)} per 100,000)")),
                                                                     tags$br(),
                                                     glue("Week ending {cov_admissions_recent_week %>%
                                                .$DateThisWeek %>% format('%d %b %y')}")),
                                                     color = "navy",
                                                     icon = icon_no_warning_fn("calendar-week")),
                                            h4("* provisional figures",
                                               actionButton("glossary",
                                                            label = "Go to glossary",
                                                            icon = icon_no_warning_fn("paper-plane")
                                                            )), 
                                               # p("Between 22 May and October 2025, Public Health Scotland (PHS) will be",
                                               #   "reporting Scotland level admissions for COVID-19,",
                                               #   "Influenza and RSV, due to low levels of hospital admissions."),
                                               h6("hidden text for padding page")
                                              
                                               ,
                                            ),

                                   tagList(h2("Rate of acute COVID-19 hospital admissions per 100,000 population in Scotland")),
                                   
                           tabBox(width = NULL, type = "pills",
                                  tabPanel("Plot",
                                           tagList(
                                             linebreaks(1),
                                             altTextUI("hospital_admissions_modal"),
                                             withNavySpinner(plotlyOutput("hospital_admissions_plot"))),
                                           fluidRow(column(
                                             width=12, linebreaks(1),
                                             p("*Hospital admissions for the most recent week may be incomplete,",
                                               "and should be treated as provisional and interpreted with caution.")
                                           ))),
                                  tabPanel("Data",
                                           tagList(
                                             withNavySpinner(dataTableOutput("hospital_admissions_table")))
                                           ),

                           ),
                           
                           
                           
                           tagList(h2("Rate of acute COVID-19 hospital admissions per 100,000 population by age group")),
                           
                           #),
                           #br(),
                           #fluidRow(
                             tabBox(width = NULL,
                                    type = "pills",
                                    tabPanel("Plot",
                                             br(),
                                             pickerInput(inputId = "adm_season_cov_age",
                                                         label = "Select season",
                                                         choices = {admissions_seasons %>%  tail(3) },
                                                         selected = {admissions_seasons %>% tail(1)}),
                                             tagList(linebreaks(1),
                                                     altTextUI("hospital_admissions_age_modal"),
                                                     withNavySpinner(plotlyOutput("covid_admissions_age_plot")),
                                             )),
                                    tabPanel("Data",
                                             tagList(linebreaks(1),
                                                     withNavySpinner(dataTableOutput("covid_admissions_age_table"))
                                             ) # tagList
                                    ) # tabPanel
                                    
                             ), # tabBox
                             linebreaks(1),
                           #), 
                           
                           #fluidRow(width = NULL,
                                    tagList(h2("Rate of COVID-19 admissions per 100,000 population by NHS Health Board of treatment")),
                                    #linebreaks(1),#),
                           
                          
                           
                           
                             
                           
                          # fluidRow(
                             tabBox(width = NULL,
                                    type = "pills",
                                    tabPanel("Plot",
                                             br(),
                                             pickerInput(
                                               inputId = "hospital_adms_selected_seasons", 
                                               label = "Select season", 
                                               choices = tail(sort(unique(admissions_hb_all_path$Season)), 3),
                                               selected = tail(sort(unique(admissions_hb_all_path$Season)), 1)  # current season
                                               ),
                                             tagList(linebreaks(1),
                                                     altTextUI("hospital_admissions_hb_modal"),
                                                     withNavySpinner(plotlyOutput("hospital_admissions_hb_plot")),
                                             )),
                                    tabPanel("Data",
                                             tagList(linebreaks(1),
                                                     withNavySpinner(dataTableOutput("hospital_admissions_hb_table"))
                                             ) # tagList
                                    ) # tabPanel
                                    
                             ), # tabBox
                             linebreaks(1)
                          # ), 
                          ,
                           
                           
                           
                           # fluidRow
                           
                           
                           
                           #          tagList(h2("Number of acute COVID-19 admissions to hospital by NHS Health Board of treatment; week ending")),
                           # 
                           # 
                           # fluidRow(width=12,
                           #          box(width = NULL,
                           #              withNavySpinner(dataTableOutput("hospital_admissions_hb_table"))),
                           # ),

                           tagList(h2("Rate of acute COVID-19 hospital admissions per 100,000 population by deprivation category (SIMD)"))

                           ),
                          # br(),
                           
                           tabBox(width = NULL, type = "pills",
                                  tabPanel("Plot",
                                           br(),
                                           pickerInput(inputId = "adm_season_cov_simd",
                                                       label = "Select season",
                                                       choices = {admissions_seasons %>%  tail(3) },
                                                       selected = {admissions_seasons %>% tail(1)}),
                                           tagList(
                                             linebreaks(1),
                                             altTextUI("hospital_admissions_simd_modal"),
                                             actionButton("btn_modal_simd",
                                                          label = "What is SIMD?",
                                                          class = "simd-btn",
                                                          icon = icon_no_warning_fn("circle-question")
                                             ),
                                             withNavySpinner(
                                               plotlyOutput("hospital_admissions_simd_plot"))
                                           )
                                  ),
                                  tabPanel("Data",
                                           tagList(
                                             withNavySpinner(
                                               dataTableOutput("hospital_admissions_simd_table"))
                                           )
                                  )
                                  
                           ),

                           ##### age/sex admissions pyramid        
                           
                           # #tagList(h2(glue("Acute COVID-19 cases by age and sex in Scotland")),
                           # tagList(uiOutput("cov_adm_pyr_title")),
                           # tabBox(width = NULL,
                           #        type = "pills",
                           #        tabPanel("Plot",
                           #                 tagList(linebreaks(1),
                           #                         fluidRow(column(4, pickerInput("cov_age_sex_adm_season",
                           #                                                        label = "Select a season",
                           #                                                        choices = {Admissions_AgeSex_Season %>% 
                           #                                                            filter(Pathogen == "cov") %>%
                           #                                                            .$Season %>% unique()},
                           #                                                        selected = "2024-2025")  )),#tfluidrow
                           #                                                   altTextUI("covid_adm_age_sex"),
                           #                         withNavySpinner(plotlyOutput("covid_adm_age_sex_pyramid_plot"))
                           #                                                 ) # tagList
                           #                                        ), # tabPanel
                           #        tabPanel("Data",
                           #                 withNavySpinner(dataTableOutput("covid_adm_age_sex_pyramid_table"))) #tabpanel
                           #               ), # tabbox
                           #                         #), #age/sex 
                           #                # ),
                           ##### LOS section
                           tagList(h2("Average length of stay of acute COVID-19 hospital admissions"),
                                   #  temporary caveat for no LOS information
                                   # tagList("Public Health Scotland have paused reporting of the Length",
                                   #         "of Stay of acute COVID-19 hospital admissions as we undertake developments",
                                   #         "to include this analysis for other respiratory pathogens.")
                                   ),
                           
                           
                           # tagList(h2("Length of stay of acute COVID-19 hospital admissions"),
                           #         tags$div(class = "headline",
                           #                  h3(glue("Median length of stay of acute COVID-19 hospital admissions for 4 week period {los_date_start %>% format('%d %b %y')} to {los_date_end%>% format('%d %b %y')} ")),
                           #                  valueBox(value = glue("{
                           #                                       Length_of_Stay_Median %>% 
                           #                                       filter(AgeGroup == 'All Ages') %>% 
                           #                                       filter(Pathogen =='cov') %>%
                           #                                       .$MedianLengthOfStay %>% round_half_up(1)} days"),
                           #                           subtitle = glue("All ages"),
                           #                           color = "navy",
                           #                           icon = icon_no_warning_fn("clock")),# valuebox
                           #                  valueBox(value = glue("{cov_los_median_min$MedianLengthOfStay %>%
                           #                                        round_half_up(1)} days"),
                           #                           subtitle = glue("Shortest median stay ({cov_los_median_min$AgeGroup})"),
                           #                           color = "navy",
                           #                           icon = icon_no_warning_fn("clock")),# value box
                           #                  valueBox(value = glue("{cov_los_median_max$MedianLengthOfStay %>%
                           #                                        round_half_up(1)} days"),
                           #                           subtitle = glue("Longest median stay ({cov_los_median_max$AgeGroup})"),
                           #                           color = "navy",
                           #                           icon = icon_no_warning_fn("clock")),
                           #                 # This text is hidden by css but helps pad the box at the bottom
                           #                 h6("hidden text for padding page"))
                           #         ),
                           # br(), 
                           tabBox( width = NULL, type = "pills",
                                   tabPanel("Plot",
                                            #tagList(uiOutput("cov_los_title")),
                                            tagList(h5("Use the drop-down menu to select a season."),
                                                    pickerInput(inputId = "los_season_cov",
                                                                label = "Select season",
                                                                choices = admission_seasons,
                                                                selected = tail(admission_seasons, 1)),
                                                    altTextUI("cov_los_modal"),
                                                    withNavySpinner( plotlyOutput("cov_los_plot")),
                                                    #linebreaks(1)
                                            ),
                                            fluidRow(column(
                                              width=12, linebreaks(1),
                                              textOutput("cov_los_text")
                                            ))), #taglist
                                   tabPanel("Data",
                                            tagList(linebreaks(1),
                                                    withNavySpinner(dataTableOutput("cov_los_table")) )
                                   ) # tabPanel
                           ),#tabbox
                           ###
### end LOS section
                           tagList(h2("Number of acute COVID-19 admissions to hospital by ethnicity"),
                                   #  temporary caveat for no Ethnicity information
                                   tagList("Public Health Scotland have paused reporting of  COVID-19 admissions to",
                                           "hospital broken down by ethnic group",
                                           " as we undertake developments",
                                           "to include this analysis for other respiratory pathogens.")),
                  )#tabPanel
  ), #fluid row
  
  
  
  # Padding out the bottom of the page
  fluidRow(height="200px", width=12, linebreaks(5))
  
)#taglist
