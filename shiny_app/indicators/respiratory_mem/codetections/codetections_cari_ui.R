
# CARI HB data
codetection_cari_age <- Respiratory_Pathogens_CARI_codetections %>% 
  mutate(AgeGroup = factor(AgeGroup, levels = c("All ages", "0-4 years", "5-14 years", 
                                                "15-44 years", "45-64 years", "65+ years")))

tagList(
  
  fluidRow(width = 12,
           metadataButtonUI("respiratory_codetections_cari"),
           linebreaks(1),
  ),
  
  fluidRow(width = 12,
           tagList(h2("CARI - Pathogen composition of two-pathogen co-detections"))),
  
  fluidRow(
    tabBox(width = NULL,
           type = "pills",
           tabPanel("Plot",
                    tagList(linebreaks(1),
                            altTextUI("duodetections_cari_modal"),
                            withNavySpinner(plotlyOutput("duodetections_cari_plot")),
                    )),
           tabPanel("Data",
                    tagList(linebreaks(1),
                            withNavySpinner(dataTableOutput("duodetections_cari_table"))
                    ) # tagList
           ) # tabPanel
           
    ), # tabBox
    linebreaks(1)
  ),
  
  fluidRow(width = 12,
           tagList(h2("CARI - Proportion of positive samples that are co-detections by age group"))),
  
  fluidRow(
    selectInput("codetection_cari_selected_age", "Select age group(s) of interest:", 
                choices = sort(unique(codetection_cari_age$AgeGroup)),
                selected = sort(unique(codetection_cari_age$AgeGroup))[1],
                multiple = TRUE),
    tabBox(width = NULL,
           type = "pills",
           tabPanel("Plot",
                    tagList(linebreaks(1),
                            altTextUI("codetections_cari_modal"),
                            withNavySpinner(plotlyOutput("codetections_cari_plot")),
                    )),
           tabPanel("Data",
                    tagList(linebreaks(1),
                            withNavySpinner(dataTableOutput("codetections_cari_table"))
                    ) # tagList
           ) # tabPanel
           
    ), # tabBox
    linebreaks(1)
  )
)
