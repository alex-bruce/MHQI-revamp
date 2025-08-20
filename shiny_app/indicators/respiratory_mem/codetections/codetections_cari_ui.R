
tagList(
  
  fluidRow(width = 12,
           metadataButtonUI("respiratory_codetections_cari"),
           linebreaks(1),
  ),
  
  fluidRow(width = 12,
           tagList(h2("CARI - Proportion of two pathogen co-detections"))),
  
  fluidRow(
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
  