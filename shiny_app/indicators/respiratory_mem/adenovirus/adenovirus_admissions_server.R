
metadataButtonServer(id="respiratory_adenovirus_admissions",
                     panel="Respiratory infection activity",
                     parent = session)


altTextServer("adenovirus_admissions_modal",
              title = "Adenovirus hospital admissions in Scotland",
              content = tags$ul(tags$li("This is a plot showing the number of adenovirus hospital admissions in Scotland."),
                                tags$li("The x axis shows the ISO week of sample, from week 40 to week 39. ",
                                        "Week 40 is typically the start of October and when the winter respiratory season starts."),
                                tags$li("The y axis shows the number of hospital admissions."),
                                tags$li("There is a trace for each of the following season from 2016/2017 to 2022/2023")))


# Adenovirus admissions table
output$adenovirus_admissions_table <- renderDataTable({
  adenovirus_admissions %>%
    arrange(desc(Date)) %>%
    select(Season, ISOWeek, Admissions) %>%
    mutate(Season = factor(Season),
           ISOWeek = factor(ISOWeek)) %>%
    rename(`ISO Week` = ISOWeek) %>%
    make_table(filter_cols = c(1,2))
})


# Adenovirus Adms plot
output$adenovirus_admissions_plot <- renderPlotly({
  adenovirus_admissions %>%
    create_adeno_adms_linechart()

})


# observeEvent(input$respiratory_season,
#              {
#                updatePickerInput(session, inputId = "respiratory_date",
#                                  choices = {Respiratory_AllData %>% filter(Season == input$respiratory_season) %>%
#                                      .$Date %>% unique() %>% as.Date() %>% format("%d %b %y")},
#                                  selected = {Respiratory_AllData %>% filter(Season == input$respiratory_season) %>%
#                                      .$Date %>% max() %>% as.Date() %>% format("%d %b %y")})
# 
#              }
)

