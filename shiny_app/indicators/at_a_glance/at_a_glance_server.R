### Setup data -----
#cases labels-(matches covid-cases data set)

latest_week_cases_title <- Resp_cases_recent_weeks %>%
  tail(1) %>%
  select(DateThisWeek) %>% 
  pull()

latest_week_cases_title %<>%
    format("%d %b %y")

previous_week_cases_title <- Resp_cases_recent_weeks %>%
  tail(1) %>%
  select(DateLastWeek) %>% 
  pull()

previous_week_cases_title %<>%
  format("%d %b %y")

# admissions labels- (matches Respiratory_admissions_summary data set)
latest_week_admissions_title <- admissions_scotland %>%
  tail(1) %>%
  select(WeekEnding)

# Convert to the correct format
latest_week_admissions_title$WeekEnding<- format(latest_week_admissions_title$WeekEnding, "%d %b %y")

# make it a value
latest_week_admissions_title <- latest_week_admissions_title$WeekEnding

previous_week_admissions_title <- admissions_scotland %>%
  filter(Pathogen=='RSV') %>%
  tail(2) %>%
  filter(WeekEnding== min(WeekEnding)) %>%
  select(WeekEnding)

# Convert to correct format
 previous_week_admissions_title$WeekEnding<- format(previous_week_admissions_title$WeekEnding, "%d %b %y")

# make it a value
previous_week_admissions_title <- previous_week_admissions_title$WeekEnding


## Occupancy labels
latest_week_occupancy_title <- occupancy_rapid_new %>%
  tail(1) %>%
  select(WeekEnding) %>%
  mutate(WeekEnding = format(WeekEnding, "%d %b %y")) %>%
  .$WeekEnding

previous_week_occupancy_title <- occupancy_rapid_new %>%
  tail(4) %>%
  select(WeekEnding) %>%
  filter(WeekEnding == min(WeekEnding)) %>%
  mutate(WeekEnding = format(WeekEnding, "%d %b %y")) %>%
  .$WeekEnding


### Cases

cases_intro <- Resp_cases_recent_weeks %>% 
  select(Pathogen, CasesLastWeek, RateLastWeek, CasesThisWeek, RateThisWeek) %>% 
  mutate(Pathogen = recode(Pathogen, 
                           "Coronavirus"="Seasonal Coronavirus (non-COVID-19)",
                           "Covid-19" = "COVID-19",
                           "Human Metapneumovirus" = "Human metapneumovirus",
                           "Parainfluenza Virus" = "Parainfluenza virus",
                           "Respiratory Syncytial Virus" = "Respiratory syncytial virus")) %>%
  mutate(Pathogen =  factor(Pathogen, levels = c("COVID-19", "Influenza", "Respiratory syncytial virus", "Adenovirus", 
                                                 "Human metapneumovirus",
                                                 "Mycoplasma pneumoniae", "Parainfluenza virus", "Rhinovirus",
                                                 "Seasonal Coronavirus (non-COVID-19)"))) %>%
  arrange(Pathogen)

colnames(cases_intro)[4] <- paste("Number of cases (", as.character(latest_week_cases_title),")")
colnames(cases_intro)[5] <- paste("Rate per 100,000 population (", as.character(latest_week_cases_title),")")
colnames(cases_intro)[2] <- paste("Number of cases (", as.character(previous_week_cases_title),")")
colnames(cases_intro)[3] <- paste("Rate per 100,000 population (", as.character(previous_week_cases_title),")")

###Hosp Adms
# create intermediate data frames for covid,flu and rsv using Respiratory_admissions_summary dataframe
# (same dataframe as that used for admissions graph below)
# limit  each data frame to the last 2 weeks and add a flag latest week /previous week
# pivot the tables by flags
# Order the pathogens in a consistent manner
# join into one table
# rename and add week titles for the dashboard

hosp_adms_intro <- admissions_scotland %>%
  filter(!Pathogen %in% c("Influenza A", "Influenza B")) %>%
  tail(18) %>%
  mutate(flag = ifelse(WeekEnding==max(WeekEnding), "latest_week", "previous_week")) %>%
  select(-WeekEnding) %>%
  select(flag, Pathogen, admissions_number = NumberAdmissionsPerWeek,
         admissions_rate=RateAdmissionsPerWeek) %>%
   pivot_wider(names_from = flag, values_from = admissions_number:admissions_rate) %>%
  select(Pathogen,
         'Number of admissions (previous week)'= admissions_number_previous_week,
         'Rate of admissions per 100,000 population (previous week)'= admissions_rate_previous_week,
         'Number of admissions (latest week)'= admissions_number_latest_week,
         'Rate of admissions per 100,000 population (latest week)'= admissions_rate_latest_week  ) %>%
  mutate(Pathogen =  factor(Pathogen, levels = c("COVID-19", "Influenza (All)", "RSV", "Adenovirus",  "HMPV",  "Mycoplasma pneumoniae", "Parainfluenza (Any Type)", "Rhinovirus", "Seasonal coronavirus"))) %>%
  arrange(Pathogen) %>%
  mutate(Pathogen=if_else(Pathogen=="RSV", "Respiratory syncytial virus", Pathogen)) %>% 
  mutate(Pathogen=if_else(Pathogen=="Seasonal coronavirus", "Seasonal Coronavirus (non COVID-19)", Pathogen)) %>% 
  mutate(Pathogen=if_else(Pathogen=="HMPV", "Human Metapneumovirus", Pathogen)) %>% 
  mutate(Pathogen=if_else(Pathogen=="Parainfluenza (Any Type)", "Parainfluenza", Pathogen)) %>% 
  mutate(Pathogen=if_else(Pathogen=="Influenza (All)", "Influenza", Pathogen))




colnames(hosp_adms_intro)[4] <- paste("Number of admissions (", as.character(latest_week_admissions_title),")")
colnames(hosp_adms_intro)[5] <- paste("Rate of admissions per 100,000 population (", as.character(latest_week_admissions_title),")")
colnames(hosp_adms_intro)[2] <- paste("Number of admissions (", as.character(previous_week_admissions_title),")")
colnames(hosp_adms_intro)[3] <- paste("Rate of admissions per 100,000 population (", as.character(previous_week_admissions_title),")")

###Inpatients

inpatients_intro <- occupancy_rapid_new %>%
  tail(6) %>%
  mutate(flag= if_else(WeekEnding == max(WeekEnding),"Latest Week", "Previous Week")) %>% #add flags
  select(Pathogen, flag, SevenDayAverageInpatients) %>%
  pivot_wider(names_from = flag, values_from = SevenDayAverageInpatients) %>%
  mutate(Pathogen = case_when(Pathogen=="Influenza (All)" ~ "Influenza",
                              TRUE ~ Pathogen)) 

colnames(inpatients_intro)[3] <- paste("Seven day average number (", as.character(latest_week_occupancy_title),")")
colnames(inpatients_intro)[2] <- paste("Seven day average number (", as.character(previous_week_occupancy_title),")")

### Data tables -----

# Cases table
output$cases_intro_table <- renderDataTable({
  cases_intro %>%
   make_summary_table(add_separator_cols_1dp = c(3,5))

})

# Hospital admissions table
output$hosp_adms_intro_table <- renderDataTable({
  hosp_adms_intro %>%
    make_summary_table(add_separator_cols_1dp = c(3,5))

})

# Inpatients table
output$inpatients_intro_table <- renderDataTable({
  inpatients_intro%>%
    make_summary_table()

})

altTextServer("adms_summary_modal",
              title = "Number of acute hospital admissions due to each pathogen",
              content = tags$ul(tags$li("This is a plot of the number of acute hospital admissions due to each of a variety of pathogens."),
                                tags$li("The x axis is the week ending"),
                                tags$li("The y axis is the number of admissions"),
                                tags$li("There are nine traces representing the number of admissions due to each pathogen.")
              )
)

altTextServer("cari_summary_modal",
              title = "Test positivity in the CARI sentinel surveillance programme",
              content = tags$ul(tags$li("This is a plot showing the test positivity rate of any and individual pathogens in the Community Acute Respiratory Infection (CARI) surveillance programme."),
                                tags$li("The x axis is the week ending date, starting 09 October 2022."),
                                tags$li("The y axis is the test positivity rate."),
                                tags$li("The plot contains a trace showing the test positivity rate for the selected pathogen(s).")))


### Plot -----
output$hosp_adms_intro_plot <- renderPlotly({
  admissions_scotland %>%
    mutate(Pathogen=if_else(Pathogen=="RSV", "Respiratory syncytial virus", Pathogen)) %>% 
    mutate(Pathogen=if_else(Pathogen=="HMPV", "Human Metapneumovirus", Pathogen)) %>% 
    mutate(Pathogen=if_else(Pathogen=="Parainfluenza (Any Type)", "Parainfluenza", Pathogen)) %>% 
    mutate(Pathogen=if_else(Pathogen=="Influenza (All)", "Influenza", Pathogen)) %>% 
    mutate(Pathogen=if_else(Pathogen=="Mycoplasma pneumoniae", "Mycoplasma Pneumoniae", Pathogen)) %>% 
    mutate(Pathogen=if_else(Pathogen=="Seasonal coronavirus", "Seasonal Coronavirus (non-COVID-19)", Pathogen)) %>% 
    mutate(Pathogen =  factor(Pathogen, levels = c("COVID-19", "Influenza", "Respiratory syncytial virus", "Adenovirus",  
                                                               "Human Metapneumovirus",  "Mycoplasma Pneumoniae", 
                                                               "Parainfluenza", "Rhinovirus", "Seasonal Coronavirus (non-COVID-19)"))) %>%
    arrange(Pathogen) %>%

    #mutate(WeekEnding = ymd(WeekEnding)) %>%
    make_adms_summary_plot()#create_summary_adms_linechart()

})


### Plot -----
output$cari_intro_plot <- renderPlotly({
  cari_at_a_glance %>%
    filter(Pathogen %in% input$cari_selected_pathogen) %>%
    create_cari_pathogen_linechart()
  
})



