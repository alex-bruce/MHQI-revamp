age_rate_data_all_path <- age_rate_data_all_path %>% 
    left_join(date_reference, by = c("week_ending" = "date"))


age_rate_data_all_path <- age_rate_data_all_path %>%   
  mutate(
    week_ending = as_date(week_ending),
    age_band = factor(age_band, levels = c("<1", "1-4", "5-14", "15-44", "45-64", "65-74", "75+", "All Ages")))



create_pathogen_adms_age_plot <- function(data){
  
  yaxis_plots[["title"]] <- "Admission rate per 100k"
  xaxis_plots[["title"]] <- "Week ending"
  
  xaxis_plots[["rangeslider"]] <- list(type = "date")
  yaxis_plots[["fixedrange"]] <- FALSE
  yaxis_plots[["ticksuffix"]] <- "%"
  
  p <- plot_ly(data) %>%
    add_trace(x = ~week_ending, y = ~'Rate per 100k', split = ~age_band, text=~age_band,
              type="scatter", mode="lines",
              color=~age_band,
              colors=phs_colours(c("phs-blue", "phs-rust", "phs-green",
                                   "phs-purple", "phs-blue-50", "phs-magenta")),
              hovertemplate = paste0('<b>Week ending</b>: %{x}<br>',
                                     '<b>Age group</b>: %{text}<br>',
                                     '<b>Test positivity</b>: %{y}')
    ) %>%
    layout(margin = list(b = 100, t = 5),
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5),
           paper_bgcolor = phs_colours("phs-liberty-10"),
           plot_bgcolor = phs_colours("phs-liberty-10")) %>%
    
    config(displaylogo = FALSE, displayModeBar = TRUE,
           modeBarButtonsToRemove = bttn_remove)
  
  return(p)
  
}

  
  
  
  
  