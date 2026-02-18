
data_explore_server <- function(input, output, filtered_ic_summary_df, session){
  
  output$data_explore_plot <- renderPlot({
    
    grouping_var <- sym(input$grouping_var)
    filter_var <- sym(input$filter_var)
    filter_selected <- input$filter_select
  
    if (input$assessed_choice == "Yes"){
      prescribed_lag <- "Interested & Eligible"
    } else {
      prescribed_lag <- "PWH"
    }
    
    temp <- filtered_ic_summary_df |>
      group_by(Variable,!!grouping_var,!!filter_var) |>
      summarize(Value = sum(Value)) |>
      arrange(match(Variable,c('PWH', 'Assessed','Educated',
                               'Interested', 'Screened', 'Eligible', 
                               'Interested & Eligible',
                               'Prescribed', 'Initiated', 'Sustained'))) |>
      # this should be the grouping var (first) and filter var (second)
      group_by(!!grouping_var,!!filter_var) |>
      mutate(prev_lab = case_when(Variable == "PWH" ~ "PWH",
                                  Variable == "Educated" ~ "PWH",
                                  Variable == "Interested" ~ "Educated",
                                  Variable == "Screened" ~ "PWH",
                                  Variable == "Eligible" ~ "Screened",
                                  Variable == "Interested & Eligible" ~ "Assessed",
                                  Variable == "Prescribed" ~ prescribed_lag,
                                  .default =  lag(Variable)),
             prev = Value[match(prev_lab, Variable)]) |>
      group_by(Variable, !!filter_var) |>
      mutate(prev = if_else(Variable == "PWH",
                            sum(Value),
                            prev)) |>
      ungroup() |>
      mutate(Percent=if_else(prev == 0,NA,Value/prev)) |>
      ungroup() |>
      # indicator selection here as a filter
      filter(Variable == if_else(input$indicator != "Demographics",input$indicator,"PWH"),
             prev > 0) |>
      mutate(y_lab = fct_rev(str_c(!!grouping_var,"\n",Value," ",Variable," out of ",prev," ", prev_lab)),
             small_n = data.table::fifelse(prev < 10,"0","1")) 
      # this should be the filter variable being filtered
      
    # generate caption text
    group_var_levels <- filtered_ic_summary_df |> pull(!!grouping_var) |> levels()
    filter_var_levels <- filtered_ic_summary_df |> 
      pull(!!filter_var) |> as.factor() |> levels()
    all_var_levels <- expand_grid(group_var_levels, filter_var_levels) |>
      filter(filter_var_levels %in% input$filter_select) |>
      mutate(full_var = str_c(filter_var_levels," ",group_var_levels)) |>
      pull(full_var)
    
    group_var_present <- temp |> pull(!!grouping_var) |> unique()
    filter_var_present <- temp |> pull(!!filter_var) |> unique()
    all_var_present <- temp |> select(!!grouping_var,!!filter_var) |> 
      filter(!!filter_var %in% filter_selected) |>
      distinct() |>
      mutate(full_var = str_c(!!filter_var," ",!!grouping_var))|>
      pull(full_var)
    
    missing_filter_var_levels = filter_var_levels[!filter_var_levels %in% filter_var_present]
    missing_filter_var_levels = missing_filter_var_levels[!str_detect(missing_filter_var_levels, "Unknown")]
    
    missing_group_vars <- all_var_levels[!all_var_levels %in% all_var_present]
    missing_group_vars <- missing_group_vars[!str_detect(missing_group_vars,"Unknown")]
    caption_text = if_else(length(missing_group_vars)==0,"",
                           str_c("Gray bars have denominator <10. The following groups have n=0 ", 
                                 unique(temp$prev_lab),": ",
                                 paste(missing_filter_var_levels, collapse = "; "), "; ",
                                 paste(missing_group_vars, collapse = "; ")))
    
    chart <- temp |>
      filter(!!filter_var %in% filter_selected) |>
      ggplot(aes(y = y_lab, x= Percent)) +
      geom_col(aes(fill = small_n), width = 0.7) +
      geom_text(aes(label = paste0(round(Percent * 100), '%')),
                position = position_dodge(width = 0.9),
                hjust = -0.1,
                size=4, fontface = "bold") +
      labs(y = NULL, x = NULL,
           caption = caption_text) +
      scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1),
                         limits = c(0,1.5)) +
      scale_fill_manual(values= c("1" = "#08519C","0" = "gray")) + 
      facet_wrap(as.formula(paste("~", rlang::as_string(filter_var))),ncol = 1,
                 strip.position = "left",
                 scales = "free_y") + 
      theme_minimal(base_family = "Roboto") +
      theme(legend.position = 'none',
            text = element_text(size = 15),
            strip.placement = "outside",
            strip.text.y.left = element_text(angle = 0, size = 16,face = "bold",
                                             margin = margin(r = 10)),
            axis.text.y = element_text(size = 15,color = "black"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x  = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            panel.background = element_rect(color = "gray40"),
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0))
    
    output$data_explore_plot_download <- download_box("data_explore_plot",chart)
    
    chart_table <- temp |> 
      mutate({{filter_var}} := fct_relevel(factor({{filter_var}}),
                                           filter_selected)) |>
      arrange({{filter_var}}) |>
      rename(Reference = prev_lab,
             Numerator = Value,
             Denominator = prev) |>
      mutate(Percent = round(100*Percent,2)) |>
      select({{filter_var}},{{grouping_var}},
             Variable, Reference,
             Numerator,Denominator, Percent)
  
    output$data_explore_table_download <- download_table("data_explore_table",chart_table)
    
    chart
  })
  
}