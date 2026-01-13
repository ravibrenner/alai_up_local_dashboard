

main_page_server <- function(input, output, tbl,ic_summary_df,selected_site,cab_master_df, session){
  
  selected_year <- reactive({
    if (is.null(input$active_year)) {
      2025
    } else {
      input$active_year
    }
  })
  
  temp <- ic_summary_df |>
    group_by(Variable) |>
    summarise(Value=sum(Value,na.rm = T)) 

  count_df <- ic_summary_df |>
    group_by(Variable) |> 
    summarize(Value = sum(Value))
  
  pwh_count <- count_df  |>
    filter(Variable == "PWH") |> pull()
  
  assessed_count <- count_df  |>
    filter(Variable == "Assessed") |> pull()
  
  educated_count <- count_df  |>
    filter(Variable == "Educated") |> pull()
  
  interested_count <- count_df  |>
    filter(Variable == "Interested") |> pull()
  
  screened_count <- count_df  |>
    filter(Variable == "Screened") |> pull()
  
  eligible_count <- count_df  |>
    filter(Variable == "Eligible") |> pull()
  
  interested_eligible_count <- count_df  |>
    filter(Variable == "Interested & Eligible") |> pull()
  
  prescribed_count <- count_df  |>
    filter(Variable == "Prescribed") |> pull()
  
  initiated_count <- count_df  |>
    filter(Variable == "Initiated") |> pull()
  
  sustained_count <- count_df  |>
    filter(Variable == "Sustained") |> pull()
  
  output$overall_n = renderUI({
    HTML(paste0(
      "<span style='color: #FE5000; font-size: 36px;'>", 
      pwh_count, 
      "</span> people with HIV received care at ", 
      selected_site, 
      " in ", selected_year()
    ))
  })
  
  demo_sections <- list(
    sex1       = list(var = "sex_birth",        label = "sex_demographics"),
    race1      = list(var = "race",             label = "race_demographics"),
    ethnicity1 = list(var = "ethnicity",        label = "ethnicity_demographics"),
    age1       = list(var = "age_cat",          label = "age_demographics"),
    insurance1 = list(var = "insurance_status", label = "insurance_demographics"),
    housing1   = list(var = "housing_status",   label = "housing_demographics")
  )
  
  n_bars <- list()
  
  lapply(names(demo_sections), function(id) {
    section <- demo_sections[[id]]
    var_str <- section$var
    label <- section$label
    
    n_bars[[id]] <- reactiveVal(1)
    
    output[[paste0(id, "_plot")]] <- renderPlot({
      base_size <- 14
      
      p <- demo_plot(tbl, var_str, base_size,
                     selected_site = selected_site, selected_year = selected_year(),
                     by_cab_status = FALSE)
      
      output[[paste0(id, "_plot_download")]] <- download_box(label, p,nrow(p$data))
      output[[paste0(id, "_table_download")]] <- download_table(label, p$data)
      output[[paste0(id,"_download_ui")]] <- renderUI({
        tagList(
          downloadButton(outputId = paste0(id, "_plot_download"), label = "Download plot"),
          downloadButton(outputId = paste0(id, "_table_download"),  
                         label = "Download table",
                         icon = icon("table"))
        )
      })
      
      n_bars[[id]](nrow(p$data))
      p
    }, height = function() {
      50 * n_bars[[id]]() + 50
    })
  })
  
  n_bars_demo_keypop <<- reactiveVal(1)
  output$keypop1_plot <- renderPlot({
    base_size <- 14
    
    title_text = str_c("PWH at ", selected_site,
                       " active in ",selected_year(),
                       " by key populations")
    caption_text = "MSM: Men who have sex with men; IDU: Injection drug use. Note that there may be overlap between these categories."
    
    temp <- tbl |> 
      summarize(n = n(),
                MSM = sum(risk_msm == 1,na.rm = T), 
                IDU = sum(risk_idu == 1,na.rm = T),
                TG_NB = sum(gender_id %in% c(3,4,5),na.rm = T)) |>
      pivot_longer(cols = !c(n),
                   names_to = "key_pop",
                   values_to = "count") |>
      mutate(key_pop = case_when(
               key_pop == "TG_NB" ~ "Transgender/nonbinary",
               .default = key_pop
             ),
             pct = count/n,
             bar_text = str_c(round(100*pct,0),"%"),
             axis_text = str_c(key_pop," (",count," out of ",n," PWH)")) 
    
    p <- temp |>
      ggplot(aes(x = 100*pct,y = axis_text)) + 
      geom_bar(position = "dodge",stat = "identity", fill = "#08519C", width = 0.7) + 
      geom_text(aes(label = bar_text),
                size = 14/2.5,
                hjust = -0.1) +
      scale_x_continuous(limits = c(0,100)) + 
      scale_y_discrete(labels = \(x) str_wrap(x, width = 70)) +
      facet_wrap(~key_pop, 
                 ncol = 1, scales = "free_y",
                 strip.position = "left") +
      labs(title = title_text,
           x = NULL, y = NULL,
           caption = str_wrap(caption_text,120)) + 
      theme_minimal(base_size = 14,
                    base_family = "Roboto") +
      theme(plot.title = element_text(size = rel(1.3)),
            plot.title.position = "plot",
            strip.placement = "outside",
            strip.text.y.left = element_text(angle = 0, size = rel(1.5), hjust = 1),
            axis.title = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_text(hjust = 1, size = rel(1.4), color = "black"),
            plot.margin = margin(5,0,5,0),
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0),
            legend.position = "none")
    
    output$keypop1_plot_download <- download_box("keypop_demographics_lai", p,nrow(p$data))
    output$keypop1_table_download <- download_table("keypop_demographics_lai", p$data)
    output$keypop1_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "keypop1_plot_download", label = "Download plot"),
        downloadButton(outputId = "keypop1_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    n_bars_demo_keypop(nrow(p$data))
    p
  }, height = function() {
    30 * n_bars_demo_keypop() + 100
  })
  
  demo_sections_b <- list(
    sex1b       = list(var = "sex_birth",        label = "sex_demographics_lai"),
    race1b      = list(var = "race",             label = "race_demographics_lai"),
    ethnicity1b = list(var = "ethnicity",        label = "ethnicity_demographics_lai"),
    age1b       = list(var = "age_cat",          label = "age_demographics_lai"),
    insurance1b = list(var = "insurance_status", label = "insurance_demographics_lai"),
    housing1b   = list(var = "housing_status",   label = "housing_demographics_lai")
  )
  
  n_bars <- list()
  
  lapply(names(demo_sections_b), function(id) {
    section <- demo_sections_b[[id]]
    var_str <- section$var
    label <- section$label
    
    n_bars[[id]] <- reactiveVal(1)
    
    output[[paste0(id, "_plot")]] <- renderPlot({
      base_size <- 14
      
      p <- demo_plot(tbl, var_str, base_size,
                     selected_site = selected_site, selected_year = selected_year(),
                     by_cab_status = TRUE)
      
      output[[paste0(id, "_plot_download")]] <- download_box(label, p,nrow(p$data))
      output[[paste0(id, "_table_download")]] <- download_table(label, p$data)
      output[[paste0(id,"_download_ui")]] <- renderUI({
        tagList(
          downloadButton(outputId = paste0(id, "_plot_download"), label = "Download plot"),
          downloadButton(outputId = paste0(id, "_table_download"),  
                         label = "Download table",
                         icon = icon("table"))
        )
      })
      
      n_bars[[id]](nrow(p$data))
      p
    }, height = function() {
      30 * n_bars[[id]]() + 50
    })
  })
  
  n_bars_demo_keypop <<- reactiveVal(1)
  output$keypop1b_plot <- renderPlot({
    base_size <- 14
    
    title_text = str_c("PWH at ", selected_site,
                       " active in ",selected_year(),
                       " by key populations")
    caption_text = "MSM: Men who have sex with men; IDU: Injection drug use. Note that there may be overlap between these categories."
    
    temp <- tbl |> 
      mutate(ever_on_cab = if_else(ever_on_cab == 0,
                                   "Never on LAI ART","Ever on LAI ART")) |>
      summarize(.by = ever_on_cab,
                n = n(),
                MSM = sum(risk_msm == 1,na.rm = T), 
                IDU = sum(risk_idu == 1,na.rm = T),
                TG_NB = sum(gender_id %in% c(3,4,5),na.rm = T)) |>
      pivot_longer(cols = !c(n,ever_on_cab),
                   names_to = "key_pop",
                   values_to = "count") |>
      mutate(.by = ever_on_cab,
             key_pop = case_when(
               key_pop == "TG_NB" ~ "Transgender/nonbinary",
               .default = key_pop
             ),
             pct = count/n,
             bar_text = str_c(round(100*pct,0),"%"),
             axis_text = str_c(ever_on_cab," (",count," out of ",n," PWH)")) 
    
    p <- temp |>
      ggplot(aes(x = 100*pct,y = axis_text)) + 
      geom_bar(position = "dodge",stat = "identity", aes(fill = ever_on_cab), width = 0.7) + 
      geom_text(aes(label = bar_text),
                size = 14/2.5,
                hjust = -0.1) +
      scale_x_continuous(limits = c(0,100)) + 
      scale_y_discrete(labels = \(x) str_wrap(x, width = 70)) +
      scale_fill_manual(values = c(
        "Never on LAI ART" = "#9ECAE1",
        "Ever on LAI ART" = "#08519C"
      )) + 
      facet_wrap(~key_pop, 
                 ncol = 1, scales = "free_y",
                 strip.position = "left") +
      labs(title = title_text,
           x = NULL, y = NULL,
           caption = str_wrap(caption_text,120)) + 
      theme_minimal(base_size = 14,
                    base_family = "Roboto") +
      theme(plot.title = element_text(size = rel(1.3)),
            plot.title.position = "plot",
            strip.placement = "outside",
            strip.text.y.left = element_text(angle = 0, size = rel(1.5), hjust = 1),
            axis.title = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_text(hjust = 1, size = rel(1.4), color = "black"),
            plot.margin = margin(5,0,5,0),
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0),
            legend.position = "none")
    
    output$keypop1b_plot_download <- download_box("keypop_demographics_lai", p,nrow(p$data))
    output$keypop1b_table_download <- download_table("keypop_demographics_lai", p$data)
    output$keypop1b_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "keypop1b_plot_download", label = "Download plot"),
        downloadButton(outputId = "keypop1b_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    n_bars_demo_keypop(nrow(p$data))
    p
  }, height = function() {
    30 * n_bars_demo_keypop() + 100
  })
  
  n_bars_caregap <- reactiveVal(1)
  output$lai_care_gap_plot <- renderPlot({
    
    base_size <- 14
    
    ic_df <- ic_summary_df |>
      filter(site != "Harlem") |>
      group_by(site, Variable) |>
      summarise(Value=sum(Value,na.rm = T)) |>
      arrange(match(Variable,c('PWH', 'Assessed','Educated',
                               'Interested', 'Screened', 'Eligible', 
                               'Interested & Eligible',
                               'Prescribed', 'Initiated', 'Sustained'))) |>
      group_by(site) |>
      mutate(prev_lab = case_when(Variable == "Educated" ~ "PWH",
                                  Variable == "Interested" ~ "Educated",
                                  Variable == "Screened" ~ "PWH",
                                  Variable == "Eligible" ~ "Screened",
                                  Variable == "Interested & Eligible" ~ "Assessed",
                                  .default =  lag(Variable)),
             prev = Value[match(prev_lab, Variable)]) |>
      mutate(
        Variable = if_else(Variable != "PWH",str_to_lower(Variable),Variable) ,
        prev_lab = if_else(prev_lab != "PWH",str_to_lower(prev_lab),prev_lab),
        Percent=Value/prev,
        x_lab = str_c(Value," patients were ",Variable," out of ",prev," ", prev_lab)) |>
      filter(Variable %in% c("PWH","assessed","interested & eligible","prescribed","initiated","sustained")) |>
      group_by(Variable) |>
      summarize(low_pct = min(Percent),
                low_site = max(site[Percent == low_pct]),
                high_pct = max(Percent),
                high_site = max(site[Percent == high_pct]),
                Value = sum(Value),
                Variable = unique(Variable),
                prev_lab = unique(prev_lab),
                prev = sum(prev)) |>
      mutate(Percent = Value/prev,
             x_lab = str_c(Value," patients were ",Variable," out of ",prev," ", prev_lab),
             low_site = str_c(low_site, ", ",round(100*low_pct),"%"),
             high_site = str_c(high_site, ", ",round(100*high_pct),"%")) |>
      arrange(match(Variable,c('PWH','assessed', 'interested & eligible', 
                               'prescribed', 'initiated', 'sustained'))) |>
      mutate(x_order = row_number(),
             x_lab = fct_reorder(x_lab,x_order))
    
    bar_names <- ic_df$x_lab[-1]
    max_y = max(ic_df$high_pct,na.rm = T)
    text_size = base_size / 2.5
    
    # size goal: base size 12. text size 4.8. axis text size 18 (12*1.5)
    
    chart <- ggplot(ic_df, aes(y = x_lab, x = Percent)) +
      geom_col(fill="#08519C") +
      geom_text(aes(label = paste0(round(Percent * 100), '%')),
                position = position_dodge(width = 0.9),
                hjust = -0.5,
                size=text_size,
                family = "Roboto") +
      scale_y_discrete(limits = rev(bar_names),
                       labels = \(x) str_wrap(x, width = 30)) +
      labs(y = NULL, x = NULL,
           caption = str_wrap("JMFC not included.\nError bars have been used to show range of values from lowest to highest by site. A narrow interval means less variability by site. Prescribed may be above 100% if patients not clinically eligible are prescribed. Initiated may be above 100% if patients switched into the clinic while on LAI.",
                              width = 150)) +
      scale_x_continuous(labels = scales::percent, breaks = seq(0, max_y, 0.2),
                         limits = c(0,max_y + 0.15)) +
      expand_limits(y = 1) +
      theme_minimal(base_size = base_size,
                    base_family = "Roboto") + 
      theme(legend.position = 'none',
            axis.text.y = element_text(size = rel(1.5),color = "black"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            plot.caption = element_text(hjust = 0),
            plot.caption.position = "plot") 
    
    if (length(unique(ic_summary_df$site)) > 1){
      chart <- chart + 
        geom_errorbar(aes(y = as.numeric(fct_rev(x_lab)) + 0.25,
                          xmin = low_pct,xmax = high_pct),
                      color = "black",width = .25)
    } else {
      chart <- chart
    }
    
    n_bars_caregap(nrow(chart$data))
    output$lai_care_gap_plot_download <- download_box("lai_care_gap",chart,nrow(chart$data))
    output$lai_care_gap_table_download <- download_table("lai_care_gap",chart$data)
    output$lai_care_gap_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "lai_care_gap_plot_download", label = "Download plot"),
        downloadButton(outputId = "lai_care_gap_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    chart
  },height = function(){
    50 * n_bars_caregap() + 100
  })
  
  output$int_elig_table <- render_gt({
    gt_tbl <- int_elig_table_func(tbl, input$int_elig_pct)
    
    output$int_elig_table_download <- download_table("assessed_outcomes",gt_tbl$`_data`)
    gt_tbl

  })

  
  plot_sections <- list(
    sex2       = list(var = "sex_birth",        label = "sex",       input_var = "assessed"),
    race2      = list(var = "race",             label = "race",      input_var = "assessed"),
    ethnicity2 = list(var = "ethnicity",        label = "ethnicity", input_var = "assessed"),
    age2       = list(var = "age_cat",          label = "age",       input_var = "assessed"),
    insurance2 = list(var = "insurance_status", label = "insurance", input_var = "assessed"),
    housing2   = list(var = "housing_status",   label = "housing",   input_var = "assessed"),
    
    sex3       = list(var = "sex_birth",        label = "sex",       input_var = "educated"),
    race3      = list(var = "race",             label = "race",      input_var = "educated"),
    ethnicity3 = list(var = "ethnicity",        label = "ethnicity", input_var = "educated"),
    age3       = list(var = "age_cat",          label = "age",       input_var = "educated"),
    insurance3 = list(var = "insurance_status", label = "insurance", input_var = "educated"),
    housing3   = list(var = "housing_status",   label = "housing",   input_var = "educated"),
    
    sex4       = list(var = "sex_birth",        label = "sex",       input_var = "interested"),
    race4      = list(var = "race",             label = "race",      input_var = "interested"),
    ethnicity4 = list(var = "ethnicity",        label = "ethnicity", input_var = "interested"),
    age4       = list(var = "age_cat",          label = "age",       input_var = "interested"),
    insurance4 = list(var = "insurance_status", label = "insurance", input_var = "interested"),
    housing4   = list(var = "housing_status",   label = "housing",   input_var = "interested"),
    
    sex5       = list(var = "sex_birth",        label = "sex",       input_var = "screened"),
    race5      = list(var = "race",             label = "race",      input_var = "screened"),
    ethnicity5 = list(var = "ethnicity",        label = "ethnicity", input_var = "screened"),
    age5       = list(var = "age_cat",          label = "age",       input_var = "screened"),
    insurance5 = list(var = "insurance_status", label = "insurance", input_var = "screened"),
    housing5   = list(var = "housing_status",   label = "housing",   input_var = "screened"),
    
    sex6       = list(var = "sex_birth",        label = "sex",       input_var = "eligible"),
    race6      = list(var = "race",             label = "race",      input_var = "eligible"),
    ethnicity6 = list(var = "ethnicity",        label = "ethnicity", input_var = "eligible"),
    age6       = list(var = "age_cat",          label = "age",       input_var = "eligible"),
    insurance6 = list(var = "insurance_status", label = "insurance", input_var = "eligible"),
    housing6   = list(var = "housing_status",   label = "housing",   input_var = "eligible"),
    
    sex7       = list(var = "sex_birth",        label = "sex",       input_var = "prescribed"),
    race7      = list(var = "race",             label = "race",      input_var = "prescribed"),
    ethnicity7 = list(var = "ethnicity",        label = "ethnicity", input_var = "prescribed"),
    age7       = list(var = "age_cat",          label = "age",       input_var = "prescribed"),
    insurance7 = list(var = "insurance_status", label = "insurance", input_var = "prescribed"),
    housing7   = list(var = "housing_status",   label = "housing",   input_var = "prescribed"),
    
    sex8       = list(var = "sex_birth",        label = "sex",       input_var = "initiated"),
    race8      = list(var = "race",             label = "race",      input_var = "initiated"),
    ethnicity8 = list(var = "ethnicity",        label = "ethnicity", input_var = "initiated"),
    age8       = list(var = "age_cat",          label = "age",       input_var = "initiated"),
    insurance8 = list(var = "insurance_status", label = "insurance", input_var = "initiated"),
    housing8   = list(var = "housing_status",   label = "housing",   input_var = "initiated"),
    
    sex9       = list(var = "sex_birth",        label = "sex",       input_var = "sustained"),
    race9      = list(var = "race",             label = "race",      input_var = "sustained"),
    ethnicity9 = list(var = "ethnicity",        label = "ethnicity", input_var = "sustained"),
    age9       = list(var = "age_cat",          label = "age",       input_var = "sustained"),
    insurance9 = list(var = "insurance_status", label = "insurance", input_var = "sustained"),
    housing9   = list(var = "housing_status",   label = "housing",   input_var = "sustained")
  )
  
  n_bars <- list()
  
  lapply(names(plot_sections), function(id) {
    section <- plot_sections[[id]]
    input_var <- section$input_var
    page_selected <- paste0(input_var,"_page")
    var_str <- section$var
    label <- paste0(section$label,"_",section$input_var)
    
    n_bars[[id]] <- reactiveVal(1)
    
    output[[paste0(id, "_plot")]] <- renderPlot({
      req(input$sidebar == page_selected)

      base_size <- 14
      p <- ic_var_plot(ic_summary_df, input_var, by_group = T, group_var = var_str, base_size_in = base_size,
                       selected_site = selected_site, selected_year = selected_year())
      
      output[[paste0(id, "_plot_download")]] <- download_box(label, p,nrow(p$data))
      output[[paste0(id, "_table_download")]] <- download_table(label, p$data)
      output[[paste0(id,"_download_ui")]] <- renderUI({
        tagList(
          downloadButton(outputId = paste0(id, "_plot_download"), label = "Download plot"),
          downloadButton(outputId = paste0(id, "_table_download"),  
                         label = "Download table",
                         icon = icon("table"))
        )
      })

      n_bars[[id]](nrow(p$data))
      p
    }, height = function() {
      50 * n_bars[[id]]() + 100
    })
  })
  
  output$assessed_n <-  renderUI({
    HTML(paste0(
      "<span style='color: #FE5000; font-size: 36px;'>", 
      assessed_count, 
      "</span> people were assessed out of ",
      pwh_count," people with HIV who received care at ", 
      selected_site,  
      " in ", selected_year()
    ))
  })
  
  n_bars_assessed_overall <<- reactiveVal(1)
  output$assessed_overall_plot <- renderPlot({
    base_size <- 14
    
    p <- ic_var_plot(ic_summary_df, "assessed",by_group = F, base_size_in = base_size,
                     selected_site = selected_site, selected_year = selected_year())
    
    output$assessed_overall_plot_download <- download_box("assessed_overall", p,nrow(p$data))
    output$assessed_overall_table_download <- download_table("assessed_overall", p$data)
    output$assessed_overall_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "assessed_overall_plot_download", label = "Download plot"),
        downloadButton(outputId = "assessed_overall_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    n_bars_assessed_overall(nrow(p$data))
    p
  }, height = function() {
    50 * n_bars_assessed_overall() + 50
  })
  
  n_bars_assessed_keypop <<- reactiveVal(1)
  output$keypop2_plot <- renderPlot({
    base_size <- 14
    
    p <- key_pop_var_plot(ic_summary_df,"assessed",
                          base_size_in = base_size,
                          selected_site = selected_site, selected_year = selected_year())
    
    output$keypop2_plot_download <- download_box("assessed_keypop", p,nrow(p$data))
    output$keypop2_table_download <- download_table("assessed_keypop", p$data)
    output$keypop2_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "keypop2_plot_download", label = "Download plot"),
        downloadButton(outputId = "keypop2_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    n_bars_assessed_keypop(nrow(p$data))
    p
  }, height = function() {
    50 * n_bars_assessed_keypop() + 100
  })
  
  output$time2_plot <- renderPlot({
    base_size <- 14
    
    p <- tbl |>
      select(alai_up_uid, 
             contains("icab_rpv")&(contains("counsel")|contains("screen")) & contains("date")) |>
      pivot_longer(cols = contains("icab_rpv")&(contains("counsel")|contains("screen")) & contains("date"),
                   names_to = "event",
                   values_to = "date") |>
      plot_outcome_by_month("Number of people assessed by month based on most recent assessment date",
                            base_size_in = base_size)
    
    output$time2_plot_download <- download_box("assessed_time",p)
    output$time2_table_download <- download_table("assessed_time",p$data)
    output$time2_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "time2_plot_download", label = "Download plot"),
        downloadButton(outputId = "time2_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    p
  }, height = 400)
  
  output$time2_event_plot <- renderPlot({
    base_size <- 14
    
    p <- tbl |>
      select(alai_up_uid, 
             contains("icab_rpv")&(contains("counsel")|contains("screen")) & contains("date")) |>
      pivot_longer(cols = contains("icab_rpv")&(contains("counsel")|contains("screen")) & contains("date"),
                   names_to = "event",
                   values_to = "date") |>
      plot_outcome_by_month("Number of assessment encounters by month",
                            base_size_in = base_size,
                            by_person = F)
    
    output$time2_event_plot_download <- download_box("assessed_time_event",p)
    output$time2_event_table_download <- download_table("assessed_time_event",p$data)
    output$time2_event_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "time2_event_plot_download", label = "Download plot"),
        downloadButton(outputId = "time2_event_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    p
  }, height = 400)
  
  
  output$educated_n <-  renderUI({
    HTML(paste0(
      "<span style='color: #FE5000; font-size: 36px;'>", 
      educated_count, 
      "</span> people were educated out of ",
      pwh_count," people with HIV who received care at ", 
      selected_site,  
      " in ", selected_year()
    ))
  })
  
  n_bars_educated_overall <<- reactiveVal(1)
  output$educated_overall_plot <- renderPlot({
    base_size <- 14
    
    p <- ic_var_plot(ic_summary_df, "educated",by_group = F, base_size_in = base_size,
                     selected_site = selected_site, selected_year = selected_year())
    
    output$educated_overall_plot_download <- download_box("educated_overall", p,nrow(p$data))
    output$educated_overall_table_download <- download_table("educated_overall", p$data)
    output$educated_overall_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "educated_overall_plot_download", label = "Download plot"),
        downloadButton(outputId = "educated_overall_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })

    n_bars_educated_overall(nrow(p$data))
    p
  }, height = function() {
    50 * n_bars_educated_overall() + 50
  })
  
  n_bars_educated_keypop <<- reactiveVal(1)
  output$keypop3_plot <- renderPlot({
    base_size <- 14
    
    p <- key_pop_var_plot(ic_summary_df,"educated",
                          base_size_in = base_size,
                          selected_site = selected_site, selected_year = selected_year())
    
    output$keypop3_plot_download <- download_box("educated_keypop", p,nrow(p$data))
    output$keypop3_table_download <- download_table("educated_keypop", p$data)
    output$keypop3_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "keypop3_plot_download", label = "Download plot"),
        downloadButton(outputId = "keypop3_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    n_bars_educated_keypop(nrow(p$data))
    p
  }, height = function() {
    50 * n_bars_educated_keypop() + 100
  })
  
  output$time3_plot <- renderPlot({
    base_size <- 14
    
    p <- tbl |> 
      select(alai_up_uid, 
             contains("icab_rpv")&(contains("counsel")) & contains("date")) |>
      pivot_longer(cols = contains("icab_rpv")&(contains("counsel")) & contains("date"),
                   names_to = "event",
                   values_to = "date") |>
      plot_outcome_by_month("Number of people by month of most recent education",base_size_in = base_size)
    
    output$time3_plot_download <- download_box("educated_time",p)
    output$time3_table_download <- download_table("educated_time",p$data)
    output$time3_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "time3_plot_download", label = "Download plot"),
        downloadButton(outputId = "time3_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    p
  }, height = 400)
  
  output$time3_event_plot <- renderPlot({
    base_size <- 14
    
    p <- tbl |> 
      select(alai_up_uid, 
             contains("icab_rpv")&(contains("counsel")) & contains("date")) |>
      pivot_longer(cols = contains("icab_rpv")&(contains("counsel")) & contains("date"),
                   names_to = "event",
                   values_to = "date") |>
      plot_outcome_by_month("Number of education encounters by month",base_size_in = base_size,
                            by_person = F)
    
    output$time3_event_plot_download <- download_box("educated_time_event",p)
    output$time3_event_table_download <- download_table("educated_time_event",p$data)
    output$time3_event_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "time3_event_plot_download", label = "Download plot"),
        downloadButton(outputId = "time3_event_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    p
  }, height = 400)
  
  
  output$interested_n <-  renderUI({
    HTML(paste0(
      "<span style='color: #FE5000; font-size: 36px;'>", 
      interested_count, 
      "</span> people were interested out of ",
      educated_count," people educated at ", 
      selected_site,  
      " among PWH active in ", selected_year()
    ))
  })
  
  n_bars_interested_overall <<- reactiveVal(1)
  output$interested_overall_plot <- renderPlot({
    base_size <- 14
    
    p <- ic_var_plot(ic_summary_df, "interested",by_group = F, base_size_in = base_size,
                     selected_site = selected_site, selected_year = selected_year())
    
    output$interested_overall_plot_download <- download_box("interested_overall", p,nrow(p$data))
    output$interested_overall_table_download <- download_table("interested_overall", p$data)
    output$interested_overall_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "interested_overall_plot_download", label = "Download plot"),
        downloadButton(outputId = "interested_overall_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    n_bars_interested_overall(nrow(p$data))
    p
  }, height = function() {
    50 * n_bars_interested_overall() + 50
  })
  
  n_bars_interested_keypop <<- reactiveVal(1)
  output$keypop4_plot <- renderPlot({
    base_size <- 14
    
    p <- key_pop_var_plot(ic_summary_df,"interested",
                          base_size_in = base_size,
                          selected_site = selected_site, selected_year = selected_year())
    
    output$keypop4_plot_download <- download_box("interested_keypop", p,nrow(p$data))
    output$keypop4_table_download <- download_table("interested_keypop", p$data)
    output$keypop4_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "keypop4_plot_download", label = "Download plot"),
        downloadButton(outputId = "keypop4_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    n_bars_interested_keypop(nrow(p$data))
    p
  }, height = function() {
    50 * n_bars_interested_keypop() + 100
  })
  
  output$time4_plot <- renderPlot({
    base_size <- 14
    
    p <- tbl |> 
      select(alai_up_uid, 
             contains("icab_rpv")&contains("counsel")) |>
      pivot_longer(cols = contains("icab_rpv")&contains("counsel"),
                   names_to = c("event",".value"),
                   names_pattern = "(.+)_(date|outcome)") |>
      mutate(outcome = case_when(outcome == 1 ~ "i1",
                                 outcome == 2 ~ "i2",
                                 outcome == 3 ~ "i3")) |> 
      plot_outcome_by_month("Number of people interested by month of most recent education",
                            base_size_in = base_size,
                            by_outcome = T)
    
    output$time4_plot_download <- download_box("educated_time",p)
    output$time4_table_download <- download_box("educated_time",p$data)
    output$time4_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "time4_plot_download", label = "Download plot"),
        downloadButton(outputId = "time4_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    p
  }, height = 400)
  
  output$time4_event_plot <- renderPlot({
    base_size <- 14
    
    p <- tbl |> 
      select(alai_up_uid, 
             contains("icab_rpv")&contains("counsel")) |> 
      pivot_longer(cols = contains("icab_rpv")&contains("counsel"),
                   names_to = c("event",".value"),
                   names_pattern = "(.+)_(date|outcome)") |>
      mutate(outcome = case_when(outcome == 1 ~ "i1",
                                 outcome == 2 ~ "i2",
                                 outcome == 3 ~ "i3")) |> 
      plot_outcome_by_month("Number of education encounters by month and outcome",base_size_in = base_size,
                            by_outcome = T,
                            by_person = F)
    
    output$time4_event_plot_download <- download_box("educated_time_event",p)
    output$time4_event_table_download <- download_table("educated_time_event",p$data)
    output$time4_event_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "time4_event_plot_download", label = "Download plot"),
        downloadButton(outputId = "time4_event_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    p
  }, height = 400)
  
  
  n_bars_not_interested <<- reactiveVal(1)
  output$not_interested_reason_plot <- renderPlot({
    base_size <- 14

    p <- not_interested_reason_func(tbl, base_size_in = base_size)
    
    output$not_interested_reason_plot_download <- download_box("not_interested_reason",p,nrow(p$data))
    output$not_interested_reason_table_download <- download_table("not_interested_reason",p$data)
    output$not_interested_reason_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "not_interested_reason_plot_download", label = "Download plot"),
        downloadButton(outputId = "not_interested_reason_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    n_bars_not_interested(nrow(p$data))
    p
  }, height = function() {
    50 * n_bars_not_interested() + 50
  })
  
  output$screened_n <-  renderUI({
    HTML(paste0(
      "<span style='color: #FE5000; font-size: 36px;'>", 
      screened_count, 
      "</span> people were screened out of ",
      pwh_count," people with HIV who received care at ", 
      selected_site,  
      " in ", selected_year()
    ))
  })
  
  n_bars_screened_overall <<- reactiveVal(1)
  output$screened_overall_plot <- renderPlot({
    base_size <- 14
    
    p <- ic_var_plot(ic_summary_df, "screened",by_group = F, base_size_in = base_size,
                     selected_site = selected_site, selected_year = selected_year())
    
    output$screened_overall_plot_download <- download_box("screened_overall", p,nrow(p$data))
    output$screened_overall_table_download <- download_table("screened_overall", p$data)
    output$screened_overall_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "screened_overall_plot_download", label = "Download plot"),
        downloadButton(outputId = "screened_overall_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    n_bars_screened_overall(nrow(p$data))
    p
  }, height = function() {
    50 * n_bars_screened_overall() + 50
  })
  
  n_bars_screened_keypop <<- reactiveVal(1)
  output$keypop5_plot <- renderPlot({
    base_size <- 14
    
    p <- key_pop_var_plot(ic_summary_df,"screened",
                          base_size_in = base_size,
                          selected_site = selected_site, selected_year = selected_year())
    
    output$keypop5_plot_download <- download_box("screened_keypop", p,nrow(p$data))
    output$keypop5_table_download <- download_table("screened_keypop", p$data)
    output$keypop5_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "keypop5_plot_download", label = "Download plot"),
        downloadButton(outputId = "keypop5_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    n_bars_screened_keypop(nrow(p$data))
    p
  }, height = function() {
    50 * n_bars_screened_keypop() + 100
  })
  
  output$time5_plot <- renderPlot({
    base_size <- 14
    
    p <- tbl |> 
      select(alai_up_uid, 
             contains("icab_rpv")&(contains("screen")) & contains("date")) |> 
      pivot_longer(cols = contains("icab_rpv")&(contains("screen")) & contains("date"),
                   names_to = "event",
                   values_to = "date") |>
      plot_outcome_by_month("Number of people by month of most recent screening",base_size_in = base_size)
    
    output$time5_plot_download <- download_box("screened_time",p)
    output$time5_table_download <- download_table("screened_time",p$data)
    output$time5_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "time5_plot_download", label = "Download plot"),
        downloadButton(outputId = "time5_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    p
  }, height = 400)
  
  output$time5_event_plot <- renderPlot({
    base_size <- 14
    
    p <- tbl |> 
      select(alai_up_uid, 
             contains("icab_rpv")&(contains("screen")) & contains("date")) |> 
      pivot_longer(cols = contains("icab_rpv")&(contains("screen")) & contains("date"),
                   names_to = "event",
                   values_to = "date") |>
      plot_outcome_by_month("Number of screening enounters by month",base_size_in = base_size,
                            by_person = F)
    
    output$time5_event_plot_download <- download_box("screened_time_event",p)
    output$time5_event_table_download <- download_table("screened_time_event",p$data)
    output$time5_event_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "time5_event_plot_download", label = "Download plot"),
        downloadButton(outputId = "time5_event_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    p
  }, height = 400)
  
  output$eligible_n <-  renderUI({
    HTML(paste0(
      "<span style='color: #FE5000; font-size: 36px;'>", 
      eligible_count, 
      "</span> people were eligible out of ",
      assessed_count," people assessed at ", 
      selected_site,  
      " among PWH active in ", selected_year()
    ))
  })
  
  n_bars_eligible_overall <<- reactiveVal(1)
  output$eligible_overall_plot <- renderPlot({
    base_size <- 14
    
    p <- ic_var_plot(ic_summary_df, "eligible",by_group = F, base_size_in = base_size,
                     selected_site = selected_site, selected_year = selected_year())
    
    output$eligible_overall_plot_download <- download_box("eligible_overall", p,nrow(p$data))
    output$eligible_overall_table_download <- download_table("eligible_overall", p$data)
    output$eligible_overall_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "eligible_overall_plot_download", label = "Download plot"),
        downloadButton(outputId = "eligible_overall_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    n_bars_eligible_overall(nrow(p$data))
    p
  }, height = function() {
    50 * n_bars_eligible_overall() + 50
  })
  
  n_bars_eligible_keypop <<- reactiveVal(1)
  output$keypop6_plot <- renderPlot({
    base_size <- 14
    
    p <- key_pop_var_plot(ic_summary_df,"eligible",
                          base_size_in = base_size,
                          selected_site = selected_site, selected_year = selected_year())
    
    output$keypop6_plot_download <- download_box("eligible_keypop", p,nrow(p$data))
    output$keypop6_table_download <- download_table("eligible_keypop", p$data)
    output$keypop6_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "keypop6_plot_download", label = "Download plot"),
        downloadButton(outputId = "keypop6_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    n_bars_eligible_keypop(nrow(p$data))
    p
  }, height = function() {
    50 * n_bars_eligible_keypop() + 100
  })
  
  output$time6_plot <- renderPlot({
    base_size <- 14
    
    p <- tbl |> 
      select(alai_up_uid, 
             contains("icab_rpv")&(contains("screen"))) |> 
      pivot_longer(cols = contains("icab_rpv")&contains("screen"),
                   names_to = c("event",".value"),
                   names_pattern = "(.+)_(date|outcome)") |>
      mutate(outcome = case_when(outcome == 0 ~ "e0",
                                 outcome == 1 ~ "e1"))  |>
      plot_outcome_by_month("Number of people by month of most recent screening and outcome",base_size_in = base_size,
                            by_outcome = TRUE)
    
    output$time6_plot_download <- download_box("eligible_time",p)
    output$time6_table_download <- download_table("eligible_time",p$data)
    output$time6_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "time6_plot_download", label = "Download plot"),
        downloadButton(outputId = "time6_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    p
  }, height = 400)
  
  output$time6_event_plot <- renderPlot({
    base_size <- 14
    
    p <- tbl |> 
      select(alai_up_uid, 
             contains("icab_rpv")&(contains("screen"))) |> 
      pivot_longer(cols = contains("icab_rpv")&contains("screen"),
                   names_to = c("event",".value"),
                   names_pattern = "(.+)_(date|outcome)") |>
      mutate(outcome = case_when(outcome == 0 ~ "e0",
                                 outcome == 1 ~ "e1"))  |>
      plot_outcome_by_month("Number of screening encounters by month and outcome",base_size_in = base_size,
                            by_person = F,
                            by_outcome = TRUE)
    
    output$time6_event_plot_download <- download_box("eligible_time_event",p)
    output$time6_event_table_download <- download_table("eligible_time_event",p$data)
    output$time6_event_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "time6_event_plot_download", label = "Download plot"),
        downloadButton(outputId = "time6_event_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    p
  }, height = 400)
  
  n_bars_not_eligible <<- reactiveVal(1)
  output$not_eligible_reason_plot <- renderPlot({
    base_size <- 14
    
    p <- not_eligible_reason_func(tbl, base_size_in = base_size)
    
    output$not_eligible_reason_plot_download <- download_box("not_eligible_reason",p,nrow(p$data))
    output$not_eligible_reason_table_download <- download_table("not_eligible_reason",p$data)
    output$not_eligible_reason_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "not_eligible_reason_plot_download", label = "Download plot"),
        downloadButton(outputId = "not_eligible_reason_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    n_bars_not_eligible(nrow(p$data))
    p
  }, height = function() {
    50 * n_bars_not_eligible() + 50
  })
  
  output$prescribed_n <-  renderUI({
    HTML(paste0(
      "<span style='color: #FE5000; font-size: 36px;'>", 
      prescribed_count, 
      "</span> people were prescribed out of ",
      interested_eligible_count," people eligible & interested at ", 
      selected_site,  
      " among PWH active in ", selected_year()
    ))
  })
  
  n_bars_prescribed_overall <<- reactiveVal(1)
  output$prescribed_overall_plot <- renderPlot({
    base_size <- 14
    
    p <- ic_var_plot(ic_summary_df, "prescribed",by_group = F, base_size_in = base_size,
                     selected_site = selected_site, selected_year = selected_year())
    
    output$prescribed_overall_plot_download <- download_box("prescribed_overall", p,nrow(p$data))
    output$prescribed_overall_table_download <- download_table("prescribed_overall", p$data)
    output$prescribed_overall_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "prescribed_overall_plot_download", label = "Download plot"),
        downloadButton(outputId = "prescribed_overall_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    n_bars_prescribed_overall(nrow(p$data))
    p
  }, height = function() {
    50 * n_bars_prescribed_overall() + 50
  })
  
  n_bars_prescribed_keypop <<- reactiveVal(1)
  output$keypop7_plot <- renderPlot({
    base_size <- 14
    
    p <- key_pop_var_plot(ic_summary_df,"prescribed",
                          base_size_in = base_size,
                          selected_site = selected_site, selected_year = selected_year())
    
    output$keypop7_plot_download <- download_box("prescribed_keypop", p,nrow(p$data))
    output$keypop7_table_download <- download_table("prescribed_keypop", p$data)
    output$keypop7_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "keypop7_plot_download", label = "Download plot"),
        downloadButton(outputId = "keypop7_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    n_bars_prescribed_keypop(nrow(p$data))
    p
  }, height = function() {
    50 * n_bars_prescribed_keypop() + 100
  })
  
  output$time7_plot <- renderPlot({
    base_size <- 14
    
    p <- tbl |>
      mutate(date = icab_rpv_rx_date,
             event = NA) |>
      plot_outcome_by_month("Prescription month",base_size_in = base_size)
    
    output$time7_plot_download <- download_box("prescribed_time",p)
    output$time7_table_download <- download_table("prescribed_time",p$data)
    output$time7_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "time7_plot_download", label = "Download plot"),
        downloadButton(outputId = "time7_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    p
  }, height = 400)
  
  output$initiated_n <-  renderUI({
    HTML(paste0(
      "<span style='color: #FE5000; font-size: 36px;'>", 
      initiated_count, 
      "</span> people were initiated out of ",
      prescribed_count," people prescribed at ", 
      selected_site,  
      " among PWH active in ", selected_year()
    ))
  })
  
  n_bars_initiated_overall <<- reactiveVal(1)
  output$initiated_overall_plot <- renderPlot({
    base_size <- 14
    
    p <- ic_var_plot(ic_summary_df, "initiated",by_group = F, base_size_in = base_size,
                     selected_site = selected_site, selected_year = selected_year())
    
    output$initiated_overall_plot_download <- download_box("initiated_overall", p,nrow(p$data))
    output$initiated_overall_table_download <- download_table("initiated_overall", p$data)
    output$initiated_overall_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "initiated_overall_plot_download", label = "Download plot"),
        downloadButton(outputId = "initiated_overall_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    n_bars_initiated_overall(nrow(p$data))
    p
  }, height = function() {
    50 * n_bars_initiated_overall() + 50
  })
  
  n_bars_initiated_keypop <<- reactiveVal(1)
  output$keypop8_plot <- renderPlot({
    base_size <- 14
    
    p <- key_pop_var_plot(ic_summary_df,"initiated",
                          base_size_in = base_size,
                          selected_site = selected_site, selected_year = selected_year())
    
    output$keypop8_plot_download <- download_box("initiated_keypop", p,nrow(p$data))
    output$keypop8_table_download <- download_table("initiated_keypop", p$data)
    output$keypop8_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "keypop8_plot_download", label = "Download plot"),
        downloadButton(outputId = "keypop8_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    n_bars_initiated_keypop(nrow(p$data))
    p
  }, height = function() {
    50 * n_bars_initiated_keypop() + 100
  })
  
  output$time8_plot <- renderPlot({
    base_size <- 14
    
    p <- tbl |>
      mutate(date = icab_rpv_shot1_date,
             event = NA) |>
      plot_outcome_by_month("Initiation month",base_size_in = base_size)
    
    output$time8_plot_download <- download_box("initiated_time",p)
    output$time8_table_download <- download_table("initiated_time",p$data)
    output$time8_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "time8_plot_download", label = "Download plot"),
        downloadButton(outputId = "time8_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    p
  }, height = 400)
  
  
  output$sustained_n <-  renderUI({
    HTML(paste0(
      "<span style='color: #FE5000; font-size: 36px;'>", 
      sustained_count, 
      "</span> people were sustained out of ",
      initiated_count," people initiated at ", 
      selected_site,  
      " among PWH active in ", selected_year()
    ))
  })
  
  n_bars_sustained_overall <<- reactiveVal(1)
  output$sustained_overall_plot <- renderPlot({
    base_size <- 14
    
    p <- ic_var_plot(ic_summary_df, "sustained",by_group = F, base_size_in = base_size,
                     selected_site = selected_site, selected_year = selected_year())
    
    output$sustained_overall_plot_download <- download_box("sustained_overall", p,nrow(p$data))
    output$sustained_overall_table_download <- download_table("sustained_overall", p$data)
    output$sustained_overall_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "sustained_overall_plot_download", label = "Download plot"),
        downloadButton(outputId = "sustained_overall_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    n_bars_sustained_overall(nrow(p$data))
    p
  }, height = function() {
    50 * n_bars_sustained_overall() + 50
  })
  
  n_bars_sustained_keypop <<- reactiveVal(1)
  output$keypop9_plot <- renderPlot({
    base_size <- 14
    
    p <- key_pop_var_plot(ic_summary_df,"sustained",
                          base_size_in = base_size,
                          selected_site = selected_site, selected_year = selected_year())
    
    output$keypop9_plot_download <- download_box("sustained_keypop", p,nrow(p$data))
    output$keypop9_table_download <- download_table("sustained_keypop", p$data)
    output$keypop9_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "keypop9_plot_download", label = "Download plot"),
        downloadButton(outputId = "keypop9_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    n_bars_sustained_keypop(nrow(p$data))
    p
  }, height = function() {
    50 * n_bars_sustained_keypop() + 100
  })
  
  output$time9_plot <- renderPlot({
    base_size <- 14
    
    temp <- tbl |>
      filter(!is.na(icab_rpv_shot1_date) | !is.na(icab_rpv_shot2_date)) |>
      select(alai_up_uid,icab_rpv_discontinued,
             icab_rpv_discontinued_date, 
             contains("icab_rpv_shot")&contains("date")) |> 
      pivot_longer(cols = contains("icab_rpv_shot")&contains("date"), 
                   values_drop_na = T) |>
      summarize(.by = alai_up_uid, 
                first_shot_date = min(value),
                last_shot_date = max(value), 
                icab_rpv_discontinued = max(icab_rpv_discontinued),
                icab_rpv_discontinued_date = max(icab_rpv_discontinued_date)) |>
      mutate(.by = alai_up_uid,
             last_cab_date = max(last_shot_date,icab_rpv_discontinued_date,na.rm = T),
             last_cab_time = as.numeric(difftime(
               last_cab_date, first_shot_date, units = "days"
             )),
             icab_rpv_discontinued = case_when(icab_rpv_discontinued == 1~1,
                                               .default = 0),
             period =15 + (floor(last_cab_time/30) * 30)) |>
      summarize(.by = c(period,icab_rpv_discontinued),
                n = n()) 
    
    
    
    p <- temp |>
      ggplot(aes(x = period,y = n, fill = factor(icab_rpv_discontinued))) + 
      geom_col() +
      scale_x_continuous(breaks = seq(0,max(temp$period),by = 180)) +
      scale_fill_manual(name = NULL,
                        values = c("0" = "#08519C",
                                   "1" = "#FE5000"),
                        labels = c("0" = "Sustained on CAB",
                                   "1" = "Discontinued")) + 
      labs(x = "Time spent on CAB (days)",
           y = NULL,
           title = "Time spent on CAB as of reporting period") +
      theme_minimal(base_size = 14,
                    base_family = "Roboto") +
      theme(axis.text = element_text(size = rel(1.1),color = "black"),
            axis.title = element_text(size = rel(1)),
            legend.text = element_text(size = rel(1.1)))
    
    output$time9_plot_download <- download_box("discontinued_time",p)
    output$time9_table_download <- download_table("discontinued_time",p$data)
    output$time9_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "time9_plot_download", label = "Download plot"),
        downloadButton(outputId = "time9_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    p
  }, height = 400)
  
  n_bars_discontinued <<- reactiveVal(1)
  output$discontinued_reason_plot <- renderPlot({
    base_size <- 14
    
    p <- discontinued_reason_func(tbl, base_size_in = base_size)
    
    output$discontinued_reason_plot_download <- download_box("discontinued_reason",p,nrow(p$data))
    output$discontinued_reason_table_download <- download_table("discontinued_reason",p$data)
    output$discontinued_reason_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "discontinued_reason_plot_download", label = "Download plot"),
        downloadButton(outputId = "discontinued_reason_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    
    n_bars_discontinued(nrow(p$data))
    
    p
  }, height = function() {
    50 * n_bars_discontinued() + 50
  })
  
  # on time outcomes
 
  interval_1 <- reactive({
    if (input$ontime_target_input == "4 or 8 weeks") {
      28
    } else if (input$ontime_target_input == "1 or 2 months") {
      31
    }
  })
  
  interval_2 <- reactive({
    if (input$ontime_target_input == "4 or 8 weeks") {
      56
    } else if (input$ontime_target_input == "1 or 2 months") {
      62
    }
  })
  
  cab_doses_df <- cab_master_df |>
    filter(shot_appt == 1)

  temp <- cab_doses_df |>
    filter(!is.na(on_time)) |>
    summarise(.by = on_time,
              n = n()) |>
    mutate(pct = n/sum(n),
           my_string = paste0(n, "/", sum(n), " (",round(100*pct,1),"%)"))
  
  num_doses <- nrow(cab_doses_df)
  
  num_people_w_doses <- length(unique(cab_doses_df$alai_up_uid))
  
  on_time_string <- temp |>
    mutate(on_time = if_else(on_time == "Late","Late","On time/early")) |>
    summarize(.by = on_time,
              n = sum(n)) |>
    mutate(pct = n / sum(n),
           my_string = paste0(n, "/", sum(n), " (",round(100*pct,1),"%)")) |>
    filter(on_time == "On time/early") |>
    pull(my_string)
  
  output$clinical_n <-  renderUI({
    HTML(paste0(
      "<span style='color: #08519C; font-size: 36px;'>", 
      num_doses, 
      "</span> injections were administered to ",
      "<span style='color: #08519C; font-size: 36px;'>", 
      num_people_w_doses, 
      "</span> individuals. ",
      "<span style='color: #08519C; font-size: 36px;'>", 
      on_time_string,
      "</span> follow up injections were administered on time or early"
    ))
  })
  
  output$ontime_status_bar <- renderPlot({
    
    p <- temp |>
      mutate(y = "null",
             on_time = factor(on_time, levels = c("Late","On time","Early")),
             label_col = str_c(on_time, "\n",n, "/", sum(n), " (",round(100*pct,1),"%)")) |>
      arrange(desc(on_time)) |>
      mutate(cum_pct = cumsum(pct),
             midpoint = if_else(is.na(lag(cum_pct)),cum_pct/2,
                                (cum_pct + lag(cum_pct))/2)) |>  
      ggplot(aes(x = pct, y = y, fill=on_time)) + 
      geom_bar(position = "stack",stat = "identity",
               width = 0.3) + 
      geom_label(aes(x = midpoint,label = label_col),
                 vjust = -1,
                 alpha = 0.4,
                 size = 5) + 
      scale_fill_manual(values = c("Late" = "#fE5000","On time" = "#08519C","Early" = "#9ECAE1")) + 
      theme_minimal() +
      theme(axis.title = element_blank(),
            legend.position = "none",
            axis.text = element_blank(),
            panel.grid = element_blank())
    
    output$ontime_status_bar_download <- download_box("ontime_status",p)
    output$ontime_status_table_download <- download_table("ontime_status",p$data)
    output$ontime_status_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "ontime_status_bar_download", label = "Download plot"),
        downloadButton(outputId = "ontime_status_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    p
    
  }, height = 200)
  
  output$ontime_plot_monthly <- renderPlot({

    
    p <- ontime_plot_func(cab_master_df |>
                             filter(shot_appt == 1),
                          interval_1(),interval_2(),
                          "Monthly injection interval")
    
    output$ontime_plot_monthly_download <- download_box("ontime_dist_monthly",p)
    output$ontime_table_monthly_download <- download_table("ontime_dist_monthly",p$data)
    output$ontime_monthly_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "ontime_plot_monthly_download", label = "Download plot"),
        downloadButton(outputId = "ontime_table_monthly_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    p
    
  }, height = 400)
  
  output$ontime_plot_bimonthly <- renderPlot({
    
    p <- ontime_plot_func(cab_master_df |>
                             filter(shot_appt == 1),interval_1(),interval_2(),
                          "Bimonthly injection interval")
    
    output$ontime_plot_bimonthly_download <- download_box("ontime_dist_bimonthly",p)
    output$ontime_table_bimonthly_download <- download_table("ontime_dist_bimonthly",p$data)
    output$ontime_bimonthly_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "ontime_plot_bimonthly_download", label = "Download plot"),
        downloadButton(outputId = "ontime_table_bimonthly_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    p
    
  }, height = 400)
  
  output$late_pt_plot <- renderPlot({
    base_size <- 14
    text_size <- base_size / 3.5
    
    plot_data <-  cab_master_df |>
      filter(shot_appt == 1) |>
      filter(!is.na(on_time)) |>
      summarize(.by = alai_up_uid, 
                late = sum(late)) |>
      summarize(.by = late,
                num_clients = n()) |>
      mutate(fill_col = if_else(late == 0, "ontime", "late"),
             label_col = paste0(num_clients," patients had ", late," late injections")) 
    
    max_y = max(plot_data$num_clients)
    
    p <- plot_data |>
      ggplot(aes(x = factor(late), y = num_clients)) +
      geom_text(aes(label = str_wrap(label_col,12)),
                vjust = -0.2, size = text_size,
                family = "Roboto") +
      geom_col(aes(fill = fill_col)) +
      labs(x = "Number of late injections",
           y = "Number of patients") + 
      scale_fill_manual(values = c("ontime" = "#08519C",
                                   "early" = "#9ECAE1",
                                   "late" = "#fE5000"),
                        labels = c("ontime" = "No late injections",
                                   "late" = "Late")) + 
      ylim(0,max_y + 30) + 
      theme_minimal(base_size = base_size,
                    base_family = "Roboto") +
      theme(legend.position = "none",
            axis.text = element_text(size = rel(1.1)),
            axis.title = element_text(size = rel(1)),)
    
    output$late_pt_plot_download <- download_box("late_by_pt",p)
    output$late_pt_table_download <- download_table("late_by_pt",p$data)
    output$late_pt_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "late_pt_plot_download", label = "Download plot"),
        downloadButton(outputId = "late_pt_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    p
  }, height = 400)
  
  output$early_pt_plot <- renderPlot({
    base_size <- 14
    text_size <- base_size / 3.5
    
    plot_data <-  cab_master_df |>
      filter(shot_appt == 1) |>
      filter(!is.na(on_time)) |>
      summarize(.by = alai_up_uid,
                early = sum(early)) |>
      summarize(.by = early,
                num_clients = n()) |>
      mutate(fill_col = if_else(early == 0, "ontime", "early"),
             label_col = paste0(num_clients," patients had ", early," early injections")) 
    
    max_y = max(plot_data$num_clients)
      
    p <- plot_data |>
      ggplot(aes(x = fct_rev(factor(early)), y = num_clients)) +
      geom_col(aes(fill = fill_col)) +
      geom_text(aes(label = str_wrap(label_col,12)),
                vjust = -0.2, size = text_size,
                family = "Roboto") + 
      labs(x = "Number of early injections",
           y = "Number of patients") + 
      scale_fill_manual(values = c("ontime" = "#08519C",
                                   "early" = "#9ECAE1",
                                   "late" = "#fE5000"),
                        labels = c("ontime" = "No early injections",
                                   "early" = "Early")) + 
      ylim(0,max_y + 30) + 
      theme_minimal(base_size = base_size,
                    base_family = "Roboto") +
      theme(legend.position = "none",
            axis.text = element_text(size = rel(1.1)),
            axis.title = element_text(size = rel(1)),)
    
    output$early_pt_plot_download <- download_box("early_by_pt",p)
    output$early_pt_table_download <- download_table("early_by_pt",p$data) 
    output$early_pt_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "early_pt_plot_download", label = "Download plot"),
        downloadButton(outputId = "early_pt_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    p
  }, height = 400)
  
  # VL page
  # BANs
  cab_vl <- cab_master_df |>
    filter(vl_appt == 1)
  
  num_people_w_doses <- length(unique(cab_master_df$alai_up_uid))
  
  n_pre_cab_vl <- cab_vl |> 
    filter(!is.na(pre_icab_vl_result)) |>
    ungroup() |>
    summarize(n = n_distinct(alai_up_uid)) |>
    pull(n)
  
  pre_cab_vl_string <- str_c(n_pre_cab_vl,"/", num_people_w_doses," (",
                             round(100 * n_pre_cab_vl/num_people_w_doses,1),
                             "%)")
  
  val1 <- reactive({
    if (input$vl_cutoff_input == "50 copies/mL"){
      "<50"
    } else if (input$vl_cutoff_input == "200 copies/mL"){
      "<200"
    } 
  })
  
  val2 <- reactive({
    if (input$vl_cutoff_input == "50 copies/mL"){
      "\u226550"
    } else if (input$vl_cutoff_input == "200 copies/mL"){
      "\u2265200"
    } 
  })
  
  observe({
    
    n_vs <- cab_vl |> 
      filter(!is.na(pre_icab_vl_result),
             pre_icab_vl_result <= if_else(val1() == "<50",2,3)) |>
      ungroup()  |>
      summarize(n = n_distinct(alai_up_uid)) |>
      pull(n)
    
    n_vs_string <- str_c(n_vs,"/", n_pre_cab_vl," (",
                         round(100 * n_vs/n_pre_cab_vl,1),
                         "%)")
    
    n_vf <- cab_vl |> 
      filter(!is.na(pre_icab_vl_result),
             pre_icab_vl_result > if_else(val1() == "<50",2,3)) |>
      ungroup()  |>
      summarize(n = n_distinct(alai_up_uid)) |>
      pull(n)
    
    n_vf_string <- str_c(n_vf,"/", n_pre_cab_vl," (",
                         round(100 * n_vf/n_pre_cab_vl,1),
                         "%)")
    
    output$vl_n <-  renderUI({
      HTML(paste0(
        "<span style='color: #08519C; font-size: 36px;'>", 
        num_people_w_doses, 
        "</span> individuals had ever received a CAB dose. ",
        "<span style='color: #08519C; font-size: 36px;'>", 
        pre_cab_vl_string,
        "</span> of them had a valid pre-iCAB VL result, and VL results while on CAB.<br>",
        "<span style='color: #08519C; font-size: 36px;'>", 
        n_vs_string,
        "</span> had a pre-iCAB VL of ", val1()," copies/mL.<br>",
        "<span style='color: #FE5000; font-size: 36px;'>", 
        n_vf_string,
        "</span> had a pre-iCAB VL of ", val2()," copies/mL. "
      ))
    })
  })
  
  
  
  # VL figures
  output$vl_time_to_vs <- renderPlot({
    base_size <- 14
    text_size <- base_size / 3
    
    if (val1() == "<50"){
      temp <- cab_master_df |>
        filter(vl_appt == 1) |>
        filter(pre_icab_vl_result > 2) |> 
        ungroup() |>
        summarize(.by = alai_up_uid,
                  event = max(first_under50),
                  time = min(first_under50_time, last_cab_time,na.rm = T)) 
    } else {
      temp <- cab_master_df |>
        filter(vl_appt == 1) |>
        filter(pre_icab_vl_result > 3) |> 
        ungroup() |>
        summarize(.by = alai_up_uid,
                  event = max(first_under200),
                  time = min(first_under200_time, last_cab_time,na.rm = T)) 
    }
    
    if (nrow(temp) == 0) {
      output$time_to_failure2_download_ui <- renderUI(NULL)
      validate(need(nrow(temp)>0,
                    "No data available for this starting VL"))
    }
    
    fit <- survfit2(Surv(time, event) ~1, data = temp)
    
    p <- summary(fit,
                 times = c(0,60,120,180,360),
                 data.frame = T,
                 extend = T) |>
      mutate(total_event = 1-surv,
             cum_event = cumsum(n.event),
             n_data = cum_event + n.risk) |>
      pivot_longer(cols = c(surv,total_event)) |> 
      mutate(n.risk = if_else(name =="surv",n.risk,NA),
             label_col = case_when(
               value < 0.01 ~ NA,
               .default = paste0(round(100*value),"%"))) |>
      mutate(.by = name,
             axis_order = row_number(),
             axis_labels = str_c(time, " (",n_data,")"),
             axis_labels = fct_reorder(axis_labels,axis_order)) |>
      ggplot(aes(x = axis_labels,y =value,
                 fill = name)) + 
      geom_col() +
      geom_text(
        position = position_fill(vjust = 0.5),
        aes(label = label_col),
        vjust = 0.5, color = "black", size = text_size
      ) +
      labs(x = "Days since CAB initiation (number contributing data)",
           y = NULL,
           title = str_c("Estimated time of first VL ",val1(),", among those starting CAB with VL ",val2()),
           caption = str_wrap("Percentages at each time point are estimated using the Kaplan-Meier survival function, accounting for uncertainty due to (1) people who discontinue and (2) people who have not accrued enough time on CAB yet.",120)) + 
      scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                         limits = c(0,1)) +
      scale_fill_manual(name = NULL,
                        values = c("surv" = "#FEA77F",
                                   "total_event" = "#FE5000"),
                        labels = c("surv" = str_c("Not suppressed (VL ", val2(),")"),
                                   "total_event" = str_c("Suppressed (VL ",val1(),")"))) +
      theme_minimal(base_size = base_size,
                    base_family = "Roboto") +
      theme(plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0),
            plot.title.position = "plot",
            axis.text.x = element_text(color = "black", size = rel(1.2),
                                       margin = margin(t = -5)),
            axis.text.y = element_blank(),
            axis.title.x = element_text(size = rel(1.2),
                                        margin = margin(t = 10, b = 10)),
            title = element_text(size = rel(1.2)),
            legend.text = element_text(size = rel(1.2)))
    
    output$vl_time_to_vs_plot_download <- download_box("time_to_VS",p)
    output$vl_time_to_vs_table_download <- download_table("time_to_VS",p$data)
    output$vl_time_to_vs_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "vl_time_to_vs_plot_download", label = "Download plot"),
        downloadButton(outputId = "vl_time_to_vs_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    p
  }, height = 400)
  
  output$time_to_el_vl1 <- renderPlot({
    base_size <- 14
    text_size <- base_size / 3
    
    if (val1() == "<50"){
      temp <-  cab_master_df |>
        filter(vl_appt == 1) |>
        filter(.by = alai_up_uid,
               !is.na(starting_vl),
               pre_icab_vl_result <= 2
        ) |>
        ungroup() |>
        mutate(first_elevated_vl_ref50 = if_else(is.na(first_elevated_vl_ref50),0,first_elevated_vl_ref50)) |>
        summarize(.by = c(alai_up_uid),
                  event = max(first_elevated_vl_ref50),
                  time = min(first_elevated_vl_ref50_time, max(time_from_50,na.rm = T),na.rm = T)) 
    } else {
      temp <-  cab_master_df |>
        filter(vl_appt == 1) |>
        filter(.by = alai_up_uid,
               !is.na(starting_vl),
               pre_icab_vl_result <= 3
        ) |>
        ungroup() |>
        mutate(first_elevated_vl_ref200 = if_else(is.na(first_elevated_vl_ref200),0,first_elevated_vl_ref200)) |>
        summarize(.by = c(alai_up_uid),
                  event = max(first_elevated_vl_ref200),
                  time = min(first_elevated_vl_ref200_time, max(time_from_200,na.rm = T),na.rm = T)) 
    }
    
    if (nrow(temp) == 0) {
      output$time_to_failure2_download_ui <- renderUI(NULL)
      validate(need(nrow(temp)>0,
                    "No data available for this starting VL"))
    }
    
    fit <- survfit2(Surv(time, event) ~ 1, data = temp)
    
    p <- summary(fit,
                 times = c(0,60,120,180,360),
                 data.frame = T,
                 extend = T) |> 
      mutate(total_event = 1-surv) |>
      mutate(cum_event = cumsum(n.event),
             n_data = cum_event + n.risk) |>
      pivot_longer(cols = c(surv,total_event)) |> 
      mutate(n.risk = if_else(name =="surv",n.risk,NA),
             label_col = case_when(
               value < 0.01 ~ NA,
               .default = paste0(round(100*value),"%"))) |> 
      mutate(.by = name,
             axis_order = row_number(),
             axis_labels = str_c(time, " (",n_data,")"),
             axis_labels = fct_reorder(axis_labels,axis_order)) |> 
      ggplot(aes(x = axis_labels,y =value,
                 fill = name)) + 
      geom_col() +
      geom_text(
        position = position_fill(vjust = 0.5),
        aes(label = label_col),
        vjust = 0.5, color = "black", size = text_size
      ) +
      labs(x = str_c("Days since VL ",val1()," (number contributing data)"),
           y = NULL,
           title = str_c("Estimated time of first VL ",val2(),", among those with pre-CAB VL ,",val1()),
           caption = str_wrap("Percentages at each time point are estimated using the Kaplan-Meier survival function, accounting for uncertainty due to (1) people who discontinue and (2) people who have not accrued enough time on CAB yet.",120)) + 
      scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                         limits = c(0,1)) +
      scale_fill_manual(name = NULL,
                        values = c("surv" = "#9ECAE1",
                                   "total_event" = "#08519C"
                        ),
                        labels = c("surv" = str_c("Suppressed (VL ",val1(),")"),
                                   "total_event" = str_c("Not suppressed (VL ",val2(),")"))) +
      theme_minimal(base_size = base_size,
                    base_family = "Roboto") +
      theme(plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0),
            plot.title.position = "plot",
            axis.text.x = element_text(color = "black", size = rel(1.2),
                                       margin = margin(t = -5)),
            axis.text.y = element_blank(),
            axis.title.x = element_text(size = rel(1.2),
                                        margin = margin(t = 10, b = 10)),
            title = element_text(size = rel(1.2)),
            legend.text = element_text(size = rel(1.2)))
    
    output$time_to_el_vl1_plot_download <- download_box("time_to_elevated_vl1",p)
    output$time_to_el_vl1_table_download <- download_table("time_to_elevated_vl1",p$data)
    output$time_to_el_vl1_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "time_to_el_vl1_plot_download", label = "Download plot"),
        downloadButton(outputId = "time_to_el_vl1_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    p
    
  }, height = 400)
  
  output$time_to_el_vl2 <- renderPlot({
    base_size <- 14
    text_size <- base_size / 3
    
    if (val1() == "<50"){
      temp <-  cab_master_df |>
        filter(vl_appt == 1) |>
        filter(.by = alai_up_uid,
               !is.na(starting_vl),
               pre_icab_vl_result > 2 & any(first_under50 == 1)
        ) |>
        ungroup() |>
        mutate(first_elevated_vl_ref50 = if_else(is.na(first_elevated_vl_ref50),0,first_elevated_vl_ref50)) |>
        summarize(.by = c(alai_up_uid),
                  event = max(first_elevated_vl_ref50),
                  time = min(first_elevated_vl_ref50_time, max(time_from_50,na.rm = T),na.rm = T))
    } else {
      temp <-  cab_master_df |>
        filter(vl_appt == 1) |>
        filter(.by = alai_up_uid,
               !is.na(starting_vl),
               pre_icab_vl_result > 3 & any(first_under200 == 1)
        ) |>
        ungroup() |>
        mutate(first_elevated_vl_ref200 = if_else(is.na(first_elevated_vl_ref200),0,first_elevated_vl_ref200)) |>
        summarize(.by = c(alai_up_uid),
                  event = max(first_elevated_vl_ref200),
                  time = min(first_elevated_vl_ref200_time, max(time_from_200,na.rm = T),na.rm = T))
    }
    
    if (nrow(temp) == 0) {
      output$time_to_failure2_download_ui <- renderUI(NULL)
      validate(need(nrow(temp)>0,
                    "No data available for this starting VL"))
    }
    
    fit <- survfit2(Surv(time, event) ~ 1, data = temp)
    
    p <- summary(fit,
                 times = c(0,60,120,180,360),
                 data.frame = T,
                 extend = T) |> 
      mutate(total_event = 1-surv) |>
      mutate(cum_event = cumsum(n.event),
             n_data = cum_event + n.risk) |>
      pivot_longer(cols = c(surv,total_event)) |> 
      mutate(n.risk = if_else(name =="surv",n.risk,NA),
             label_col = case_when(
               value < 0.01 ~ NA,
               .default = paste0(round(100*value),"%"))) |> 
      mutate(.by = name,
             axis_order = row_number(),
             axis_labels = str_c(time, " (",n_data,")"),
             axis_labels = fct_reorder(axis_labels,axis_order)) |> 
      ggplot(aes(x = axis_labels,y =value,
                 fill = name)) + 
      geom_col() +
      geom_text(
        position = position_fill(vjust = 0.5),
        aes(label = label_col),
        vjust = 0.5, color = "black", size = text_size
      ) +
      labs(x = str_c("Days since VL ",val1()," (number contributing data)"),
           y = NULL,
           title = str_c("Estimated time of first VL ",val2(),", among those with pre-CAB VL ,",val2()),
           caption = str_wrap("Percentages at each time point are estimated using the Kaplan-Meier survival function, accounting for uncertainty due to (1) people who discontinue and (2) people who have not accrued enough time on CAB yet.",120)) + 
      scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                         limits = c(0,1)) +
      scale_fill_manual(name = NULL,
                        values = c("surv" = "#FEA77F",
                                   "total_event" = "#FE5000"
                        ),
                        labels = c("surv" = str_c("Suppressed (VL ",val1(),")"),
                                   "total_event" = str_c("Not suppressed (VL ",val2(),")"))) +
      theme_minimal(base_size = base_size,
                    base_family = "Roboto") +
      theme(plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0),
            plot.title.position = "plot",
            axis.text.x = element_text(color = "black", size = rel(1.2),
                                       margin = margin(t = -5)),
            axis.text.y = element_blank(),
            axis.title.x = element_text(size = rel(1.2),
                                        margin = margin(t = 10, b = 10)),
            title = element_text(size = rel(1.2)),
            legend.text = element_text(size = rel(1.2)))
    
    output$time_to_el_vl2_plot_download <- download_box("time_to_elevated_vl2",p)
    output$time_to_el_vl2_table_download <- download_table("time_to_elevated_vl2",p$data)
    output$time_to_el_vl2_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "time_to_el_vl2_plot_download", label = "Download plot"),
        downloadButton(outputId = "time_to_el_vl2_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    p
    
  }, height = 400)
  
  output$time_to_failure1 <- renderPlot({
    base_size <- 14
    text_size <- base_size / 3
    
    if (val1() == "<50"){
      temp <-  cab_master_df |>
        filter(vl_appt == 1) |>
        filter(!is.na(starting_vl),
               pre_icab_vl_result <= 2) |>
        ungroup() |>
        mutate(first_failure_ref50 = if_else(is.na(first_failure_ref50),0,first_failure_ref50)) |>
        summarize(.by = c(alai_up_uid, starting_vl),
                  event = max(first_failure_ref50),
                  time = min(first_failure_ref50_time, max(time_from_50,na.rm = T),na.rm = T)) 
    } else {
      temp <-  cab_master_df |>
        filter(vl_appt == 1) |>
        filter(!is.na(starting_vl),
               pre_icab_vl_result <= 3) |>
        ungroup() |>
        mutate(first_failure_ref200 = if_else(is.na(first_failure_ref200),0,first_failure_ref200)) |>
        summarize(.by = c(alai_up_uid, starting_vl),
                  event = max(first_failure_ref200),
                  time = min(first_failure_ref200_time, max(time_from_200,na.rm = T),na.rm = T)) 
    }
    
    if (nrow(temp) == 0) {
      output$time_to_failure2_download_ui <- renderUI(NULL)
      validate(need(nrow(temp)>0,
                    "No data available for this starting VL"))
    }
    
    fit <- survfit2(Surv(time, event) ~ 1, data = temp)
    
    p <- summary(fit,
                 times = c(0,60,120,180,360),
                 data.frame = T,
                 extend = T) |> 
      mutate(total_event = 1-surv,
             cum_event = cumsum(n.event),
             n_data = cum_event + n.risk) |>
      pivot_longer(cols = c(surv,total_event)) |> 
      mutate(n.risk = if_else(name =="surv",n.risk,NA),
             label_col = case_when(
               value < 0.01 ~ NA,
               .default = paste0(round(100*value),"%"))) |>
      mutate(.by = name,
             axis_order = row_number(),
             axis_labels = str_c(time, " (",n_data,")"),
             axis_labels = fct_reorder(axis_labels,axis_order)) |> 
      ggplot(aes(x = axis_labels,y =value,
                 fill = name)) + 
      geom_col() +
      geom_text(
        position = position_fill(vjust = 0.5),
        aes(label = label_col),
        vjust = 0.5, color = "black", size = text_size
      ) +
      labs(x = str_c("Days since VL ",val1()," (number contributing data)"),
           y = NULL,
           title = str_c("Estimated time of first Viral failure, among those with pre-CAB VL ",val1()),
           caption = str_wrap("Percentages at each time point are estimated using the Kaplan-Meier survival function, accounting for uncertainty due to (1) people who discontinue and (2) people who have not accrued enough time on CAB yet.",120)) + 
      scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                         limits = c(0,1)) +
      scale_fill_manual(name = NULL,
                        values = c("surv" = "#9ECAE1",
                                   "total_event" = "#08519C"
                        ),
                        labels = c("surv" = "Suppressed",
                                   "total_event" = "Virologic Failure")) +
      theme_minimal(base_size = base_size,
                    base_family = "Roboto") +
      theme(plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0),
            plot.title.position = "plot",
            axis.text.x = element_text(color = "black", size = rel(1.2),
                                       margin = margin(t = -5)),
            axis.text.y = element_blank(),
            axis.title.x = element_text(size = rel(1.2),
                                        margin = margin(t = 10, b = 10)),
            title = element_text(size = rel(1.2)),
            legend.text = element_text(size = rel(1.2)))
    
    output$time_to_failure1_plot_download <- download_box("time_to_failure1",p)
    output$time_to_failure1_table_download <- download_table("time_to_failure1",p$data)
    output$time_to_failure1_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "time_to_failure1_plot_download", label = "Download plot"),
        downloadButton(outputId = "time_to_failure1_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    p
    
  }, height = 400)
  
  output$time_to_failure2 <- renderPlot({
    base_size <- 14
    text_size <- base_size / 3
    
    if (val1() == "<50"){
      temp <-  cab_master_df |>
        filter(vl_appt == 1) |>
        filter(!is.na(starting_vl),
               pre_icab_vl_result > 2 & any(first_under50 == 1)) |>
        ungroup() |>
        mutate(first_failure_ref50 = if_else(is.na(first_failure_ref50),0,first_failure_ref50)) |>
        summarize(.by = c(alai_up_uid, starting_vl),
                  event = max(first_failure_ref50),
                  time = min(first_failure_ref50_time, max(time_from_50,na.rm = T),na.rm = T)) 
    } else {
      temp <-  cab_master_df |>
        filter(vl_appt == 1) |>
        filter(!is.na(starting_vl),
               pre_icab_vl_result > 3 & any(first_under200 == 1)) |>
        ungroup() |>
        mutate(first_failure_ref200 = if_else(is.na(first_failure_ref200),0,first_failure_ref200)) |>
        summarize(.by = c(alai_up_uid, starting_vl),
                  event = max(first_failure_ref200),
                  time = min(first_failure_ref200_time, max(time_from_200,na.rm = T),na.rm = T)) 
    }
    
    if (nrow(temp) == 0) {
      output$time_to_failure2_download_ui <- renderUI(NULL)
      validate(need(nrow(temp)>0,
                    "No data available for this starting VL"))
    }
    
    fit <- survfit2(Surv(time, event) ~ 1, data = temp)
    
    p <- summary(fit,
                 times = c(0,60,120,180,360),
                 data.frame = T,
                 extend = T) |> 
      mutate(total_event = 1-surv,
             cum_event = cumsum(n.event),
             n_data = cum_event + n.risk) |>
      pivot_longer(cols = c(surv,total_event)) |> 
      mutate(n.risk = if_else(name =="surv",n.risk,NA),
             label_col = case_when(
               value < 0.01 ~ NA,
               .default = paste0(round(100*value),"%"))) |>
      mutate(.by = name,
             axis_order = row_number(),
             axis_labels = str_c(time, " (",n_data,")"),
             axis_labels = fct_reorder(axis_labels,axis_order)) |> 
      ggplot(aes(x = axis_labels,y =value,
                 fill = name)) + 
      geom_col() +
      geom_text(
        position = position_fill(vjust = 0.5),
        aes(label = label_col),
        vjust = 0.5, color = "black", size = text_size
      ) +
      labs(x = str_c("Days since VL ",val1()," (number contributing data)"),
           y = NULL,
           title = str_c("Estimated time of first Viral failure, among those with pre-CAB VL ",val2()),
           caption = str_wrap("Percentages at each time point are estimated using the Kaplan-Meier survival function, accounting for uncertainty due to (1) people who discontinue and (2) people who have not accrued enough time on CAB yet.",120)) + 
      scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                         limits = c(0,1)) +
      scale_fill_manual(name = NULL,
                        values = c("surv" = "#FEA77F",
                                   "total_event" = "#FE5000"
                        ),
                        labels = c("surv" = "Suppressed",
                                   "total_event" = "Virologic Failure")) +
      theme_minimal(base_size = base_size,
                    base_family = "Roboto") +
      theme(plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0),
            plot.title.position = "plot",
            axis.text.x = element_text(color = "black", size = rel(1.2),
                                       margin = margin(t = -5)),
            axis.text.y = element_blank(),
            axis.title.x = element_text(size = rel(1.2),
                                        margin = margin(t = 10, b = 10)),
            title = element_text(size = rel(1.2)),
            legend.text = element_text(size = rel(1.2)))
    
   
    output$time_to_failure2_plot_download <- download_box("time_to_failure2",p)
    output$time_to_failure2_table_download <- download_table("time_to_failure2",p$data)
    output$time_to_failure2_download_ui <- renderUI({
      tagList(
        downloadButton(outputId = "time_to_failure2_plot_download", label = "Download plot"),
        downloadButton(outputId = "time_to_failure2_table_download",  
                       label = "Download table",
                       icon = icon("table"))
      )
    })
    p
    
  }, height = 400)
  
}


