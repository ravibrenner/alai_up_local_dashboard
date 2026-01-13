
options(dplyr.summarise.inform = FALSE)
theme_set(theme_minimal())
font_add_google(name = "Roboto", family = "Roboto")
showtext_auto()

source("server scripts/main_page_server.R")
source("server scripts/data_explore_server.R")
source("server scripts/server_util.R")
source("server scripts/render_scroll_page.R")
source("server scripts/help_text.R")


user_base <- read_csv("data/passwords.csv",
                      show_col_types = F) |>
  mutate(password_hash = map_chr(password_hash,\(x) sodium::password_store(x)))

# loading in the master dataset and processing it 
raw_data <- read_csv("data/combined_data_for_dashboard_2026-01-07.csv",
                     col_types = cols(.default = col_character()),
                     show_col_types = F,
                     na = c("NA","UNK", ".", "C","888888","999999")) |>
  load_and_process_data()

server <- function(input, output, session) {
  
  # log in/logout section
  logout_init <- reactiveVal(FALSE)
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password_hash,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  user_info <- reactive({
    credentials()$info
  })
  
  output$home_page <-  renderUI({
    req(credentials()$user_auth)
    source(file='ui scripts/home.R', local= T)$value
  })
  
  # site choice options
  output$site_choice <- renderUI({
    req(credentials()$user_auth)
    selectInput("dataset", "Choose one of the sites below:",
                choices = c("All ALAI UP Sites",
                            "AP (Dallas, TX)",
                            "BCHD (Baltimore, MD)",
                            "CFHC (Biloxi, MS)",
                            "JMFC (New York, NY)",
                            "PIHC (Decatur, GA)",
                            "SAAF (San Antonio, TX)",
                            "SCC (Orlando, FL)",
                            "SIDC (Chicago, IL)"
                ),
                selected="All ALAI UP Sites")
  })
  
  # site choice options updater
  observe({
    req(credentials()$user_auth)
    if (user_info()$permissions == "admin") {
      updateSelectInput(session, "dataset",
                        choices = c("All ALAI UP Sites",
                                    "AP (Dallas, TX)",
                                    "BCHD (Baltimore, MD)",
                                    "CFHC (Biloxi, MS)",
                                    "JMFC (New York, NY)",
                                    "PIHC (Decatur, GA)",
                                    "SAAF (San Antonio, TX)",
                                    "SCC (Orlando, FL)",
                                    "SIDC (Chicago, IL)"
                        ),
                        selected="All ALAI UP Sites")
    } else if (user_info()$permissions == "shared"){
      updateSelectInput(session, "dataset",
                        choices = c("All ALAI UP Sites",
                                    # "AP (Dallas, TX)",
                                    "BCHD (Baltimore, MD)",
                                    "CFHC (Biloxi, MS)",
                                    # "JMFC (New York, NY)",
                                    "PIHC (Decatur, GA)",
                                    "SAAF (San Antonio, TX)",
                                    # "SCC (Orlando, FL)",
                                    "SIDC (Chicago, IL)"
                        ),
                        selected=get_user_for_login(user_info()$site))
    } else {
      site_choices <- c(get_user_for_login(user_info()$site), "All ALAI UP Sites")
      updateSelectInput(session, "dataset",
                        choices = site_choices,
                        selected = site_choices[[1]])
    } 
  })

  
  
  
  df <- reactiveVal()
  tbl <- reactiveVal()
  ic_summary_df <- reactiveVal()
  filtered_ic_summary_df <- reactiveVal()
  selected_site <- reactiveVal()
  
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
  
  
  observeEvent(user_info()$permissions, {
    user <- user_info()
    observeEvent(input$dataset, {
      df(filter_data_by_site(raw_data, get_dataset_for_user(input$dataset)))
      selected_site(input$dataset)
    })
  })
  
  cab_master_df <- reactive({
    req(tbl(), interval_1(), interval_2())
    prepare_cab_master_df(tbl(), interval_1(), interval_2())
  })
  
  filtered_tbl <- reactive({
    req(df())
    if (length(input$date_filter) == 2 &!is.null(input$active_year)) {
      get_current_year_data(df(),
                            as.numeric(input$active_year),
                            filter_dates = TRUE,
                            start_date = as.Date(input$date_filter[1]),
                            end_date = as.Date(input$date_filter[2]))
    }
    else {
      tbl()  # fallback if no date range selected
    }
  })
  
  observeEvent(df(), {
    tbl(get_current_year_data(df(), as.numeric(2025)))
    ic_summary_df(prepare_ic_summary(tbl()))

    update_app()
  })
  
  observeEvent(cab_master_df(), {
    update_app()
  })
  
  observeEvent(filtered_tbl(), {
    filtered_ic_summary_df(prepare_ic_summary(filtered_tbl()))

    data_explore_server(input, output, filtered_ic_summary_df(), session)
  })
  
  update_app <- function() {
    main_page_server(input, output, tbl(), ic_summary_df(), selected_site(), cab_master_df(), session)
    dynamic_filter_select(input, output, ic_summary_df(), selected_site(), session)
  }
  
  
  
  observeEvent(input$sidebarItemExpanded, {
    if (input$sidebarItemExpanded == "<strong>LAIindicators</strong>") {
      updateTabItems(session, inputId = "sidebar", selected = "lai_overview")
    }
    if (input$sidebarItemExpanded == "<strong>ClinicDemographics</strong>") {
      updateTabItems(session, inputId = "sidebar", selected = "demographics_page")
    }
    if (input$sidebarItemExpanded == "<strong>Clinicaloutcomes</strong>") {
      updateTabItems(session, inputId = "sidebar", selected = "inj_page")
    }
  })
  
  output$indicator <- renderUI({
    req(credentials()$user_auth)
    selectInput("indicator",
                "Select an indicator",
                c("Demographics",
                  "Assessed",
                  'Educated',
                  'Interested',
                  'Screened',
                  "Eligible",
                  "Interested & Eligible",
                  "Prescribed",
                  "Initiated",
                  "Sustained"),
                selected = "Assessed")
  })
  
  # RENDER grouping_var UI
  output$grouping_var <- renderUI({
    req(credentials()$user_auth)
    selectInput("grouping_var", 
                "Comparison variable", 
                choices = c("Age" = "age_cat",
                            "Sex" = "sex_birth",
                            "Race" = "race",
                            "Ethnicity" = "ethnicity",
                            "Housing status" = "housing_status",
                            "Insurance status" = "insurance_status"),
                selected = "age_cat")
  })
  
  # OBSERVE changes to grouping_var and update filter_var
  observeEvent(input$grouping_var, {
    all_vars <- c("Age" = "age_cat",
                  "Sex" = "sex_birth",
                  "Race" = "race",
                  "Ethnicity" = "ethnicity",
                  "Housing status" = "housing_status",
                  "Insurance status" = "insurance_status")
    
    other_vars <- all_vars[all_vars != input$grouping_var]
    
    updateSelectInput(session, "filter_var",
                      choices = other_vars,
                      selected = other_vars[[1]])
  })
  
  # RENDER filter_var UI
  output$filter_var_ui <- renderUI({
    req(credentials()$user_auth)
    req(input$grouping_var)  # depends on this being ready
    selectInput("filter_var", "Filter by", choices = NULL)
  })
  
  # RENDER filter_select UI
  dynamic_filter_select <- function(input, output, ic_summary_df,selected_site, session){
    
    
    # Create a reactive expression for the filter options
    filter_options <- reactive({
      req(ic_summary_df, input$filter_var)
      
      grouping_var <- sym(input$grouping_var)
      filter_var <- sym(input$filter_var)
      
      temp <- ic_summary_df |>
        group_by(Variable,!!grouping_var,!!filter_var) |>
        summarize(Value = sum(Value), PWH1 = sum(PWH1)) |>
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
                                    .default =  lag(Variable)),
               prev = Value[match(prev_lab, Variable)]) |> 
        mutate(Percent=if_else(prev == 0, NA, Value/prev)) |>
        ungroup() |>
        # indicator selection here as a filter
        filter(Variable == if_else(input$indicator != "Demographics",input$indicator,"PWH"),
               prev > 0) 
      
      temp |> pull(!!filter_var) |>
        unique() |>
        as.character() |>
        sort()
    })
    
    
    # Render the checkbox group input
    output$filter_select <- renderUI({
      req(credentials()$user_auth)
      req(filter_options())
      
      checkboxGroupInput("filter_select",
                         "Select the groups you want to see",
                         choices = filter_options(),
                         selected = filter_options()[1])
    })
  }
  
  
  output$filter_by_year <- renderUI({
    req(credentials()$user_auth)
    checkboxInput('filter_by_year',label = "Filter time period",value = FALSE)
  })
  
  output$active_year_choice <- renderUI({
    req(credentials()$user_auth)
    selectInput("active_year", "Active year of patients",
                choices = c(2021,2022,2023,2024,2025),
                selected=2025)
    
  })
  
  # A reactiveVal to cache the computed max date from tbl()
  cached_max_date <- reactiveVal()
  
  # Compute and cache the max date only when tbl() changes
  observeEvent(tbl(), {

    # Defensive check in case 'tbl' has no date columns
    date_cols <- tbl() |>
      select(contains("date")&contains("icab_rpv"))
    
    if (ncol(date_cols) == 0) {
      cached_max_date(as.Date("2025-12-31"))  # Fallback default
    } else {
      max_date <- date_cols |>
        summarize(across(everything(), \(x) max(x, na.rm = TRUE))) |>
        pivot_longer(everything()) |>
        summarize(max(value, na.rm = TRUE)) |>
        pull()
      
      cached_max_date(max_date)
    }
  })
  
  computed_end_date <- reactive({
    req(input$active_year, cached_max_date())
    
    if (input$active_year == 2025) {
      cached_max_date()
    } else {
      as.Date(sprintf("%s-12-31", input$active_year))
    }
  })
  
  output$date_filter_ui <- renderUI({
    req(credentials()$user_auth, computed_end_date())
    
    dateRangeInput(
      inputId = 'date_filter',
      label = "Range of event dates",
      start = as.Date("2021-01-01"),
      end = computed_end_date(),
      min = as.Date("2021-01-01"),
      max = cached_max_date()
    )
  })
  

  
  output$data_explore_page <- renderUI({
    req(credentials()$user_auth)
    req(input$grouping_var)
    req(input$filter_var)
    req(input$filter_select)
    source(file='ui scripts/data_explore_ui.R', local= T)$value

  })
  
  # main pages
  # action links for scrolling
  demo_sections_info <- list(
    list(id = "top1", title = "Home", plot = NULL, download = NULL),
    list(id = "sex1", title = "Sex", plot = "sex1_plot", download = "sex1_download_ui"),
    list(id = "race1", title = "Race", plot = "race1_plot", download = "race1_download_ui"),
    list(id = "ethnicity1", title = "Ethnicity", plot = "ethnicity1_plot", download = "ethnicity1_download_ui"),
    list(id = "age1", title = "Age", plot = "age1_plot", download = "age1_download_ui"),
    list(id = "insurance1", title = "Insurance status", plot = "insurance1_plot", download = "insurance1_download_ui"),
    list(id = "housing1", title = "Housing status", plot = "housing1_plot", download = "housing1_download_ui"),
    list(id = "keypop1", title = "Key populations", plot = "keypop1_plot", download = "keypop1_download_ui")
  )
  
  output$demographics_page <- renderUI({
    req(credentials()$user_auth)
    
    fluidPage(
      fluidRow(
        column(
          width = 2,
          div(class = "toc-container",
              h4("Jump to Section"),
              div(class = "toc-links",
                  map(demo_sections_info, function(section) {
                    actionLink(inputId = paste0("go_", section$id), label = section$title)
                  })
              )
          )
        ),
        column(
          width = 10,
          # Special treatment for "Home"/top-level content
          div(id = "top1", h3(uiOutput("overall_n"), style = "font-size: 26px;")),
          box(id = "info1_box",
              title = "Instructions",
              width = 12,
              status = "info",
              solidHeader = TRUE,
              help_text$info1),
          # Plot boxes
          map(demo_sections_info[-1], function(section) {
            box(
              id = paste0(section$id, "_box"),
              title = section$title,
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              plotOutput(section$plot, height = "auto"),
              uiOutput(section$download),
              size = "xs"
            )
          })
        )
      )
    )
  })
  
  map(demo_sections_info, function(section) {
    observeEvent(input[[paste0("go_", section$id)]], {
      runjs(sprintf("
      const target = document.getElementById('%s_box') || document.getElementById('%s');
      if (target) {
        window.scrollTo({
          top: target.getBoundingClientRect().top + window.scrollY - 120,
          behavior: 'smooth'
        });
      }
    ", section$id, section$id))
    })
  })
  
# demographics by LAI page
  demo_sections_info_b <- list(
    list(id = "top1b", title = "Home", plot = NULL, download = NULL),
    list(id = "sex1b", title = "Sex", plot = "sex1b_plot", download = "sex1b_download_ui"),
    list(id = "race1b", title = "Race", plot = "race1b_plot", download = "race1b_download_ui"),
    list(id = "ethnicity1b", title = "Ethnicity", plot = "ethnicity1b_plot", download = "ethnicity1b_download_ui"),
    list(id = "age1b", title = "Age", plot = "age1b_plot", download = "age1b_download_ui"),
    list(id = "insurance1b", title = "Insurance status", plot = "insurance1b_plot", download = "insurance1b_download_ui"),
    list(id = "housing1b", title = "Housing status", plot = "housing1b_plot", download = "housing1b_download_ui"),
    list(id = "keypop1b", title = "Key populations", plot = "keypop1b_plot", download = "keypop1b_download_ui")
  )
  
  output$demo_by_lai <- renderUI({
    req(credentials()$user_auth)
    
    fluidPage(
      fluidRow(
        column(
          width = 2,
          div(class = "toc-container",
              h4("Jump to Section"),
              div(class = "toc-links",
                  map(demo_sections_info_b, function(section) {
                    actionLink(inputId = paste0("go_", section$id), label = section$title)
                  })
              )
          )
        ),
        column(
          width = 10,
          # Special treatment for "Home"/top-level content
          div(id = "top1", h3(uiOutput("overall_n"), style = "font-size: 26px;")),
          box(id = "info1b_box",
              title = "Instructions",
              width = 12,
              status = "info",
              solidHeader = TRUE,
              help_text$info1b),
          # Plot boxes
          map(demo_sections_info_b[-1], function(section) {
            box(
              id = paste0(section$id, "_box"),
              title = section$title,
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              plotOutput(section$plot, height = "auto"),
              uiOutput(section$download),
              size = "xs"
            )
          })
        )
      )
    )
  })
  
  map(demo_sections_info_b, function(section) {
    observeEvent(input[[paste0("go_", section$id)]], {
      runjs(sprintf("
      const target = document.getElementById('%s_box') || document.getElementById('%s');
      if (target) {
        window.scrollTo({
          top: target.getBoundingClientRect().top + window.scrollY - 120,
          behavior: 'smooth'
        });
      }
    ", section$id, section$id))
    })
  })
  
  lai_overview_sections <- list(
    list(id = "top0",title = "Home",plot = NULL, download = NULL),
    list(id = "care_gap", title = "LAI Care Gap Analysis", plot = "lai_care_gap_plot",
         download = "lai_care_gap_download_ui"),
    list(id = "assessed_outcomes", title = "Outcomes among those assessed", plot = NULL,
         download = NULL)
  )
  
  output$lai_overview <- renderUI({
    req(credentials()$user_auth)
    
    fluidPage(
      fluidRow(
        column(
          width = 2,
          div(class = "toc-container",
              h4("Jump to Section"),
              div(class = "toc-links",
                  map(lai_overview_sections, function(section) {
                    actionLink(inputId = paste0("go_", section$id), label = section$title)
                  })
              )
          )
        ),
        column(
          width = 10,
          # Special treatment for "Home"/top-level content
          div(id = "top0", h3(uiOutput("overall_n"), style = "font-size: 26px;")),
          box(id = "info0_box",
              title = "Instructions",
              width = 12,
              status = "info",
              solidHeader = TRUE,
              help_text$info0),
          # Plot boxes
          map(lai_overview_sections[2], function(section) {
            box(
              id = paste0(section$id, "_box"),
              title = section$title,
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              plotOutput(section$plot, height = "auto"),
              uiOutput(section$download),
              size = "xs"
            )
          }),
          box(id = "assessed_outcomes_box",
              title = "Outcomes among those assessed",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              div(style = "display:flex; align-items:top; gap:10px;",  
                  div("Percent"),  
                  radioButtons(
                    inputId = "int_elig_pct",label = NULL, 
                    choices = c("Table", "Row", "Column"),
                    selected = "Table",
                    inline = TRUE)),
              gt_output(outputId = "int_elig_table"),
              downloadButton(outputId = "int_elig_table_download",
                             label = "Download table",
                             icon = icon("table")),
              size = "xs")
        )
      )
    )
  })
  
  map(lai_overview_sections, function(section) {
    observeEvent(input[[paste0("go_", section$id)]], {
      runjs(sprintf("
      const target = document.getElementById('%s_box') || document.getElementById('%s');
      if (target) {
        window.scrollTo({
          top: target.getBoundingClientRect().top + window.scrollY - 120,
          behavior: 'smooth'
        });
      }
    ", section$id, section$id))
    })
  })
  
  
  
  assessed_sections_info <- list(
    list(id = "top2", title = "Home", plot = NULL, download = NULL),
    list(id = "overall2", title = "Overall", plot = "assessed_overall_plot", download = "assessed_overall_download_ui"                 ),  
    list(id = "sex2", title = "Sex", plot = "sex2_plot", download = "sex2_download_ui"                                                 ),
    list(id = "race2", title = "Race", plot = "race2_plot", download = "race2_download_ui"                                             ),
    list(id = "ethnicity2", title = "Ethnicity", plot = "ethnicity2_plot", download = "ethnicity2_download_ui"                         ),
    list(id = "age2", title = "Age", plot = "age2_plot", download = "age2_download_ui"                                                 ),
    list(id = "insurance2", title = "Insurance status", plot = "insurance2_plot", download = "insurance2_download_ui"                  ),
    list(id = "housing2", title = "Housing status", plot = "housing2_plot", download = "housing2_download_ui"                          ),
    list(id = "keypop2", title = "Key populations", plot = "keypop2_plot", download = "keypop2_download_ui"                            ),
    list(id = "time2", title = "Assessed over time by person", plot = "time2_plot", download = "time2_download_ui"                     ),
    list(id = "time2_event", title = "Assessed over time by encounter", plot = "time2_event_plot", download = "time2_event_download_ui")
  )

  # Assessed
  renderSectionPage(
    input, output, credentials,
    page_id = "assessed_page",
    sections_info = assessed_sections_info,
    n_output_id = "assessed_n"
  )
  
  educated_sections_info <- list(
    list(id = "top3", title = "Home", plot = NULL, download = NULL),
    list(id = "overall3", title = "Overall", plot = "educated_overall_plot", download = "educated_overall_download_ui"),
    list(id = "sex3", title = "Sex", plot = "sex3_plot", download = "sex3_download_ui"),
    list(id = "race3", title = "Race", plot = "race3_plot", download = "race3_download_ui"),
    list(id = "ethnicity3", title = "Ethnicity", plot = "ethnicity3_plot", download = "ethnicity3_download_ui"),
    list(id = "age3", title = "Age", plot = "age3_plot", download = "age3_download_ui"),
    list(id = "insurance3", title = "Insurance status", plot = "insurance3_plot", download = "insurance3_download_ui"),
    list(id = "housing3", title = "Housing status", plot = "housing3_plot", download = "housing3_download_ui"),
    list(id = "keypop3", title = "Key populations", plot = "keypop3_plot", download = "keypop3_download_ui"),
    list(id = "time3", title = "Educated over time by person", plot = "time3_plot", download = "time3_download_ui"),
    list(id = "time3_event", title = "Educated over time by encounter", plot = "time3_event_plot", download = "time3_event_download_ui")
  )
  
  # Educated
  renderSectionPage(
    input, output, credentials,
    page_id = "educated_page",
    sections_info = educated_sections_info,
    n_output_id = "educated_n"
  )
  
  interested_sections_info <- list(
    list(id = "top4", title = "Home", plot = NULL, download = NULL),
    list(id = "overall4", title = "Overall", plot = "interested_overall_plot", download = "interested_overall_download_ui"),
    list(id = "sex4", title = "Sex", plot = "sex4_plot", download = "sex4_download_ui"),
    list(id = "race4", title = "Race", plot = "race4_plot", download = "race4_download_ui"),
    list(id = "ethnicity4", title = "Ethnicity", plot = "ethnicity4_plot", download = "ethnicity4_download_ui"),
    list(id = "age4", title = "Age", plot = "age4_plot", download = "age4_download_ui"),
    list(id = "insurance4", title = "Insurance status", plot = "insurance4_plot", download = "insurance4_download_ui"),
    list(id = "housing4", title = "Housing status", plot = "housing4_plot", download = "housing4_download_ui"),
    list(id = "keypop4", title = "Key populations", plot = "keypop4_plot", download = "keypop4_download_ui"),
    list(id = "time4", title = "Interested over time by person", plot = "time4_plot", download = "time4_download_ui"),
    list(id = "time4_event", title = "Interested over time by encounter", plot = "time4_event_plot", download = "time4_event_download_ui"),
    list(id = "reason4", title = "Not interested reasons", plot = "not_interested_reason_plot", download = "not_interested_reason_download_ui")
  )
  
  # interested
  renderSectionPage(
    input, output, credentials,
    page_id = "interested_page",
    sections_info = interested_sections_info,
    n_output_id = "interested_n"
  )
  
  screened_sections_info <- list(
    list(id = "top5", title = "Home", plot = NULL, download = NULL),
    list(id = "overall5", title = "Overall", plot = "screened_overall_plot", download = "screened_overall_download_ui"),
    list(id = "sex5", title = "Sex", plot = "sex5_plot", download = "sex5_download_ui"),
    list(id = "race5", title = "Race", plot = "race5_plot", download = "race5_download_ui"),
    list(id = "ethnicity5", title = "Ethnicity", plot = "ethnicity5_plot", download = "ethnicity5_download_ui"),
    list(id = "age5", title = "Age", plot = "age5_plot", download = "age5_download_ui"),
    list(id = "insurance5", title = "Insurance status", plot = "insurance5_plot", download = "insurance5_download_ui"),
    list(id = "housing5", title = "Housing status", plot = "housing5_plot", download = "housing5_download_ui"),
    list(id = "keypop5", title = "Key populations", plot = "keypop5_plot", download = "keypop5_download_ui"),
    list(id = "time5", title = "Screened over time by person", plot = "time5_plot", download = "time5_download_ui"),
    list(id = "time5_event", title = "Screened over time by encounter", plot = "time5_event_plot", download = "time5_event_download_ui")
 )
  
  # screened
  renderSectionPage(
    input, output, credentials,
    page_id = "screened_page",
    sections_info = screened_sections_info,
    n_output_id = "screened_n"
  )
  
  eligible_sections_info <- list(
    list(id = "top6", title = "Home", plot = NULL, download = NULL),
    list(id = "overall6", title = "Overall", plot = "eligible_overall_plot", download = "eligible_overall_download_ui"),
    list(id = "sex6", title = "Sex", plot = "sex6_plot", download = "sex6_download_ui"),
    list(id = "race6", title = "Race", plot = "race6_plot", download = "race6_download_ui"),
    list(id = "ethnicity6", title = "Ethnicity", plot = "ethnicity6_plot", download = "ethnicity6_download_ui"),
    list(id = "age6", title = "Age", plot = "age6_plot", download = "age6_download_ui"),
    list(id = "insurance6", title = "Insurance status", plot = "insurance6_plot", download = "insurance6_download_ui"),
    list(id = "housing6", title = "Housing status", plot = "housing6_plot", download = "housing6_download_ui"),
    list(id = "keypop6", title = "Key populations", plot = "keypop6_plot", download = "keypop6_download_ui"),
    list(id = "time6", title = "Eligible over time by person", plot = "time6_plot", download = "time6_download_ui"),
    list(id = "time6_event", title = "Eligible over time by encounter", plot = "time6_event_plot", download = "time6_event_download_ui"),
    list(id = "reason6", title = "Not eligible reasons", plot = "not_eligible_reason_plot", download = "not_eligible_reason_download_ui")
  )
  
  # eligible
  renderSectionPage(
    input, output, credentials,
    page_id = "eligible_page",
    sections_info = eligible_sections_info,
    n_output_id = "eligible_n"
  )
  
  prescribed_sections_info <- list(
    list(id = "top7", title = "Home", plot = NULL, download = NULL),
    list(id = "overall7", title = "Overall", plot = "prescribed_overall_plot", download = "prescribed_overall_download_ui"),
    list(id = "sex7", title = "Sex", plot = "sex7_plot", download = "sex7_download_ui"),
    list(id = "race7", title = "Race", plot = "race7_plot", download = "race7_download_ui"),
    list(id = "ethnicity7", title = "Ethnicity", plot = "ethnicity7_plot", download = "ethnicity7_download_ui"),
    list(id = "age7", title = "Age", plot = "age7_plot", download = "age7_download_ui"),
    list(id = "insurance7", title = "Insurance status", plot = "insurance7_plot", download = "insurance7_download_ui"),
    list(id = "housing7", title = "Housing status", plot = "housing7_plot", download = "housing7_download_ui"),
    list(id = "keypop7", title = "Key populations", plot = "keypop7_plot", download = "keypop7_download_ui"),
    list(id = "time7", title = "Prescribed over time", plot = "time7_plot", download = "time7_download_ui")
 )
  
  # prescribed
  renderSectionPage(
    input, output, credentials,
    page_id = "prescribed_page",
    sections_info = prescribed_sections_info,
    n_output_id = "prescribed_n"
  )
  
  initiated_sections_info <- list(
    list(id = "top8", title = "Home", plot = NULL, download = NULL),
    list(id = "overall8", title = "Overall", plot = "initiated_overall_plot", download = "initiated_overall_download_ui"),
    list(id = "sex8", title = "Sex", plot = "sex8_plot", download = "sex8_download_ui"),
    list(id = "race8", title = "Race", plot = "race8_plot", download = "race8_download_ui"),
    list(id = "ethnicity8", title = "Ethnicity", plot = "ethnicity8_plot", download = "ethnicity8_download_ui"),
    list(id = "age8", title = "Age", plot = "age8_plot", download = "age8_download_ui"),
    list(id = "insurance8", title = "Insurance status", plot = "insurance8_plot", download = "insurance8_download_ui"),
    list(id = "housing8", title = "Housing status", plot = "housing8_plot", download = "housing8_download_ui"),
    list(id = "keypop8", title = "Key populations", plot = "keypop8_plot", download = "keypop8_download_ui"),
    list(id = "time8", title = "Initiated over time", plot = "time8_plot", download = "time8_download_ui")
  )
  
  # initiated
  renderSectionPage(
    input, output, credentials,
    page_id = "initiated_page",
    sections_info = initiated_sections_info,
    n_output_id = "initiated_n"
  )
  
  sustained_sections_info <- list(
    list(id = "top9", title = "Home", plot = NULL, download = NULL),
    list(id = "overall9", title = "Overall", plot = "sustained_overall_plot", download = "sustained_overall_download_ui"),
    list(id = "sex9", title = "Sex", plot = "sex9_plot", download = "sex9_download_ui"),
    list(id = "race9", title = "Race", plot = "race9_plot", download = "race9_download_ui"),
    list(id = "ethnicity9", title = "Ethnicity", plot = "ethnicity9_plot", download = "ethnicity9_download_ui"),
    list(id = "age9", title = "Age", plot = "age9_plot", download = "age9_download_ui"),
    list(id = "insurance9", title = "Insurance status", plot = "insurance9_plot", download = "insurance9_download_ui"),
    list(id = "housing9", title = "Housing status", plot = "housing9_plot", download = "housing9_download_ui"),
    list(id = "keypop9", title = "Key populations", plot = "keypop9_plot", download = "keypop9_download_ui"),
    list(id = "time9", title = "Time spent on CAB", plot = "time9_plot", download = "time9_download_ui"),
    list(id = "reason9", title = "Discontinued reasons", plot = "discontinued_reason_plot", download = "discontinued_reason_download_ui")
  )
  
  # sustained
  renderSectionPage(
    input, output, credentials,
    page_id = "sustained_page",
    sections_info = sustained_sections_info,
    n_output_id = "sustained_n"
  )
  
  clinical_sections_info <- list(
    list(id = "top10", title = "Home", plot = NULL, download = NULL),
    list(id = "status_bar", title = "On time injections", plot = "ontime_status_bar", download = "ontime_status_download_ui"),
    list(id = "ontime_1m", title = "On time injections by days since prior injection, monthly injection interval", plot = "ontime_plot_monthly", download = "ontime_monthly_download_ui"),
    list(id = "ontime_2m", title = "On time injections by days since prior injection, bimonthly injection interval", plot = "ontime_plot_bimonthly", download = "ontime_bimonthly_download_ui"),
    list(id = "late_pt", title = "Late injections by patient", plot = "late_pt_plot", download = "late_pt_download_ui"),
    list(id = "early_pt", title = "Early injections by patient", plot = "early_pt_plot", download = "early_pt_download_ui")
  )
  
  renderSectionPage(
    input, output, credentials,
    page_id = "inj_page",
    sections_info = clinical_sections_info,
    n_output_id = "clinical_n"
  )
  
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
  
  # VL page
  vl_sections_info <- vl_sections_info <- reactive({
    list(
      list(id = "top11", title = "Home", plot = NULL, download = NULL),
      list(id = "time_to_vs", title = str_c("Time to first VL ",val1()), plot = "vl_time_to_vs", download = "vl_time_to_vs_download_ui"),
      list(id = "time_to_el_vl1", title = str_c("Time to first VL ",val2()," pre-CAB VL ",val1()), plot = "time_to_el_vl1", download = "time_to_el_vl1_download_ui"),
      list(id = "time_to_el_vl2", title = str_c("Time to first VL ",val2()," pre-CAB VL ",val2()), plot = "time_to_el_vl2", download = "time_to_el_vl2_download_ui"),
      list(id = "time_to_failure1", title = str_c("Time to first virologic failure, pre-CAB VL ",val1()), plot = "time_to_failure1", download = "time_to_failure1_download_ui"),  
      list(id = "time_to_failure2", title = str_c("Time to first virologic failure, pre-CAB VL ",val2()), plot = "time_to_failure2", download = "time_to_failure2_download_ui")
    )
  })
  
  observe({
    renderSectionPage(
      input, output, credentials,
      page_id = "vl_page",
      sections_info = vl_sections_info(),  
      n_output_id = "vl_n"
    )
  })
  
}
