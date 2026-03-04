
options(dplyr.summarise.inform = FALSE)
options(shiny.maxRequestSize = 30 * 1024^2)
theme_set(theme_minimal())
font_add(family = "Roboto", regular = "Roboto-Regular.ttf", bold = "Roboto-Bold.ttf")
showtext_auto()
showtext_opts(dpi = 96)

source("server scripts/main_page_server.R")
source("server scripts/data_explore_server.R")
source("server scripts/server_util.R")
source("server scripts/render_scroll_page.R")
source("server scripts/help_text.R")




server <- function(input, output, session) {
  
  observeEvent(input$file1, {
    file_path <- input$file1$datapath

    # Get all sheet names from the uploaded Excel file
    sheet_names <- excel_sheets(file_path)

    # Dynamically update the 'select_sheet' input with the found names
    updateSelectInput(session,
                      inputId = "select_sheet",
                      choices = sheet_names,
                      selected = sheet_names[1] # Automatically select the first sheet
    )
  })
  
  observe({
    file_ready <- !is.null(input$file1)
    name_ready <- !is.null(input$site_name_input) && input$site_name_input != ""
    if (file_ready && name_ready){
      enable("go_button")
    } else {
      disable("go_button")
    }
  })
  
  # loading in the master dataset and processing it 
  raw_data <- eventReactive(input$go_button, {
    req(input$file1,input$select_sheet)

    result <- tryCatch({
      data <- read_excel(input$file1$datapath,
                         sheet = input$select_sheet,
                         col_types = "text",
                         na = c("NA","UNK", ".", "C","888888","999999")) |>
        load_and_process_data()
      
      data
      
    }, error = function(e) {
      showNotification(
        paste("Error processing data. Please check your data carefully.\n
              The R error message was:", conditionMessage(e)),
        type = "error",
        duration = 10,
        id = "processing_error" # Prevents duplicate notifications
      )
      
      return(NULL)
    })
    
    req(result)
    return(result)
    
  })
  
  output$home_page <-  renderUI({
    source(file='ui scripts/home.R', local= T)$value
  })
  

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
  
  site_list <- reactive({
    req(input$file1,input$select_sheet)
    
    raw_data() |>
      pull(site) |>
      unique() |>
      str_sort()
  })
  
  output$site_choice <- renderUI({
    req(input$file1,input$select_sheet)
    req(length(site_list()) > 1)
    choice_list <- c(str_c(input$site_name_input, " (all sites)"),site_list())
    
    freezeReactiveValue(input, "site_choice_input")
    
    selectInput("site_choice_input", "Choose one of the sites below:",
                choices = choice_list,
                selected = choice_list[1])
    
  })
  
  trigger_site_filter <- reactiveVal(FALSE)
  
  observe({
    req(raw_data())
    if (is.null(input$site_choice_input) ||
        !input$site_choice_input %in% site_list()) {
      trigger_site_filter(FALSE)
    } else if (input$site_choice_input %in% site_list()){
      trigger_site_filter(TRUE)
    } 
  }) 
  
  df <- reactive({
    req(raw_data())
    withProgress(message = "Processing data",
                 detail = "This may take a moment...",
                 {
                   if (trigger_site_filter() == FALSE) {
                     return(raw_data())
                   } else if (trigger_site_filter() == TRUE){
                     return(raw_data() |>
                              filter(site == input$site_choice_input))
                   }
                 })
  }) 
  
  tbl <- reactive({
    req(df())
    withProgress(message = "Processing data",
                 detail = "This may take a moment...",
                 value = 0.4,
                 {
                   if (input$filter_by_year == TRUE &&
                       length(input$date_filter) == 2 & !is.null(input$active_year)) {
                     get_current_year_data(df(),
                                           as.numeric(input$active_year),
                                           filter_dates = TRUE,
                                           start_date = as.Date(input$date_filter[1]),
                                           end_date = as.Date(input$date_filter[2]))
                   }
                   else {
                     df()  # fallback if no date range selected
                   } 
                 })
    
  })
  
  cab_master_df <- reactive({
    req(tbl(), interval_1(), interval_2())
    withProgress(message = "Processing data",
                 detail = "This may take a moment...",
                 value = 0.8,
                 {
                   prepare_cab_master_df(tbl(), interval_1(), interval_2())
                 })
  })
  
  ic_summary_df <- reactive({
    req(tbl())
    withProgress(message = "Processing data",
                 detail = "This may take a moment...",
                 value = 0.6,
                 {
                   prepare_ic_summary(tbl())
                 })
    
  })
  
  active_year_options <- reactive({
    req(df())
    df() |>
      select(contains("active")) |>
      names() |>
      str_extract("\\d+") |>
      as.numeric() |>
      sort() 
  })

  observeEvent(ic_summary_df(),{
    updateActionButton(session, "go_button",
                       label = "Data is ready",
                       icon = icon("check"))
  })
  
  observe({
    req(tbl(), ic_summary_df(), cab_master_df())
      
      if (length(site_list()) > 1){
        selected_site <- input$site_choice_input
      } else {
        selected_site <- input$site_name_input
      }
      main_page_server(input, output, tbl(), ic_summary_df(), selected_site, cab_master_df(), session)
      dynamic_filter_select(input, output, ic_summary_df(), selected_site, session)
      data_explore_server(input, output, ic_summary_df(), session)

  })
  
  full_report_data <- reactive({
    req(tbl(), ic_summary_df(), cab_master_df())
    full_report_table(ic_summary_df(), cab_master_df())
  })
  
  output$full_report_download <- download_table("ALAI_UP_Report",full_report_data())
  
  output$full_report_download_ui <- renderUI({
    req(ic_summary_df()) 
    
    downloadButton("full_report_download", "Download Results",
                   style = "color: #333; background-color: #f4f4f4; border-color: #ccc; 
                            margin-left: 20px; display: inline-block;")
  })
  

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
  
  # Define the items and their properties in a simple list
  menu_items <- list(
    list(id = "assessed",  label = strong("Assessed"),              tab = "assessed_page",   ic = NULL),
    list(id = "educated",  label = "Educated",                      tab = "educated_page",   ic = "angle-double-right"),
    list(id = "interested",label = HTML("&nbsp;&nbsp; Interested"), tab = "interested_page", ic = "angle-double-right"),
    list(id = "screened",  label = "Screened",                      tab = "screened_page",   ic = "angle-double-right"),
    list(id = "eligible",  label = HTML("&nbsp;&nbsp; Eligible"),   tab = "eligible_page",   ic = "angle-double-right")
  )
  
  # Use a loop to create all 5 renderMenu functions at once
  map(menu_items, function(item) {
    output[[paste0(item$id, "_sidebar")]] <- renderMenu({
      
      req(input$assessed_choice == "Yes") # Stops rendering if choice is not "Yes"
      
      menuItem(
        text = item$label,
        tabName = item$tab,
        icon = if(!is.null(item$ic)) icon(item$ic) else NULL
      )
    })
  })
  
  observeEvent(input$assessed_choice, {
    if (input$assessed_choice == "No") {
      # If they are on any of the 'Assessed' related tabs, kick them back to home
      if (input$sidebar %in% c('assessed_page', 'educated_page', 'interested_page', 'screened_page', 'eligible_page')) {
        updateTabItems(session, "sidebar", "lai_overview")
      }
    }
  })
  
  output$indicator <- renderUI({
    req(input$file1)
    if (input$assessed_choice == "Yes"){
      choice_list <- c("Demographics",
                       "Assessed",
                       'Educated',
                       'Interested',
                       'Screened',
                       "Eligible",
                       "Interested & Eligible",
                       "Prescribed",
                       "Initiated",
                       "Sustained")
    } else {
      choice_list <- c("Demographics",
                       "Prescribed",
                       "Initiated",
                       "Sustained")
    }
    selectInput("indicator",
                "Select an indicator",
                choices = choice_list,
                selected = "Demographics")
  })
  
  # RENDER grouping_var UI
  output$grouping_var <- renderUI({
    req(input$file1)
    choice_list <- c("Age" = "age_cat",
                     "Sex" = "sex_birth",
                     "Race" = "race",
                     "Ethnicity" = "ethnicity",
                     "Insurance status" = "insurance_status",
                     "Housing status" = "housing_status",
                     "Gender" = "gender_id",
                     "Risk MSM" = "risk_msm",
                     "Risk IDU" = "risk_idu",
                     "Risk Heterosex" = "risk_heterosex",
                     "Employment status" = "employment_status",
                     "Poverty level" = "poverty_level",
                     "Immigration status" = "immigration_status_undoc",
                     "Language" = "language",
                     "Incarceration history" = "incarceration_history",
                     "Recent CD4" = "cd4_recent_result")
  
    if (length(site_list()) > 1){
      choice_list <- c(choice_list,c("Site" = "site"))
    }
    
    selectInput("grouping_var", 
                "Comparison variable", 
                choices = choice_list,
                selected = "age_cat")
  })
  
  # OBSERVE changes to grouping_var and update filter_var
  observeEvent(input$grouping_var, {
    all_vars <-  c("Age" = "age_cat",
                   "Sex" = "sex_birth",
                   "Race" = "race",
                   "Ethnicity" = "ethnicity",
                   "Insurance status" = "insurance_status",
                   "Housing status" = "housing_status",
                   "Gender" = "gender_id",
                   "Risk MSM" = "risk_msm",
                   "Risk IDU" = "risk_idu",
                   "Risk Heterosex" = "risk_heterosex",
                   "Employment status" = "employment_status",
                   "Poverty level" = "poverty_level",
                   "Immigration status" = "immigration_status_undoc",
                   "Language" = "language",
                   "Incarceration history" = "incarceration_history",
                   "Recent CD4" = "cd4_recent_result")
    
    if (length(site_list()) > 1){
      all_vars <- c(all_vars,c("Site" = "site"))
    }
    
    other_vars <- all_vars[all_vars != input$grouping_var]
    
    updateSelectInput(session, "filter_var",
                      choices = other_vars,
                      selected = other_vars[[1]])
  })
  
  # RENDER filter_var UI
  output$filter_var_ui <- renderUI({
    req(input$file1)
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
      req(input$file1)
      req(filter_options())
      
      checkboxGroupInput("filter_select",
                         "Select the groups you want to see",
                         choices = filter_options(),
                         selected = filter_options()[1])
    })
  }
  
  
  output$filter_by_year <- renderUI({
    req(input$file1)
    checkboxInput('filter_by_year',label = "Filter time period",value = FALSE)
  })
  
  output$active_year_choice <- renderUI({
    req(active_year_options())
    
    selectInput("active_year", "Active year of patients",
                choices = active_year_options(),
                selected = active_year_options() |> last())
    
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
    req(input$file1, computed_end_date())
    
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
    req(input$file1)
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
    list(id = "keypop1", title = "Key populations", plot = NULL, download = NULL),
    list(id = "zip1", title = "ZIP code", plot = NULL, download = NULL)
  )
  
  output$demographics_page <- renderUI({
    req(input$file1)
    
    choice_list <- c("Housing status","Gender",
                     "Risk MSM","Risk IDU","Risk Heterosex",
                     "Employment status","Poverty level",
                     "Immigration status","Language",
                     "Incarceration history","Recent CD4")
    
    if (length(site_list()) > 1){
      choice_list <- c(choice_list,"Site")
    } 
    
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
          map(demo_sections_info[2:6], function(section) {
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
          #box for key pop
          box(id = "keypop1_box",
              title = "Key Populations",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              selectInput(
                inputId = "keypop1_choice",
                label = "Key population choice", 
                choices = choice_list,
                selected = "Housing status"),
              plotOutput("keypop1_plot",, height = "auto"),
              uiOutput("keypop1_download_ui")),
          # Box for the map
          box(id = "zip1_box",
              title = "ZIP codes",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              actionButton("render_map_button",
                           label = "Create map",
                           icon = icon("play")),
              leafletOutput("zip_map"),
              downloadButton(outputId = "map_data_download", label = "Download data"))
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
    list(id = "keypop1b", title = "Key populations", plot = NULL, download = NULL)
  )
  
  output$demo_by_lai <- renderUI({
    req(input$file1)
    
    choice_list <- c("Housing status","Gender",
                     "Risk MSM","Risk IDU","Risk Heterosex",
                     "Employment status","Poverty level",
                     "Immigration status","Language",
                     "Incarceration history","Recent CD4")
    
    if (length(site_list()) > 1){
      choice_list <- c(choice_list,"Site")
    } 
    
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
          map(demo_sections_info_b[2:6], function(section) {
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
          box(id = "keypop1b_box",
              title = "Key Populations",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              selectInput(
                inputId = "keypop1b_choice",
                label = "Key population choice", 
                choices = choice_list,
                selected = "Housing status"),
              plotOutput("keypop1b_plot",, height = "auto"),
              uiOutput("keypop1b_download_ui"))
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
  
  lai_overview_sections <- reactive({
    sections <- list(
      list(id = "top0",title = "Home",plot = NULL, download = NULL),
      list(id = "care_gap", title = "LAI Care Gap Analysis", plot = "lai_care_gap_plot",
           download = "lai_care_gap_download_ui")
    )
    
    if (input$assessed_choice == "Yes"){
      sections <- append(sections, list(
        
        list(id = "assessed_outcomes", title = "Outcomes among those assessed", plot = NULL,
             download = NULL)
      ))
    }
    return(sections)
  })
  
  output$lai_overview <- renderUI({
    req(input$file1)
    
    if (input$assessed_choice == "Yes"){
      int_elig_box <- box(id = "assessed_outcomes_box",
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
    } else {
      int_elig_box <- NULL
    }
    
    fluidPage(
      fluidRow(
        column(
          width = 2,
          div(class = "toc-container",
              h4("Jump to Section"),
              div(class = "toc-links",
                  map(lai_overview_sections(), function(section) {
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
          map(lai_overview_sections()[2], function(section) {
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
          int_elig_box
        )
      )
    )
  })
  
  observe({
    map(lai_overview_sections(), function(section) {
      local({
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
    })
  })
  

  keypop_choice_list <- reactive({
    temp <- c("Housing status","Gender",
      "Risk MSM","Risk IDU","Risk Heterosex",
      "Employment status","Poverty level",
      "Immigration status","Language",
      "Incarceration history","Recent CD4")
    
    if (length(site_list()) > 1){
      temp <- c(temp,"Site")
    }
    
    return(temp)
  }) 
  
  assessed_sections_info <- list(
    list(id = "top2", title = "Home", plot = NULL, download = NULL),
    list(id = "overall2", title = "Overall", plot = "assessed_overall_plot", download = "assessed_overall_download_ui"                 ),  
    list(id = "sex2", title = "Sex", plot = "sex2_plot", download = "sex2_download_ui"                                                 ),
    list(id = "race2", title = "Race", plot = "race2_plot", download = "race2_download_ui"                                             ),
    list(id = "ethnicity2", title = "Ethnicity", plot = "ethnicity2_plot", download = "ethnicity2_download_ui"                         ),
    list(id = "age2", title = "Age", plot = "age2_plot", download = "age2_download_ui"                                                 ),
    list(id = "insurance2", title = "Insurance status", plot = "insurance2_plot", download = "insurance2_download_ui"                  ),
    list(id = "keypop2", title = "Key populations", plot = "keypop2_plot", download = "keypop2_download_ui"                            ),
    list(id = "time2", title = "Assessed over time by person", plot = "time2_plot", download = "time2_download_ui"                     ),
    list(id = "time2_event", title = "Assessed over time by encounter", plot = "time2_event_plot", download = "time2_event_download_ui")
  )

  # Assessed
  renderSectionPage(
    input, output, 
    page_id = "assessed_page",
    sections_info = assessed_sections_info,
    n_output_id = "assessed_n"
  )
  
  observe({
    updateSelectInput(session, "keypop2_choice",
                      choices = keypop_choice_list(),
                      selected = input$keypop2_choice) # Preserve user selection
  })
  
  educated_sections_info <- list(
    list(id = "top3", title = "Home", plot = NULL, download = NULL),
    list(id = "overall3", title = "Overall", plot = "educated_overall_plot", download = "educated_overall_download_ui"),
    list(id = "sex3", title = "Sex", plot = "sex3_plot", download = "sex3_download_ui"),
    list(id = "race3", title = "Race", plot = "race3_plot", download = "race3_download_ui"),
    list(id = "ethnicity3", title = "Ethnicity", plot = "ethnicity3_plot", download = "ethnicity3_download_ui"),
    list(id = "age3", title = "Age", plot = "age3_plot", download = "age3_download_ui"),
    list(id = "insurance3", title = "Insurance status", plot = "insurance3_plot", download = "insurance3_download_ui"),
    list(id = "keypop3", title = "Key populations", plot = "keypop3_plot", download = "keypop3_download_ui"),
    list(id = "time3", title = "Educated over time by person", plot = "time3_plot", download = "time3_download_ui"),
    list(id = "time3_event", title = "Educated over time by encounter", plot = "time3_event_plot", download = "time3_event_download_ui")
  )
  
  
  
  # Educated
  renderSectionPage(
    input, output, 
    page_id = "educated_page",
    sections_info = educated_sections_info,
    n_output_id = "educated_n"
  )
  
  observe({
    updateSelectInput(session, "keypop3_choice",
                      choices = keypop_choice_list(),
                      selected = input$keypop3_choice)
  })
  
  interested_sections_info <- list(
    list(id = "top4", title = "Home", plot = NULL, download = NULL),
    list(id = "overall4", title = "Overall", plot = "interested_overall_plot", download = "interested_overall_download_ui"),
    list(id = "sex4", title = "Sex", plot = "sex4_plot", download = "sex4_download_ui"),
    list(id = "race4", title = "Race", plot = "race4_plot", download = "race4_download_ui"),
    list(id = "ethnicity4", title = "Ethnicity", plot = "ethnicity4_plot", download = "ethnicity4_download_ui"),
    list(id = "age4", title = "Age", plot = "age4_plot", download = "age4_download_ui"),
    list(id = "insurance4", title = "Insurance status", plot = "insurance4_plot", download = "insurance4_download_ui"),
    list(id = "keypop4", title = "Key populations", plot = "keypop4_plot", download = "keypop4_download_ui"),
    list(id = "time4", title = "Interested over time by person", plot = "time4_plot", download = "time4_download_ui"),
    list(id = "time4_event", title = "Interested over time by encounter", plot = "time4_event_plot", download = "time4_event_download_ui"),
    list(id = "reason4", title = "Not interested reasons", plot = "not_interested_reason_plot", download = "not_interested_reason_download_ui")
  )
  
  # interested
  renderSectionPage(
    input, output, 
    page_id = "interested_page",
    sections_info = interested_sections_info,
    n_output_id = "interested_n"
  )
  
  observe({
    updateSelectInput(session, "keypop4_choice",
                      choices = keypop_choice_list(),
                      selected = input$keypop4_choice)
  })
  
  screened_sections_info <- list(
    list(id = "top5", title = "Home", plot = NULL, download = NULL),
    list(id = "overall5", title = "Overall", plot = "screened_overall_plot", download = "screened_overall_download_ui"),
    list(id = "sex5", title = "Sex", plot = "sex5_plot", download = "sex5_download_ui"),
    list(id = "race5", title = "Race", plot = "race5_plot", download = "race5_download_ui"),
    list(id = "ethnicity5", title = "Ethnicity", plot = "ethnicity5_plot", download = "ethnicity5_download_ui"),
    list(id = "age5", title = "Age", plot = "age5_plot", download = "age5_download_ui"),
    list(id = "insurance5", title = "Insurance status", plot = "insurance5_plot", download = "insurance5_download_ui"),
    list(id = "keypop5", title = "Key populations", plot = "keypop5_plot", download = "keypop5_download_ui"),
    list(id = "time5", title = "Screened over time by person", plot = "time5_plot", download = "time5_download_ui"),
    list(id = "time5_event", title = "Screened over time by encounter", plot = "time5_event_plot", download = "time5_event_download_ui")
 )
  
  # screened
  renderSectionPage(
    input, output, 
    page_id = "screened_page",
    sections_info = screened_sections_info,
    n_output_id = "screened_n"
  )  
  
  observe({
    updateSelectInput(session, "keypop5_choice",
                      choices = keypop_choice_list(),
                      selected = input$keypop5_choice)
  })
  
  eligible_sections_info <- list(
    list(id = "top6", title = "Home", plot = NULL, download = NULL),
    list(id = "overall6", title = "Overall", plot = "eligible_overall_plot", download = "eligible_overall_download_ui"),
    list(id = "sex6", title = "Sex", plot = "sex6_plot", download = "sex6_download_ui"),
    list(id = "race6", title = "Race", plot = "race6_plot", download = "race6_download_ui"),
    list(id = "ethnicity6", title = "Ethnicity", plot = "ethnicity6_plot", download = "ethnicity6_download_ui"),
    list(id = "age6", title = "Age", plot = "age6_plot", download = "age6_download_ui"),
    list(id = "insurance6", title = "Insurance status", plot = "insurance6_plot", download = "insurance6_download_ui"),
    list(id = "keypop6", title = "Key populations", plot = "keypop6_plot", download = "keypop6_download_ui"),
    list(id = "time6", title = "Eligible over time by person", plot = "time6_plot", download = "time6_download_ui"),
    list(id = "time6_event", title = "Eligible over time by encounter", plot = "time6_event_plot", download = "time6_event_download_ui"),
    list(id = "reason6", title = "Not eligible reasons", plot = "not_eligible_reason_plot", download = "not_eligible_reason_download_ui")
  )
  
  # eligible
  renderSectionPage(
    input, output, 
    page_id = "eligible_page",
    sections_info = eligible_sections_info,
    n_output_id = "eligible_n"
  )
  
  observe({
    updateSelectInput(session, "keypop6_choice",
                      choices = keypop_choice_list(),
                      selected = input$keypop6_choice)
  })
  
  prescribed_sections_info <- list(
    list(id = "top7", title = "Home", plot = NULL, download = NULL),
    list(id = "overall7", title = "Overall", plot = "prescribed_overall_plot", download = "prescribed_overall_download_ui"),
    list(id = "sex7", title = "Sex", plot = "sex7_plot", download = "sex7_download_ui"),
    list(id = "race7", title = "Race", plot = "race7_plot", download = "race7_download_ui"),
    list(id = "ethnicity7", title = "Ethnicity", plot = "ethnicity7_plot", download = "ethnicity7_download_ui"),
    list(id = "age7", title = "Age", plot = "age7_plot", download = "age7_download_ui"),
    list(id = "insurance7", title = "Insurance status", plot = "insurance7_plot", download = "insurance7_download_ui"),
    list(id = "keypop7", title = "Key populations", plot = "keypop7_plot", download = "keypop7_download_ui"),
    list(id = "time7", title = "Prescribed over time", plot = "time7_plot", download = "time7_download_ui")
 )
  
  # prescribed
  renderSectionPage(
    input, output, 
    page_id = "prescribed_page",
    sections_info = prescribed_sections_info,
    n_output_id = "prescribed_n"
  )
  
  observe({
    updateSelectInput(session, "keypop7_choice",
                      choices = keypop_choice_list(),
                      selected = input$keypop7_choice)
  })
  
  initiated_sections_info <- list(
    list(id = "top8", title = "Home", plot = NULL, download = NULL),
    list(id = "overall8", title = "Overall", plot = "initiated_overall_plot", download = "initiated_overall_download_ui"),
    list(id = "sex8", title = "Sex", plot = "sex8_plot", download = "sex8_download_ui"),
    list(id = "race8", title = "Race", plot = "race8_plot", download = "race8_download_ui"),
    list(id = "ethnicity8", title = "Ethnicity", plot = "ethnicity8_plot", download = "ethnicity8_download_ui"),
    list(id = "age8", title = "Age", plot = "age8_plot", download = "age8_download_ui"),
    list(id = "insurance8", title = "Insurance status", plot = "insurance8_plot", download = "insurance8_download_ui"),
    list(id = "keypop8", title = "Key populations", plot = "keypop8_plot", download = "keypop8_download_ui"),
    list(id = "time8", title = "Initiated over time", plot = "time8_plot", download = "time8_download_ui")
  )
  
  # initiated
  renderSectionPage(
    input, output, 
    page_id = "initiated_page",
    sections_info = initiated_sections_info,
    n_output_id = "initiated_n"
  )  
  
  observe({
    updateSelectInput(session, "keypop8_choice",
                      choices = keypop_choice_list(),
                      selected = input$keypop8_choice)
  })
  
  sustained_sections_info <- list(
    list(id = "top9", title = "Home", plot = NULL, download = NULL),
    list(id = "overall9", title = "Overall", plot = "sustained_overall_plot", download = "sustained_overall_download_ui"),
    list(id = "sex9", title = "Sex", plot = "sex9_plot", download = "sex9_download_ui"),
    list(id = "race9", title = "Race", plot = "race9_plot", download = "race9_download_ui"),
    list(id = "ethnicity9", title = "Ethnicity", plot = "ethnicity9_plot", download = "ethnicity9_download_ui"),
    list(id = "age9", title = "Age", plot = "age9_plot", download = "age9_download_ui"),
    list(id = "insurance9", title = "Insurance status", plot = "insurance9_plot", download = "insurance9_download_ui"),
    list(id = "keypop9", title = "Key populations", plot = "keypop9_plot", download = "keypop9_download_ui"),
    list(id = "time9", title = "Time spent on CAB", plot = "time9_plot", download = "time9_download_ui"),
    list(id = "reason9", title = "Discontinued reasons", plot = "discontinued_reason_plot", download = "discontinued_reason_download_ui")
  )
  
  # sustained
  renderSectionPage(
    input, output, 
    page_id = "sustained_page",
    sections_info = sustained_sections_info,
    n_output_id = "sustained_n"
  )
  
  observe({
    updateSelectInput(session, "keypop9_choice",
                      choices = keypop_choice_list(),
                      selected = input$keypop9_choice)
  })
  
  clinical_sections_info <- list(
    list(id = "top10", title = "Home", plot = NULL, download = NULL),
    list(id = "status_bar", title = "On time injections", plot = "ontime_status_bar", download = "ontime_status_download_ui"),
    list(id = "ontime_1m", title = "On time injections by days since prior injection, monthly injection interval", plot = "ontime_plot_monthly", download = "ontime_monthly_download_ui"),
    list(id = "ontime_2m", title = "On time injections by days since prior injection, bimonthly injection interval", plot = "ontime_plot_bimonthly", download = "ontime_bimonthly_download_ui"),
    list(id = "late_pt", title = "Late injections by patient", plot = "late_pt_plot", download = "late_pt_download_ui"),
    list(id = "early_pt", title = "Early injections by patient", plot = "early_pt_plot", download = "early_pt_download_ui")
  )
  
  renderSectionPage(
    input, output, 
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
      list(id = "time_to_failure2", title = str_c("Time to first virologic failure, pre-CAB VL ",val2()), plot = "time_to_failure2", download = "time_to_failure2_download_ui"),  
      list(id = "clinic_level_vl", title = "Clinic level viral load", plot = "clinic_level_vl", download = "clinic_level_vl_download_ui")
    )
  })
  
  observe({
    renderSectionPage(
      input, output, 
      page_id = "vl_page",
      sections_info = vl_sections_info(),  
      n_output_id = "vl_n"
    )
  })
  
}
