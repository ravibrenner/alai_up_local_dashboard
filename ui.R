library(shiny)
library(shinydashboard)
library(shinyjs)
library(bslib)
library(dplyr)
library(forcats)
library(ggplot2)
library(hms)
library(lubridate)
library(magrittr)
library(purrr)
library(rlang)
library(stringr)
library(tibble)
library(tidyr)
library(readxl)
library(patchwork)
library(showtext)
library(ggpubr)
library(gt)
library(ggsurvfit)
library(leaflet)
library(munsell)

ui <- dashboardPage(
  dashboardHeader(
    # title
    title = 'ALAI UP Dashboard',
    tags$li(
      class = "dropdown",
      style = "padding: 8px;",
      shinyauthr::logoutUI("logout")
    )
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(id='sidebar',
                menuItem(strong("Home"), tabName= 'home'),
                radioButtons("assessed_choice",
                             label = "Is Counseling/Screening consistently recorded? (This button currently does nothing)",
                             choices = c("Yes","No"),
                             selected = "Yes"),
                menuItem(text = strong("Clinic Demographics"),
                         tabName = 'demographics_header',
                         menuItem(text = "Overall",
                                  tabName = "demographics_page"),
                         menuItem(text = "By LAI use",
                                  tabName = "demo_by_lai")),
                menuItem(strong("LAI indicators"),tabName = 'page1',
                         menuItem(text = strong("Overview"),
                                  tabName = 'lai_overview'),
                         menuItem(text = strong("Assessed"),
                                  tabName = 'assessed_page'),
                         menuSubItem(text = HTML("Educated"),
                                     tabName = "educated_page"),
                         menuSubItem(text = HTML("&emsp;Interested"),
                                     tabName = "interested_page"),
                         menuSubItem(text = HTML("Screened"),
                                     tabName = "screened_page"),
                         menuSubItem(text = HTML("&emsp;Eligible"),
                                     tabName = 'eligible_page'),
                         menuItem(text = strong("Prescribed"),
                                  tabName = 'prescribed_page'),
                         menuItem(text = strong("Initiated"),
                                  tabName = 'initiated_page'),
                         menuItem(text = strong("Sustained"),
                                  tabName = 'sustained_page')),
                menuItem(text = strong("Clinical outcomes"),
                         tabname = "clinical_page",
                         menuItem(text = "On time injections",
                                  tabName = 'inj_page'),
                         conditionalPanel(id = "ontime_target",
                                          "input.sidebar === 'inj_page'",
                                          radioButtons("ontime_target_input",
                                                       label = "Choose on time target days",
                                                       choices = c("1 or 2 months","4 or 8 weeks"),
                                                       selected = "1 or 2 months")),
                         menuItem(text = "Viral load",
                                  tabName = "vl_page"),
                         conditionalPanel(id = "vl_cutoff",
                                          "input.sidebar === 'vl_page'",
                                          radioButtons("vl_cutoff_input",
                                                       label = "Choose VL cutoff",
                                                       choices = c("50 copies/mL","200 copies/mL"),
                                                       selected = "50 copies/mL"))),
                menuItem(strong("Data Explorer"),tabName = 'page2'),
                div(id = "explorer_filters",
                    conditionalPanel(
                      "input.sidebar === 'page2'",
                      uiOutput("indicator"),
                      uiOutput("grouping_var"),
                      selectInput("filter_var",
                                  "Filter variable",
                                  choices = NULL),
                      uiOutput("filter_select")),
                    uiOutput('filter_by_year'),
                    conditionalPanel("input.filter_by_year === true",
                                     uiOutput('active_year_choice'),
                                     uiOutput('date_filter_ui')),
                )
                
    )
  ),
  dashboardBody(
    useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
    tabItems(
      tabItem(
        tabName = 'home',
        fluidRow(
          box(
            id = "input_box",
            title = "Upload data",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            fileInput("file1", "Choose Data File (.xlsx)", accept = c(".xlsx",".xls")),
            selectInput("select_sheet","Select a sheet",choices = NULL),
            textInput("site_name_input","Site name", value = NULL),
            disabled(actionButton("go_button",
                                  "Upload data",
                                  icon = icon("play")))
            )),
        uiOutput('home_page')
      ),
      tabItem(
        tabName = 'demographics_page',
        uiOutput('demographics_page')
      ),
      tabItem(
        tabName = 'demo_by_lai',
        uiOutput('demo_by_lai')
      ),
      tabItem(
        tabName = 'lai_overview',
        uiOutput('lai_overview')
      ),
      tabItem(
        tabName = 'assessed_page',
        uiOutput('assessed_page')
      ),
      tabItem(
        tabName = 'educated_page',
        uiOutput('educated_page')
      ),
      tabItem(
        tabName = 'interested_page',
        uiOutput('interested_page')
      ),
      tabItem(
        tabName = 'screened_page',
        uiOutput('screened_page')
      ),
      tabItem(
        tabName = 'eligible_page',
        uiOutput('eligible_page')
      ),
      tabItem(
        tabName = 'prescribed_page',
        uiOutput('prescribed_page')
      ),
      tabItem(
        tabName = 'initiated_page',
        uiOutput('initiated_page')
      ),
      tabItem(
        tabName = 'sustained_page',
        uiOutput('sustained_page')
      ),
      tabItem(
        tabName = 'inj_page',
        uiOutput('inj_page')
      ),
      tabItem(
        tabName = 'vl_page',
        uiOutput('vl_page')
      ),
      tabItem(
        tabName = 'page2',
        uiOutput('data_explore_page')
      )
    )
  )
)   

