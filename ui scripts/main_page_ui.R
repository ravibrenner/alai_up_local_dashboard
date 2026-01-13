
list(
  fluidRow(
    column(width=12,
           h3(uiOutput("description"),style="font-size: 26px;")
    )
  ),
  
  fluidRow(
    box(title = "Demographics",
        width = 12,
        status = "primary", solidHeader = T,
        plotOutput("demographics_plot"),
        downloadButton(outputId = "demographics_plot_download", label = "Download plot"),
        size = "xs",
        icon = icon("download", class = "opt"))
  ),
  
  fluidRow(
    box(title = "LAI Gap analysis",
        width = 12,
        status = "primary", solidHeader = T)
  ),
  
  fluidRow(
    box(title = "Assessed",
        width = 12,
        status = "primary", solidHeader = T)
  ),
  
  fluidRow(
    box(title = "Eligible",
        width = 12,
        status = "primary", solidHeader = T)
  ),
  
  fluidRow(
    box(title = "Prescribed",
        width = 12,
        status = "primary", solidHeader = T)
  ),
  
  fluidRow(
    box(title = "Initiated",
        width = 12,
        status = "primary", solidHeader = T)
  ),
  
  fluidRow(
    box(title = "Sustained",
        width = 12,
        status = "primary", solidHeader = T)
  )
)