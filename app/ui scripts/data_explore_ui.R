
list(
  fluidRow(
    column(width=12,
           box(id = "de_info_box",
                   title = "Instructions",
                   width = 12,
                   status = "info",
                   solidHeader = TRUE,
                   tagList(
                     p("The data explorer allows you to dive deeper into your data,
                       by showing LAI indicators across two variables at once. 
                       The bar plot and the table below it include the same data."),
                     p("First, select an indicator of interest from the dropdown list."),
                     p("Second, select a comparison variable. This variable will
                       appear at the base of the bar."),
                     p("Third, select a filter variable. This will allow you to
                     select select specific groups of this filter variable that
                       you are interested in focusing on. The filtered groups will
                       appear at the end of a group of bars."),
                     p("For example, suppose you are interested in comparing the
                       percentage educated across race categories for PWH aged 
                       25-34. You would (1) select “Educated” as the indicator; 
                       (2) select 'Race’ as the comparison variable; and (3) 
                       'Age' as the filter variable, and check off only age 
                       group 25-34. The resulting plot would show you the number
                       and percent educated by race for those aged 25-34, 
                       allowing for simple comparison between the groups."),
                     p("If demographics are selected, then the bars will show the 
                       distribution of the comparison variable among the group selected 
                       via the filter variable. For example, if 'Age' is the comparison 
                       variable and 'Sex' is the filter variable, you will see the 
                       age distribution by sex."),
                     p("If you are interested in focusing on an active year of 
                       patients, or filtering the dates of the events being 
                       included in the plot, click the 'Filter time period' button
                       to expand those options.")
                   )
               )
           )
  ),
  fluidRow(
    box(title = "Crosstab data",
        width = 12,
        status = "primary", solidHeader = T,
        plotOutput("data_explore_plot"),
        downloadButton(outputId = "data_explore_plot_download", label = "Download plot"),
        downloadButton(outputId = "data_explore_table_download",  
                       label = "Download table",
                       icon = icon("table")),
        size = "xs"
  )
)
)