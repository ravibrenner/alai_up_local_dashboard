renderSectionPage <- function(input, output, page_id, sections_info, n_output_id) {
  # get page number
  page_num <- str_extract(sections_info[[1]]$id,"\\d+")

  #get help text id string
  help_text_id = paste0("info",page_num)
  
  # Create TOC links
  toc_links <- map(sections_info, function(section) {
    actionLink(inputId = paste0("go_", section$id), label = section$title)
  })
  
  # "Top"/Home section
  top_section <- div(id = sections_info[[1]]$id,
                     h3(uiOutput(n_output_id), style = "font-size: 26px;"))
  
  text_section <- box(id = str_c(help_text_id,"_box"),
                      title = "Instructions",
                      width = 12,
                      status = "info",
                      solidHeader = TRUE,
                      help_text[help_text_id])
  
  # All remaining sections with plots
  plot_sections <- map(sections_info[-1], function(section) {
    box(
      id = paste0(section$id, "_box"),
      title = section$title,
      width = 12,
      status = "primary",
      solidHeader = TRUE,
      {if (str_detect(section$id,"keypop")){
        selectInput(
              inputId = paste0(section$id,"_choice"),
              label = "Key population choice", 
              choices = c("Housing status","Gender",
                          "Risk MSM","Risk IDU","Risk Heterosex",
                          "Employment status","Poverty level",
                          "Immigration status","Language",
                          "Incarceration history","Recent CD4"),
              selected = "Housing status")
      } else if (str_detect(section$id,"clinic_level_vl")){
        fluidRow(
          column(width = 3,
                 selectInput(
                   inputId = paste0(section$id,"_time_choice"),
                   label = "Time period length, months", 
                   choices = c(3,6,12),
                   selected = 6)),
          column(width = 3,
                 radioButtons(
                   inputId = paste0(section$id,"_pct_choice"),
                   label = "Percent or Count?", 
                   choices = c("Percent","Count"),
                   selected = "Percent")))
      } else{NULL}},
      plotOutput(section$plot, height = "auto"),
      uiOutput(section$download),
      size = "xs"
    )
  })
  
  # Set the UI render output
  output[[page_id]] <- renderUI({
    req(input$go_button)
    fluidPage(
      fluidRow(
        column(
          width = 2,
          div(class = "toc-container",
              h4("Jump to Section"),
              div(class = "toc-links", toc_links)
          )
        ),
        column(width = 10, top_section,text_section, plot_sections)
      )
    )
  })
  
  # Create the smooth scroll observers
  map(sections_info, function(section) {
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
}