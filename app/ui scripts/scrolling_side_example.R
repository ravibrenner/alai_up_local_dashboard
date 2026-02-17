library(shiny)
library(shinydashboard)
library(shinyjs)

ui <- dashboardPage(
  dashboardHeader(title = "Sticky ToC with shinyjs"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Document", tabName = "doc", icon = icon("file"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    # CSS styling
    tags$head(
      tags$style(HTML("
        html {
          scroll-behavior: smooth;
        }
        .toc-container {
          position: fixed;
          top: 70px; 
          width: 20%;
          background-color: #f7f7f7;
          border: 1px solid #ccc;
          padding: 15px;
          border-radius: 4px;
          box-shadow: 0 0 5px rgba(0,0,0,0.1);
          margin-bottom: 20px;
        }
        .toc-links {
          font-size: 14px;
        }
        .toc-links .action-button {
          display: block;
          margin-bottom: 10px;
          text-align: left;
          border: none;
          background: none;
          padding: 0;
          color: #007bff;
          cursor: pointer;
        }
        .content-area {
          padding: 10px 30px;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "home",
              h2("Welcome"),
              p("This is the home tab.")
      ),
      
      tabItem(tabName = "doc",
              fluidRow(
                # Sticky ToC sidebar
                column(
                  width = 3,
                  div(class = "toc-container",
                      h4("Jump to Section"),
                      div(class = "toc-links",
                          actionLink("go_intro", "Introduction"),
                          actionLink("go_methods", "Methods"),
                          actionLink("go_results", "Results"),
                          actionLink("go_conclusion", "Conclusion")
                      )
                  )
                ),
                
                # Content
                column(
                  width = 9,
                  div(class = "content-area",
                      div(id = "section_intro",
                          h2("Introduction"),
                          p(paste(rep("Intro content. ", 500), collapse = " "))
                      ),
                      div(id = "section_methods",
                          h2("Methods"),
                          p(paste(rep("Methods content. ", 500), collapse = " "))
                      ),
                      div(id = "section_results",
                          h2("Results"),
                          p(paste(rep("Results content. ", 500), collapse = " "))
                      ),
                      div(id = "section_conclusion",
                          h2("Conclusion"),
                          p(paste(rep("Conclusion content. ", 500), collapse = " "))
                      )
                  )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$go_intro, {
    runjs("document.getElementById('section_intro').scrollIntoView({ behavior: 'smooth' });")
  })
  observeEvent(input$go_methods, {
    runjs("document.getElementById('section_methods').scrollIntoView({ behavior: 'smooth' });")
  })
  observeEvent(input$go_results, {
    runjs("document.getElementById('section_results').scrollIntoView({ behavior: 'smooth' });")
  })
  observeEvent(input$go_conclusion, {
    runjs("document.getElementById('section_conclusion').scrollIntoView({ behavior: 'smooth' });")
  })
}

shinyApp(ui, server)