list(
  
  h1("Welcome"),
  #imageOutput("sites_img"),
  p('Welcome to the ALAI UP Dashboard'),
  p('The purpose of the Dashboard is to:'),
  tags$ul(
    tags$li('Help clinics clearly see and understand their HIV treatment data,
            with a focus on long-acting injectable (LAI) treatment.'),
    tags$li('Track progress toward program goals, particularly implementation
            goals for long-acting injectable (LAI) HIV treatment.'),
    tags$li('Identify gaps in care, especially disparities among different 
            demographic groups.'),
    tags$li('Guide efforts to address gaps and improve the quality of services 
            for all patients.')
  ),
  h3('Instructions'),
  p(strong('The Dashboard is organized into the following sections designed to support
    at-a-glance data review, and more in-depth data exploration.  Select a 
    clinical site to review, then navigate to one of the sections below. To
    view data on all ALAI UP sites, select “All ALAI UP Sites”')),
  tags$ol(   
    tags$li('Clinic Demographics. This page shows clinic demographic data for 
            patients overall and by patient characteristics, regardless of LAI use. 
            To view demographic data for the selected site , click on 
            "Clinic Demographics" in the menu on the left of the page.'),
    br(),
    tags$li('LAI Indicators. This section shows the LAI care gaps throughout 
            the care continuum: number and percent assessed, eligible, prescribed,
            initiated, sustained. This section also shows LAI outcomes of the 
            clinical process: number and percent of PWH on LAI who received LAI 
            early, on time, or late, and number of injections administered early,
            on time or late.  To view the LAI care indicator data for the selected
            site, click on "LAI Indicators" on the left of the page. Once you 
            click on LAI indicators in the menu, a new menu will open with a 
            column on the left showing all the indicators you can look at for
            your site.'),
    br(),
    tags$li('Data Explore. This page allows you to dive deeper into your data,
            by viewing LAI indicators disaggregated by up to two variables at 
            once. If you want to view more detailed data subset by two variables
            at once, click on the "Data Explorer" tab. There are detailed 
            instructions there for using the data explorer.')
  ),
  br(),
  br(),
  br(),
  p('The development of the ALAI UP (Accelerating Implementation of 
    Multilevel-strategies to Advance Long-Acting Injectables for Underserved 
    Populations) dashboard was financially supported the Health Resources 
    and Services Administration (HRSA), Department of Health and Human Services 
    (HHS) U1SHA46532. The award provided 100% 
    of total costs and totaled $7,450,000. The contents are those of the developers. 
    They may not reflect the policies of HRSA, HHS, or the U.S. Government.'),
  p('This beta Version 0.3 was completed on 12/16/2025. The Dashboard includes
    data from consenting ALAI UP sites through 8/31/2025 (except for SAAF as of 12/16/25).
    Users can see data 
    aggregated across all ALAI UP sites and aggregate data from their own site.')
  
)
  
  