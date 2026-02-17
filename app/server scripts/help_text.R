help_text <- list(
  info1 = tagList(
    p("This page shows overall clinic demographics and patient characteristics, 
      for all people with HIV served at your clinic, regardless of LAI use."),
    p("Use the Jump to Section navigator on the near left to jump to different
      sections of this page.")
  ),
  info1b = tagList(
    p("This page shows overall clinic demographics and patient characteristics, 
      for all people with HIV served at your clinic, stratified by LAI use."),
    p("Use the Jump to Section navigator on the near left to jump to different
      sections of this page.")
  ),
  
  info0 = tagList(
    p("This page shows an overview of the LAI care gap analysis. There are also 
      details about the documented education and screening outcomes for those assessed."),
    tags$ul(   
      tags$li('"Assessed" is a combined indicator representing the number of 
              PWH with documented education or screening for iCAB/RPV.'),
      tags$li('"Eligible" indicates the number of PWH who were assessed 
              to be eligible for iCAB/RPV based on site specific eligibility
              criteria.'),
      tags$li('"Prescribed" indicates the number of PWH who were prescribed 
              iCAB/RPV among those eligible.'),
      tags$li('"Initiated" indicates the number of PWH who received at least 
              one injection of iCAB/RPV among those prescribed.'),
      tags$li('"Sustained" indicates the number of PWH who were currently 
              sustained on iCAB/RPV at the site among those initiated.')
    ),
    p('The "Outcomes among those assessed" table shows the distribution of education 
      and screening outcomes among those assessed. The "Percent" buttons allow you to
      change how the percentages are calculated: "table" for using the number assessed
      as the denominator, "row" for using the row total, and "column" for using the
      column total. For example to see the percentage eligible among those interested, 
      using row percent is best. The highlighted cell shows the number interested and
      eligible, as in the LAI care gap analysis.'),
    p('Use the Jump to Section navigator on the near left to jump to different 
      sections of this page.'),
    p("To view details about each LAI care indicator, click the corresponding tab
      on the far left.")
  ),
  
  info2 = tagList(
    p('This page shows the number and percentage of people with HIV at the site
      in the current year who were assessed. "Assessed" is a combined indicator
      representing the number of PWH with documented education or screening for iCAB/RPV'),
    p("To view details about the number of patients educated, screened, and 
      interested in iCAB/RPV, you can navigate to those pages by clicking on 
      the corresponding tabs on the left."),
    p('Light grey bars show indicators with fewer than 10 patients and therefore
      should be interpreted with caution, given the sample size. Dotted lines
      show the average for that indicator across all clinic patients.')
  ),
  
  info3 = tagList(
    p("This page shows the number and percentage of people with HIV at the site
      in the current year who were educated about iCAB/RPV."),
    p("To view details about the number of interested in iCAB/RPV following 
      education, you can navigate to the corresponding tab on the left.")
  ),
  
  info4 = tagList(
    p("This page shows the number and percentage of people with HIV at the site
      in the current year interested in iCAB/RPV among those educated. There is
      also a plot showing reasons for not being interested in iCAB/RPV.")
  ),
  
  info5 = tagList(
    p("This page shows the number and percentage of people with HIV at the site
      in the current year screened for eligibility for iCAB/RPV.")
  ),
  
  info6 = tagList(
    p("This page shows the number and percentage of people with HIV at the site 
      in the current year who were assessed to be eligible for iCAB/RPV based 
      on site specific eligibility criteria. There is also a plot showing reasons
      for not being eligible for iCAB/RPV.")
  ),
  
  info7 = tagList(
    p("This page shows the number and percentage of people with HIV at the site
      in the current year who were prescribed iCAB/RPV among those eligible.")
  ),
  
  info8 = tagList(
    p("This page shows the number and percentage of people with HIV at the site
      in the current year who received at least one injection of iCAB/RPV among those prescribed.")
  ),
  
  info9 = tagList(
    p("This page shows the number and percentage of people with HIV at the site 
      in the current year currently sustained on iCAB/RPV at the site among those initiated. 
      There is also a plot showing reasons for discontinuation.")
  ),
  
  info10 = tagList(
    p('This page shows the numbers and percentages of early, on time, and late 
      injections out of all follow-up injections after the first injections. "Early" 
      is defined as occurring more than seven days before the target injection 
      date.  "Late" is defined as occurring more than seven days after the target
      injection date. The target injection date can be toggled on the left 
      depending on the site’s clinical practice. Either 28 or 56 days (4 or 8 weeks),
      or 31 or 62 days (1 or 2 months) may be used to calculate the target interval.'),
    p('"On time injections" shows the overall percentage of injections administered early, on time, and late.'),
    p('"On time injections by time" shows how early or late particular injections were, 
      with the on time injection region shaded in blue.'),
    p('"Late injections by patient" shows how many patients had how many late injections.'),
    p('"Early injections by patient" shows how many patients had how many early injections.')
  ),
  
  info11 = tagList(
    p('This page shows viral load results among patients on iCAB/RPV. Different 
      viral load cutoffs of interest can be selected on the left. Either 50 copies/mL 
      or 200 copies/mL can be used to calculate viral suppression. Viral failure 
      is defined as a signle viral load >1000 copies/mL or 2 consecutive viral loads
      >200 copies/mL.'),
    p('All plots are calculated using the Kaplan-Meier estimator. This gives a statistical
      estimate of the time to the first occurence of the event. Therefore, the percentages
      are only statistical estimates and have some uncertainty attached to them. The
      number of people contributing data (in parentheses) can give a sense of how certain
      or undercertain the estimate is.'),
    p('For the first plot, ideally the time to viral suppression will be short and reach 
      near 100%. For the subsequent plots, ideally time to first elevated viral load 
      or first viral failure will be long, and the percentage will remain low.')
  )
  
)