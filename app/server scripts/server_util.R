download_box <- function(exportname, plot, height_scale = 5) {
  downloadHandler(
    filename = function() {
      paste(exportname, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot, device = "png", 
             bg = "white",
             width = 1000,height=100*height_scale,
             units = "px",
             dpi = 72)
    }
  )
}

download_table <- function(exportname, plot_data){
  downloadHandler(
    filename = function() {
      paste(exportname, "_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(plot_data, file)
    }
  )
}


# function for loading and processing initial data
load_and_process_data <- function(input_df) {
  
  df <- input_df |>
    as_tibble() |>
    select(-any_of(c("date","index","event"))) |> 
    filter(age >= 18) |>
    # mutate(
    #   hiv_dx_date = case_when(str_length(hiv_dx_date)==4 ~ as.Date(paste0(hiv_dx_date,"-01-01"),format = "%Y-%m-%d"),
    #                           str_detect(hiv_dx_date,"/") ~ as.Date(paste0(str_sub(hiv_dx_date,1,4),"-01-01"),format = "%Y-%m-%d"),
    #                           .default = as.Date(as.numeric(hiv_dx_date),origin = "1899-12-30"))) |>
    mutate(across(contains("date"),
                  \(x) case_when(is.POSIXct(x) ~ as.Date(x,format = "%Y-%m-%d"),
                                 is.Date(x) ~ as.Date(x,format = "%Y-%m-%d"),
                                 str_detect(x,"-") ~ as.Date(x,format = "%Y-%m-%d"),
                                 str_detect(x,"/") ~ as.Date(x,format = "%m/%d/%Y"),
                                 .default = as.Date(as.numeric(x),origin = "1899-12-30")))) |>
    # temporary fix for some extreme date values
    mutate(race=factor(
      case_when(
        race_ai_an==1 ~ "American Indian or Alaskan Native",
        race_asian==1 ~ "Asian",
        race_black==1 ~ "Black",
        race_nh_pi==1 ~ "Native Hawaiian or Pacific Islander",
        race_white==1 ~ "White",
        race_other==1 ~ "Other" ,
        .default = "Unknown"
      ),
      levels = c("American Indian or Alaskan Native","Asian",
                 "Black","Native Hawaiian or Pacific Islander",
                 "White","Other","Unknown"))) |>
    mutate(age_cat=factor(
      case_when( 
        age>=18 & age<=24 ~ "18-24",
        age>=25 & age<=34 ~ "25-34",
        age>=35 & age<=44 ~ "35-44",
        age>=45 & age<=54 ~ "45-54",
        age>=55 & age<=64 ~ "55-64",
        age>=65 ~ "65+",
        .default = "Unknown",
      ),
      levels = c("18-24","25-34","35-44","45-54","55-64","65+","Unknown"))) |>
    mutate(
      sex_birth = factor(
        case_when(sex_birth == 1 ~ "Male",
                  sex_birth == 2 ~ "Female",
                  .default = "Unknown"),
        levels = c("Male","Female","Unknown"))) |>
    mutate(ethnicity=factor(
      case_when(
        ethnicity_hispanic==1 ~ "Hispanic",
        ethnicity_hispanic==0 ~ "Not hispanic",
        .default = "Unknown"
      ),
      levels = c("Hispanic","Not hispanic","Unknown"))) |>
    mutate(insurance_status = factor(
      case_when(
        insurance_status == 1 ~ "Private Employer",
        insurance_status == 2 ~ "Private Individual",
        insurance_status == 3 ~ "Medicare",
        insurance_status == 4 ~ "Medicaid",
        insurance_status == 5 ~ "Veterans Health Administration",
        insurance_status == 6 ~ "Indian Health Service",
        insurance_status == 7 ~ "Other",
        insurance_status == 8 ~ "Uninsured",
        .default = "Unknown"),
      levels = c("Private Employer","Private Individual","Medicare",
                 "Medicaid","Veterans Health Administration",
                 "Indian Health Service", "Other","Uninsured","Unknown")
    )) |>
    mutate(housing_status = factor(case_when(
      housing_status == 1 ~ "Stable or permanent",
      housing_status == 2 ~ "Temporary",
      housing_status == 3 ~ "Unstable",
      .default = "Unknown"
    ),
    levels = c("Stable or permanent","Temporary",
               "Unstable","Unknown")),
    gender_id = factor(case_when(
      gender_id == 1 ~ "Cisgender Man",
      gender_id == 2 ~ "Cisgender Woman",
      gender_id == 3 ~ "Transgender Woman",
      gender_id == 4 ~ "Transgender Man",
      gender_id == 5 ~ "Nonbinary",
      is.na(gender_id) ~ "Unknown",
      .default = "Other"
    ), levels = c("Cisgender Man","Cisgender Woman",
                  "Transgender Woman","Transgender Man",
                  "Nonbinary","Other","Unknown")),
    risk_msm = factor(case_when(
      risk_msm == 0 ~ "Not MSM",
      risk_msm == 1 ~ "MSM",
      .default = "Unknown"
    ), levels = c("Not MSM","MSM","Unknown")),
    risk_idu = factor(case_when(
      risk_idu == 0 ~ "No IDU",
      risk_idu == 1 ~ "IDU",
      .default = "Unknown"
    ), levels = c("No IDU","IDU","Unknown")),
    risk_heterosex = factor(case_when(
      risk_heterosex == 0 ~ "No heterosexual contact",
      risk_heterosex == 1 ~ "Heterosexual contact",
      .default = "Unknown"
    ), levels = c("No heterosexual contact","Heterosexual contact","Unknown")),
    employment_status = factor(case_when(
      employment_status == 1 ~ "Employed full time",
      employment_status == 2 ~ "Employed part time",
      employment_status == 3 ~ "Unemployed",
      .default = "Unknown"
    ), levels = c("Employed full time","Employed part time",
                  "Unemployed","Unknown")),
    poverty_level = factor(case_when(
      poverty_level == 1 ~ "<100% of the FPL",
      poverty_level == 2 ~ "101%-200% of the FPL",
      poverty_level == 3 ~ "201-300% of the FPL",
      poverty_level == 4 ~ "301% or more of the FPL",
      .default = "Unknown"
    ), levels = c("<100% of the FPL","101%-200% of the FPL",
                  "201-300% of the FPL","301% or more of the FPL","Unknown")),
    immigration_status_undoc = factor(case_when(
      immigration_status_undoc == 0 ~ "US citizen or documented",
      immigration_status_undoc == 1 ~ "Undocumented",
      .default = "Unknown"
    ), levels = c("US citizen or documented","Undocumented","Unknown")),
    language = factor(case_when(
      language == 1 ~ "English",
      language == 2 ~ "Spanish",
      language == 3 ~ "Chinese",
      language == 4 ~ "Haitian Creole",
      language == 5 ~ "Tagalog",
      language == 6 ~ "Vietnamese",
      language == 7 ~ "Arabic",
      language == 8 ~ "Amharic",
      language == 20 ~ "Other",
      .default = "Unknown"
    ), levels = c("English","Spanish",
                  "Chinese","Haitian Creole",
                  "Tagalog","Vietnamese",
                  "Arabic","Amharic",
                  "Other","Unknown")),
    incarceration_history = factor(case_when(
      incarceration_history == 0 ~ "No history of incarcerations",
      incarceration_history == 1 ~ "Current incarceration",
      incarceration_history == 2 ~ "History of incarceration",
      incarceration_history == 3 ~ "Community supervision (e.g. probation)",
      .default = "Unknown"
    ), levels = c("No history of incarcerations","Current incarceration",
                  "History of incarceration","Community supervision (e.g. probation)",
                  "Unknown"))) |>
    mutate(ever_on_cab = if_else(!is.na(icab_rpv_shot1_date) | 
                                 !is.na(icab_rpv_shot2_date),1,0))
  
  disc_dates <- df |>
    filter(icab_rpv_discontinued == 1 & is.na(icab_rpv_discontinued_date) &
             (!is.na(icab_rpv_shot1_date) | !is.na(icab_rpv_shot2_date))) |> 
    select(alai_up_uid, contains("icab_rpv")) |>
    pivot_longer(cols = contains("icab_rpv_shot")&contains("date"),
                 names_to = "index",
                 values_to = "date") |> 
    filter(.by = alai_up_uid,
           date == last(date, na_rm = T)) |>
    mutate(icab_rpv_discontinued_date = date) |>
    select(alai_up_uid, icab_rpv_discontinued_date)
  
  df <- df |>
    full_join(disc_dates, by = "alai_up_uid") |>
    mutate(icab_rpv_discontinued_date = coalesce(icab_rpv_discontinued_date.x, icab_rpv_discontinued_date.y)) |>
    select(-icab_rpv_discontinued_date.x, -icab_rpv_discontinued_date.y)
  
  disc_dates2 <- df |>
    filter(is.na(icab_rpv_discontinued) &
             (!is.na(icab_rpv_shot1_date) | !is.na(icab_rpv_shot2_date))) |> 
    select(alai_up_uid, contains("icab_rpv")) |>
    pivot_longer(cols = contains("icab_rpv_shot")&contains("date"),
                 names_to = "index",
                 values_to = "date") |> 
    filter(.by = alai_up_uid,
           date == last(date, na_rm = T)) |>
    mutate(icab_rpv_discontinued = case_when(
      # TODO this date is hard coded but we need a more flexible solution
      # Either an input or an autodetected date could work
      date + days(90) < as.Date("2025-08-31") ~ "1",
      .default = "0"
    ),
    icab_rpv_discontinued_date = case_when(
      icab_rpv_discontinued == "1" ~ date,
      .default = icab_rpv_discontinued_date
    ),
    icab_rpv_discontinued_reason = case_when(
      icab_rpv_discontinued == "1" ~ "No dose in >90 days",
      .default = icab_rpv_discontinued_reason
    )) |>
    select(alai_up_uid, icab_rpv_discontinued,icab_rpv_discontinued_date,
           icab_rpv_discontinued_reason)
  
  df <- df |>
    full_join(disc_dates2, by = "alai_up_uid") |>
    mutate(icab_rpv_discontinued_date = coalesce(icab_rpv_discontinued_date.x, icab_rpv_discontinued_date.y),
           icab_rpv_discontinued = coalesce(icab_rpv_discontinued.x,
                                            icab_rpv_discontinued.y),
           icab_rpv_discontinued_reason = coalesce(icab_rpv_discontinued_reason.x,
                                                   icab_rpv_discontinued_reason.y)) |>
    select(-icab_rpv_discontinued_date.x, -icab_rpv_discontinued_date.y,
           -icab_rpv_discontinued.x,-icab_rpv_discontinued.y,
           -icab_rpv_discontinued_reason.x,-icab_rpv_discontinued_reason.y) |>
    mutate(icab_rpv_discontinued_date = if_else(icab_rpv_discontinued == 0,NA,
                                                icab_rpv_discontinued_date))
  
  return(df)
}

filter_data_by_site <- function(input_df, dataset_name = NULL){
  if (dataset_name == "All ALAI UP Sites"){
    temp <- input_df 
  } else {
    temp <- input_df |>
      filter(site == dataset_name) 
  }
  
  return(temp)
}

  
get_current_year_data <- function(input_df, input_year,
                                  filter_dates = FALSE,
                                  start_date = NULL,
                                  end_date = NULL) {
  #current year counts
  tbl <- input_df |>  
    filter(if_any(contains("active")&contains(as.character(input_year)),
                  \(x) x == 1))
  
  if (filter_dates == TRUE){
    tbl <- time_period_filter(tbl, start_date, end_date)
  } 
  
  return(tbl)
  
}

time_period_filter <- function(input_df, start_date, end_date){
  input_df |>
    select(-any_of(c("date","index","event"))) |> 
    # educated, interested
    pivot_longer(cols = (contains("icab_rpv_counsel")) &
                   !contains("ever"),
                 names_to = c("event","index", ".value"),
                 names_pattern = "(.+)_(\\d+)_(date|outcome)") |>
    mutate(outcome = if_else(date >= start_date & date <= end_date, outcome, NA),
           date = if_else(date >= start_date & date <= end_date, date, NA)) |>
    mutate(.by = c(alai_up_uid),
           icab_rpv_counsel_ever = if_else(any(!is.na(date)),1,0)) |>
    pivot_wider(names_from = c(event,index),
                values_from = c(date,outcome),
                names_glue = "{event}_{index}_{.value}") |> 
    # screened, eligible
    pivot_longer(cols = (contains("icab_rpv_screen")) &
                   !contains("ever"),
                 names_to = c("event","index", ".value"),
                 names_pattern = "(.+)_(\\d+)_(date|outcome)") |>
    mutate(outcome = if_else(date >= start_date & date <= end_date, outcome, NA),
           date = if_else(date >= start_date & date <= end_date, date, NA)) |>
    mutate(.by = c(alai_up_uid),
           icab_rpv_screen_ever = if_else(any(!is.na(date)),1,0)) |>
    pivot_wider(names_from = c(event,index),
                values_from = c(date, outcome),
                names_glue = "{event}_{index}_{.value}") |>
    # prescribed
    mutate(icab_rpv_rx = if_else(icab_rpv_rx_date >= start_date & icab_rpv_rx_date <= end_date, 
                                 icab_rpv_rx, NA),
           icab_rpv_rx_date = if_else(icab_rpv_rx_date >= start_date & icab_rpv_rx_date <= end_date,
                                      icab_rpv_rx_date, NA)) |>
    # initiated
    pivot_longer(cols = contains("icab_rpv_shot")&
                   !contains("bmi")&!contains("needle_length")&!contains("loading"),
                 names_to = c("shot_num",".value"),
                 names_pattern = "(.+)_(date|dose)") |>
    mutate(dose = if_else(date >= start_date & date <= end_date,dose,NA),
           date = if_else(date >= start_date & date <= end_date,date,NA)) |>
    pivot_wider(names_from = shot_num,
                values_from = c(date,dose),
                names_glue = "{shot_num}_{.value}") |>
    # sustained
    mutate(icab_rpv_discontinued = if_else(icab_rpv_discontinued_date >= start_date & 
                                             icab_rpv_discontinued_date <= end_date, 
                                           icab_rpv_discontinued, NA),
           icab_rpv_discontinued_reason = if_else(icab_rpv_discontinued_date >= start_date & 
                                                    icab_rpv_discontinued_date <= end_date, 
                                                  icab_rpv_discontinued_reason, NA),
           icab_rpv_discontinued_date = if_else(icab_rpv_discontinued_date >= start_date & 
                                                  icab_rpv_discontinued_date <= end_date, 
                                                icab_rpv_discontinued_date, NA))
  
}


# function for a single demographics plot, based on imput column
demo_plot <- function(input_df, in_col, base_size_in,
                      selected_site = NULL,
                      selected_year = NULL,
                      by_cab_status = FALSE,
                      input = input){
  in_col_string = in_col
  in_col = sym(in_col)

  title = case_when(in_col_string == "age_cat" ~ "Age",
                    in_col_string == "race" ~ "Race",
                    in_col_string == "ethnicity" ~ "Ethnicity",
                    in_col_string == "sex_birth" ~ "Sex",
                    in_col_string == "insurance_status" ~ "Insurance status",
                    in_col_string == "housing_status" ~ "Housing status",
                    in_col_string == "gender_id" ~ "Gender",
                    in_col_string == "risk_msm" ~ "Risk MSM",
                    in_col_string == "risk_idu" ~ "Risk IDU",
                    in_col_string == "risk_heterosex" ~ "Risk Heterosex",
                    in_col_string == "employment_status" ~ "Employment status",
                    in_col_string == "poverty_level" ~ "Poverty level",
                    in_col_string == "immigration_status_undoc" ~ "Immigration status",
                    in_col_string == "language" ~ "Language",
                    in_col_string == "incarceration_history" ~ "Incarceration history",
                    in_col_string == "site" ~ "Site")
  
  if (is.null(selected_year)){
    title_text = str_c("PWH at ", selected_site,
                       " by ", title)
  } else {
    title_text = str_c("PWH at ", selected_site,
                       " active in ",selected_year,
                       " by ", title)
  }
  
  if (by_cab_status == FALSE){
    temp <- input_df |>
      summarize(.by = c({{in_col}}),
                n = n()) |>
      complete({{in_col}}) |>
      filter({{in_col}}!="Unknown" | ({{in_col}}=="Unknown" & n > 0)) |>
      replace_na(list(n = 0)) |>
      mutate(pct = n/sum(n,na.rm = T),
             bar_text = str_c(round(100*pct,0),"%"),
             axis_text = str_c({{in_col}}," (",n," out of ",sum(n)," PWH)"),
             axis_text = fct_rev(axis_text)) |>
      filter(.by = {{in_col}}, sum(n) > 0)
  } else if (by_cab_status == TRUE){
    temp <-  input_df |>
      mutate(ever_on_cab = if_else(ever_on_cab == 0,"Never on LAI ART","Ever on LAI ART"),
             ever_on_cab = factor(ever_on_cab,levels = c("Never on LAI ART","Ever on LAI ART"))) |>
      summarize(.by = c({{in_col}},ever_on_cab),
                n = n()) |>
      complete({{in_col}},ever_on_cab) |>
      filter({{in_col}}!="Unknown" | ({{in_col}}=="Unknown" & n > 0)) |>
      replace_na(list(n = 0)) |>
      mutate(.by = ever_on_cab,
             pct = n/sum(n,na.rm = T),
             bar_text = str_c(round(100*pct,0),"%"),
             axis_text = str_c(ever_on_cab," (",n," out of ",sum(n)," PWH)"),
             axis_text = fct_rev(axis_text)) |>
      filter(.by = {{in_col}}, sum(n) > 0)
  }
  
  
  group_var_levels <- input_df |> pull({{in_col}}) |> levels()
  group_var_present <- temp |> pull({{in_col}}) |> unique()
  missing_group_vars <- group_var_levels[!group_var_levels %in% group_var_present]
  missing_group_vars <- missing_group_vars[!missing_group_vars %in% c("Unknown","Other")]
  caption_text = data.table::fifelse(length(missing_group_vars)==0,"",
                         str_c("The following groups have n=0 PWH: ",
                               paste(missing_group_vars,
                                     collapse = ", ")))
  
  base_size <- base_size_in # set dynamically
  text_size <- base_size / 2.5

  if (by_cab_status == FALSE){
    p <- temp |>
      ggplot(aes(x = 100*pct,y = axis_text)) + 
      geom_bar(position = "dodge",stat = "identity", fill = "#08519C", width = 0.7) + 
      geom_text(aes(label = bar_text),
                size = text_size,
                hjust = -0.1,
                family = "Roboto") +
      scale_y_discrete(labels = \(x) str_wrap(x, width = 70)) 
  } else if (by_cab_status == TRUE){
    p <- temp |>
      ggplot(aes(x = 100*pct,y = axis_text)) + 
      geom_bar(position = "dodge",stat = "identity", aes(fill = ever_on_cab), width = 0.7) + 
      geom_text(aes(label = bar_text),
                size = text_size,
                hjust = -0.1,
                family = "Roboto") +
      scale_y_discrete(labels = \(x) str_wrap(x, width = 70)) +
      scale_fill_manual(values = c(
        "Never on LAI ART" = "#9ECAE1",
        "Ever on LAI ART" = "#08519C"
      )) +
      facet_wrap(as.formula(paste("~", rlang::as_name(rlang::enquo(in_col)))), 
                 ncol = 1, scales = "free_y",
                 strip.position = "left")
  }
  
  p <- p + 
    labs(title = title_text,
         caption = caption_text) + 
    theme_minimal(base_size = base_size,
                  base_family = "Roboto") +
    theme(plot.title = element_text(size = rel(1.3)),
          plot.title.position = "plot",
          strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0, size = rel(1.5),hjust = 1),
          axis.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_text(hjust = 1, size = rel(1.4),color = "black"),
          plot.margin = margin(5,0,5,0),
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0),
          legend.position = "none")
  
  plot_data <- ggplot_build(p)$data
  x_max <- max(plot_data[[1]]$x) * 1.15
  
  p + expand_limits(x =x_max)
  
}

# Get initiation cascade dataframe
get_IC_df <-function(input_df){
  
  basic_df <- input_df |>
    # 1 assessed
    mutate(Assessed=case_when(
      icab_rpv_counsel_ever==1 | icab_rpv_screen_ever==1 ~1,
      icab_rpv_counsel_ever==0 & icab_rpv_screen_ever==0 ~0,
      .default=0
    ),
    # 1a educated
    Educated = data.table::fifelse(icab_rpv_counsel_ever == 1,1,0),
    # 1c screened
    Screened = data.table::fifelse(icab_rpv_screen_ever == 1,1,0),
    # 3 prescribed
    Prescribed=case_when(
      icab_rpv_rx==1 ~ 1,
      !is.na(icab_rpv_shot1_date) | 
        !is.na(icab_rpv_shot2_date) ~1,
      .default = 0
    ),
    # 4 initiated
    Initiated=case_when(
      !is.na(icab_rpv_shot1_date) | 
        !is.na(icab_rpv_shot2_date) ~1,
      .default = 0
    ),
    # 5 sustained
    Sustained=case_when(
      Initiated & is.na(icab_rpv_discontinued)~1,
      Initiated & icab_rpv_discontinued==0~1,
      icab_rpv_discontinued==1~0,
      .default=0
    ),
    PWH=1, PWH1=1)
  
  # 1b interested
  interested_df <- input_df |>
    select(alai_up_uid, contains("icab_rpv_counsel")&contains("outcome")) |>
    pivot_longer(cols = matches("icab_rpv_counsel.*outcome"),
                 names_to = "counsel_event", values_to = "counsel_outcome") |>
    group_by(alai_up_uid) |>
    summarize(Interested = {
      val <- dplyr::last(counsel_outcome,na_rm = T)
      data.table::fcase(
        val == 1, 0,
        val == 3, 1,
        default = as.numeric(val)
      )},
      .groups = "drop"
    )
  
  disinterest_df <- input_df |>
    select(alai_up_uid, contains("icab_rpv_disinterest")&contains("reason")) |>
    rename_with(~paste0(., "_dis_outcome"), !contains("_other") & contains("reason")) |>
    pivot_longer(
      cols = contains("disinterest")&contains("reason"),
      names_to = c("event_base", ".value"),
      names_pattern = "(.*reason_\\d+)_(dis_outcome|other)"
    ) |>
    group_by(alai_up_uid) |>
    summarize(
      disinterest_reason = {
        val <- last(dis_outcome,na_rm = T)
        data.table::fifelse(is.null(val),NA_real_,as.numeric(val))
      }, 
      disinterest_other_reason = {
        val <- dplyr::last(dis_outcome,na_rm = T)
        event = other[max(which(!is.na(dis_outcome)))]
        data.table::fifelse(val == 20, event, NA_character_)
      },
      .groups = "drop"
    )
  
  # STEP 4: Eligible
  eligible_df <- input_df |>
    select(alai_up_uid,contains("icab_rpv_screen")&contains("outcome")) |>
    pivot_longer(cols = matches("icab_rpv_screen.*outcome"),
                 names_to = "screen_event", values_to = "screen_outcome") |>
    group_by(alai_up_uid) |>
    summarise(
      Eligible = {
        val <- dplyr::last(screen_outcome, na_rm = T)
        data.table::fifelse(is.null(val),NA_real_,as.numeric(val))
      },
      .groups = "drop"
    )
  
  # STEP 5: Not Eligible Reason
  not_elig_df <- input_df |>   
    select(alai_up_uid, contains("icab_rpv_not_elig")&contains("reason")) |>
    rename_with(~paste0(., "_not_elig_outcome"), !contains("_oth") & contains("reason")) |>
    rename_with(~str_replace(., "oth$", "other")) |>
    pivot_longer(cols = contains("not_elig"),
                 names_to = c("event_base",".value"),
                 names_pattern = "(.*)_(not_elig_outcome|other)") |> 
    mutate(not_elig_outcome = str_replace(not_elig_outcome,"NA",NA_character_)) |>
    group_by(alai_up_uid) |>
    summarise(
      not_elig_reason =dplyr::last(not_elig_outcome,na_rm = T),
      not_elig_other_reason = {
        val <- dplyr::last(not_elig_outcome,na_rm = T)
        event = other[max(which(!is.na(not_elig_outcome)))]
        data.table::fifelse(val == 20, event, NA_character_)
      },
      .groups = "drop"
    )
  
  # STEP 6: Join everything
  IC_df <- basic_df |>
    left_join(interested_df, by = "alai_up_uid") |>
    left_join(disinterest_df, by = "alai_up_uid") |>
    left_join(eligible_df, by = "alai_up_uid") |>
    left_join(not_elig_df, by = "alai_up_uid") |>
    mutate(
      PWH = 1,
      PWH1 = 1,
      `Interested & Eligible` = data.table::fifelse(Interested == 1 & Eligible == 1,1,0)
    )
  
  return(IC_df)
}

prepare_ic_summary <- function(input_df) {
  input_df |>
    get_IC_df() |>
    group_by(site,age_cat, sex_birth, race, ethnicity, insurance_status, 
             gender_id, 
             risk_msm, risk_idu, risk_heterosex,
             housing_status, employment_status,
             poverty_level, immigration_status_undoc, 
             language, incarceration_history) |>
    summarise(
      PWH = sum(PWH),
      PWH1 = sum(PWH1),
      Assessed = sum(Assessed, na.rm = TRUE),
      Educated = sum(Educated, na.rm = TRUE),
      Interested = sum(Interested==1, na.rm = TRUE),
      Screened = sum(Screened, na.rm = TRUE),
      Eligible = sum(Eligible, na.rm = TRUE),
      `Interested & Eligible` = sum(`Interested & Eligible`,na.rm = T),
      Prescribed = sum(Prescribed, na.rm = TRUE),
      Initiated = sum(Initiated, na.rm = TRUE),
      Sustained = sum(Sustained, na.rm = TRUE),
      .groups = "drop"
    ) |>
    pivot_longer(cols = c(PWH, Assessed, Educated, Interested, 
                          Screened, Eligible, `Interested & Eligible`,
                          Prescribed, Initiated, Sustained),
                 names_to = "Variable", values_to = "Value")
}

# plot initiation cascade
ic_var_plot <- function(input_df,
                        in_var,
                        by_group = FALSE,
                        group_var = NA,
                        base_size_in,
                        selected_site = NULL,
                        selected_year = NULL,
                        assessed_choice = "Yes"){
  
  base_size <- base_size_in # set dynamically
  text_size <- base_size_in / 2.5
  
  if (assessed_choice == "Yes"){
    prescribed_lag <- "Interested & Eligible"
  } else {
    prescribed_lag <- "PWH"
  }
  
  if (by_group == FALSE){
    ic_df <- input_df |>
      summarise(.by = Variable,
                Value=sum(Value), PWH1=sum(PWH1))|>
      group_by(Variable) |>
      arrange(match(Variable,c('PWH', 'Assessed','Educated',
                               'Interested', 'Screened', 'Eligible', 
                               'Interested & Eligible',
                               'Prescribed', 'Initiated', 'Sustained'))) |> 
      ungroup() |>
      mutate(prev_lab = case_when(Variable == "Educated" ~ "PWH",
                                  Variable == "Interested" ~ "Educated",
                                  Variable == "Screened" ~ "PWH",
                                  Variable == "Eligible" ~ "Screened",
                                  Variable == "Interested & Eligible" ~ "Assessed",
                                  Variable == "Prescribed" ~ prescribed_lag,
                                  .default =  lag(Variable)),
             prev = Value[match(prev_lab, Variable)],
             Variable = data.table::fifelse(Variable != "PWH",str_to_lower(Variable),Variable),
             prev_lab = data.table::fifelse(prev_lab != "PWH",str_to_lower(prev_lab),prev_lab),
             Percent=Value/prev,
             y_lab = str_c(Value," ",Variable," out of ",prev," ", prev_lab)) |>
      filter(Variable == {{in_var}}) 
    
    max_x = max(ic_df$Percent,na.rm=T)
    x_lim = max(1.15,max_x+0.15)
    
    if (in_var %in% c("assessed","educated","screened")) {
      if (is.null(selected_year)){
        title_text = str_c(str_to_title(unique(ic_df$Variable)), 
                           " at ",selected_site,
                           " among ",unique(ic_df$prev_lab))
      } else {
        title_text = str_c(str_to_title(unique(ic_df$Variable)), 
                           " at ",selected_site,
                           " among ",unique(ic_df$prev_lab), 
                           " active in ",selected_year)
      }
    } else {
      if (is.null(selected_year)){
        title_text = str_c(str_to_title(unique(ic_df$Variable)), 
                           " at ",selected_site,
                           " among ",unique(ic_df$prev_lab), 
                           " PWH")
      } else {
        title_text = str_c(str_to_title(unique(ic_df$Variable)), 
                           " at ",selected_site,
                           " among ",unique(ic_df$prev_lab), 
                           " PWH active in ",selected_year)
      }
      
    }
    
    
    p <- ic_df |>
      ggplot(aes(y = y_lab, x= Percent)) + 
      geom_col(fill="#08519C", width = 0.7) +
      labs(y = NULL, x = NULL) +
      expand_limits(y = 1)+
      scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1),
                         limits = c(0,x_lim)) +
      labs(title = str_wrap(title_text,80)) + 
      theme_minimal(base_size = base_size,
                    base_family = "Roboto")
  } else{
    group_var_string = group_var
    group_var = sym(group_var)
    
    ic_df <- input_df |>   
      summarise(.by = c(Variable, {{group_var}}),
                Value=sum(Value), PWH1=sum(PWH1)) |>
      group_by(Variable, {{group_var}})|>
      arrange(match(Variable,c('PWH', 'Assessed','Educated',
                               'Interested', 'Screened', 'Eligible', 
                               'Interested & Eligible',
                               'Prescribed', 'Initiated', 'Sustained'))) |> 
      ungroup() |>
      mutate(.by = {{group_var}},
             prev_lab = case_when(Variable == "Educated" ~ "PWH",
                                  Variable == "Interested" ~ "Educated",
                                  Variable == "Screened" ~ "PWH",
                                  Variable == "Eligible" ~ "Screened",
                                  Variable == "Interested & Eligible" ~ "Assessed",
                                  Variable == "Prescribed" ~ prescribed_lag,
                                  .default =  lag(Variable)),
             prev = Value[match(prev_lab, Variable)],
             Variable = data.table::fifelse(Variable != "PWH",str_to_lower(Variable),Variable),
             prev_lab = data.table::fifelse(prev_lab != "PWH",str_to_lower(prev_lab),prev_lab),
             Percent=Value/prev,
             y_lab = str_c({{group_var}}," (",Value," ",Variable," out of ",prev," ", prev_lab,")"),
             small_n = data.table::fifelse(prev < 10,"0","1")) |>
      filter(Variable == {{in_var}},
             !is.nan(Percent))
    
    
    group_var_levels <- input_df |> pull({{group_var}}) |> levels()
    group_var_present <- ic_df |> pull({{group_var}}) |> unique()
    missing_group_vars <- group_var_levels[!group_var_levels %in% group_var_present]
    missing_group_vars <- missing_group_vars[!missing_group_vars %in% c("Unknown","Other")]
    caption_text = data.table::fifelse(length(missing_group_vars)==0,"",
                                       str_c("The following groups have n=0 ", 
                                             unique(ic_df$prev_lab),": ",paste(missing_group_vars,
                                                                               collapse = ", ")))
    
    caption_text <- str_c("Dashed line indicates overall average. Gray bars have denominator <10. ",
                          caption_text)
    
    avg_value <- ic_df |>
      ungroup() |>
      summarize(pct = sum(Value)/sum(prev)) |> 
      pull(pct)
    
    max_x = max(ic_df$Percent,na.rm=T)
    x_lim = max(1.15,max_x+0.15)
    
    title = case_when(group_var_string == "age_cat" ~ "Age",
                      group_var_string == "race" ~ "Race",
                      group_var_string == "ethnicity" ~ "Ethnicity",
                      group_var_string == "sex_birth" ~ "Sex",
                      group_var_string == "insurance_status" ~ "Insurance status",
                      group_var_string == "housing_status" ~ "Housing status",
                      group_var_string == "gender_id" ~ "Gender",
                      group_var_string == "risk_msm" ~ "Risk MSM",
                      group_var_string == "risk_idu" ~ "Risk IDU",
                      group_var_string == "risk_heterosex" ~ "Risk Heterosex",
                      group_var_string == "employment_status" ~ "Employment status",
                      group_var_string == "poverty_level" ~ "Poverty level",
                      group_var_string == "immigration_status_undoc" ~ "Immigration status",
                      group_var_string == "language" ~ "Language",
                      group_var_string == "incarceration_history" ~ "Incarceration history",
                      group_var_string == "site" ~ "Site")
    
    
    if (in_var %in% c("assessed","educated","screened")) {
      if (is.null(selected_year)){
        title_text = str_c(str_to_title(unique(ic_df$Variable)), 
                           " at ",selected_site,
                           " among ",unique(ic_df$prev_lab), 
                           " by ", title)
      } else {
        title_text = str_c(str_to_title(unique(ic_df$Variable)), 
                           " at ",selected_site,
                           " among ",unique(ic_df$prev_lab), 
                           " active in ",selected_year,
                           " by ", title)
      }
    } else {
      if (is.null(selected_year)){
        title_text = str_c(str_to_title(unique(ic_df$Variable)), 
                           " at ",selected_site,
                           " among ",unique(ic_df$prev_lab), 
                           " PWH by ", title)
      } else {
        title_text = str_c(str_to_title(unique(ic_df$Variable)), 
                           " at ",selected_site,
                           " among ",unique(ic_df$prev_lab), 
                           " PWH active in ",selected_year,
                           " by ", title)
      }
    }
    
    p <- ic_df |>
      ggplot(aes(y = fct_rev(y_lab), x= Percent)) +
      geom_col(aes(fill = small_n), width = 0.7) +
      geom_vline(xintercept = avg_value, linetype = "dashed",
                 color = "black", linewidth = 0.5) + 
      scale_fill_manual(values= c("1" = "#08519C","0" = "gray")) + 
      scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1),
                         limits = c(0,x_lim)) +
      labs(title = str_wrap(title_text,80),
           y = NULL, x = NULL,
           caption = str_wrap(caption_text,120)) +
      theme_minimal(base_size = base_size,
                    base_family = "Roboto") 
  }
  
  if (nrow(p$data) == 1){
    p +
      geom_text(aes(label = paste0(round(Percent * 100), '%')),
                position = position_dodge(width = 0.9),
                hjust = -0.1,
                size=text_size*0.8, fontface = "bold",
                family = "Roboto") +
      theme(legend.position = 'none',
            title = element_text(size = rel(1)),
            axis.text.y = element_text(size = rel(1.3), color = "black"),
            panel.grid = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0, size = rel(0.9)))
  } else {
    p + 
      geom_text(aes(label = paste0(round(Percent * 100), '%')),
                position = position_dodge(width = 0.9),
                hjust = -0.1,
                size=text_size*0.9, fontface = "bold",
                family = "Roboto") +
      theme(legend.position = 'none',
            title = element_text(size = rel(1.1)),
            axis.text.y = element_text(size = rel(1.4), color = "black"),
            panel.grid = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.caption = element_text(size = rel(0.9),hjust = 0))
  }
  
}


configure_plots <- function(..., num_cols = c(1,2)){
  plot_list <- list(...)
  
  plot_sizes <- map(plot_list, \(x) nrow(x$data))

  if (num_cols == 1){
    rel_heights = plot_sizes
    final_plot = reduce(plot_list, `/`) + plot_layout(heights = rel_heights)
  } else if (num_cols==2){
    rel_heights1 = as_vector(plot_sizes[seq(1, length(plot_sizes), by = 2)])
    rel_heights2 = as_vector(plot_sizes[seq(2, length(plot_sizes), by = 2)])
    
    final_plot = (reduce(plot_list[seq(1,length(plot_list),by = 2)],`/`) + 
                    plot_layout(heights = rel_heights1)) |
      (reduce(plot_list[seq(2,length(plot_list),by = 2)],`/`) + 
         plot_layout(heights = rel_heights2))
  }
  
  final_plot
}

plot_outcome_by_month <- function(input_df, title_string, by_outcome = F,base_size_in = 14,
                                  by_person = T){
  base_size <- base_size_in # set dynamically
  text_size <- base_size / 2.5
  if (by_person == TRUE){
    temp <- input_df |>
      mutate(.by = alai_up_uid,
             first_date = min(date,na.rm = T),
             last_date = max(date,na.rm = T),
             first_date = data.table::fifelse(first_date == Inf, NA, first_date),
             last_date = data.table::fifelse(last_date == -Inf, NA, last_date)) |>
      filter(.by = alai_up_uid,
             date == last_date) |>
      select(-event,-date) |>
      distinct() |>
      mutate(period = floor_date(last_date,unit = "months")) 
  } else {
    temp <- input_df |>
      mutate(period = floor_date(date,unit = "months")) 
  }
  
  if (by_outcome == F){
    p <- temp |>
      group_by(period) |>
      count() |>
      ungroup() |>
      drop_na() |>
      ggplot(aes(x = period, y = n)) +
      geom_col(fill = "#08519C") + 
      labs(x = NULL, y = NULL)
    
  } else {
    p <- temp |>
      group_by(period, outcome) |>
      count() |>
      ungroup() |>
      drop_na() |>
      ggplot(aes(x = period, y = n, fill = outcome)) +
      geom_col() + 
      labs(x = NULL, y = NULL) + 
      scale_fill_manual(name = NULL,
                        values = c("i1" = "#fE5000",
                                   "i2" = "#9ECAE1",
                                   "i3" = "#08519C",
                                   "e0" = "#fE5000",
                                   "e1" = "#08519C"),
                        labels = c("i1" = "Not interested",
                                   "i2" = "Maybe interested",
                                   "i3" = "Interested",
                                   "e0" = "Not eligible",
                                   "e1" = "Eligible")) 
    
  }
  
  first_date <- min(p$data$period)
  last_date <- max(p$data$period)
  date_diff = as.numeric(difftime(last_date,first_date))
  if (date_diff <= 92) {
    date_breaks <- "1 month"
  } else if (date_diff > 92 & date_diff < 365*2) {
    date_breaks <- "3 months"
  } else {
    date_breaks <- "6 months"
  }
  
  p <- p + labs(title = str_c(title_string," (n=",sum(p$data$n),")")) + 
    scale_y_continuous(breaks = \(x) {
      b <- pretty(x)
      b <- b[round(b) == b] # Keep only whole numbers
      if (length(b) == 1){c(b,1)} else {b}
    }) +
    scale_x_date(date_labels = "%b %Y",
                 date_breaks = date_breaks) + 
    theme_minimal(base_size = base_size,
                  base_family = "Roboto")
  
  p
  
}

# not interested reason plot.
not_interested_reason_func <- function(input_df, base_size_in){
  base_size <- base_size_in # set dynamically
  text_size <- base_size / 2.5
  
  ic_df <- get_IC_df(input_df)
  
  disinterest_df <- ic_df |>
    mutate(disinterest_reason = factor(
      case_when(
        disinterest_reason == 1 ~ "Satisfaction with current regimen",
        disinterest_reason == 2 ~ "Fear/dislike of needles",
        disinterest_reason == 3 ~ "Quantity of clinic visits",
        disinterest_reason == 4 ~ "Concerns about cost",
        disinterest_reason == 5 ~ "Concerns about side effects",
        disinterest_reason == 6 ~ "Concerns about newness of modality",
        disinterest_reason == 20 ~ "Other",
        .default = "Unknown"
      ),
      levels = c("Satisfaction with current regimen",
                 "Fear/dislike of needles",
                 "Quantity of clinic visits",
                 "Concerns about cost",
                 "Concerns about side effects",
                 "Concerns about newness of modality",
                 "Other",
                 "Unknown"))) |>
    filter(Educated == 1,
           Interested==0 | Interested == 2) |>
    group_by(disinterest_reason) |>
    count() |>
    ungroup() |>
    complete(disinterest_reason) |>
    replace_na(list(n = 0)) |>
    filter(disinterest_reason!="Unknown" | 
             (disinterest_reason=="Unknown" & n > 0)) |>
    mutate(
      percent = n / sum(n), 
      labels = paste0(disinterest_reason,'\n',n,"/",sum(n)),
      labels = fct_rev(fct_infreq(labels, w = n))
    ) 
  
  # Get "other" reasons
  other_df <- ic_df |>
    filter(Educated == 1,
           Interested==0 | Interested == 2,
           disinterest_reason == 20) |>
    group_by(disinterest_other_reason) |>
    count() |>
    drop_na() |>
    arrange(-n) |>
    mutate(text = str_c(disinterest_other_reason," (",n,")"))
  
  other_reason_list <- other_df |>
    pull(text) |>
    str_c(collapse = ", ")
    
  
  if (nrow(disinterest_df)==0){
    
  } else {
    disinterest_df |>
      ggplot(aes(y = labels, x = percent, fill = factor(disinterest_reason))) + 
      geom_col() +
      scale_fill_manual(values = rep("#08519C",nrow(disinterest_df))) + 
      geom_text(aes(label = paste0(round(percent * 100), '%')),
                vjust = 0.5,hjust = -0.1,
                size=text_size, fontface = "bold",
                family = "Roboto") +
      labs(y = NULL, x = NULL,
           caption = str_wrap(
             str_c("Other reasons listed include: ", other_reason_list),
             120
           )) +
      scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1),
                         limits = c(0,1)) +
      theme_minimal(base_size = base_size,
                    base_family = "Roboto") + 
      theme(legend.position = 'none',
            axis.text.y = element_text(size = rel(1.5), color = "black"),
            axis.text.x = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0))
  }
}

# not eligible reason plot
not_eligible_reason_func <- function(input_df,base_size_in){
  base_size <- base_size_in # set dynamically
  text_size <- base_size / 2.5
  
  ic_df <- get_IC_df(input_df)
  
  not_elig_df <- ic_df |>
    mutate(not_elig_reason = str_split(not_elig_reason, ",")) |> 
    unnest(not_elig_reason) |>
    mutate(not_elig_reason = factor(
      case_when(
        not_elig_reason == 1 ~ "Currently not suppressed",
        not_elig_reason == 2 ~ "Known or suspected drug resistance",
        not_elig_reason == 3 ~ "History of treatment failure",
        not_elig_reason == 4 ~ "Weighs <35kg",
        not_elig_reason == 5 ~ "Younger than 12 years",
        not_elig_reason == 6 ~ "History of visit non-adherence",
        not_elig_reason == 7 ~ "History of oral ART non-adherence",
        not_elig_reason == 8 ~ "Buttocks implant",
        not_elig_reason == 9 ~ "Pregnancy/Family planning",
        not_elig_reason == 10 ~ "Not covered--Ryan White",
        not_elig_reason == 20 ~ "Other",
        .default = "Unknown"
      ),
      levels = c("Currently not suppressed",
                 "Known or suspected drug resistance",
                 "History of treatment failure",
                 "Weighs <35kg",
                 "Younger than 12 years",
                 "History of visit non-adherence",
                 "History of oral ART non-adherence",
                 "Buttocks implant",
                 "Pregnancy/Family planning",
                 "Not covered--Ryan White",
                 "Other",
                 "Unknown"))) |>
    filter(Assessed == 1,
           Eligible==0) |>
    group_by(not_elig_reason) |>
    count() |>
    ungroup() |>
    complete(not_elig_reason) |>
    replace_na(list(n = 0)) |>
    filter(not_elig_reason!="Unknown" | 
             (not_elig_reason=="Unknown" & n > 0)) |>
    mutate(
      percent = n / sum(n), 
      labels = paste0(not_elig_reason,'\n',n,"/",sum(n)),
      labels = fct_rev(fct_infreq(labels, w = n))
    ) 
  
  # Get "other" reasons
  other_df <- ic_df |>
    filter(Assessed == 1,
           Eligible==0,
           not_elig_reason == 20) |>
    group_by(not_elig_other_reason) |>
    count() |>
    drop_na() |>
    arrange(-n) |>
    mutate(text = str_c(not_elig_other_reason," (",n,")"))
  
  other_reason_list <- other_df |>
    pull(text) |>
    str_c(collapse = ", ")
  
  not_elig_df |>
    ggplot(aes(y = labels, x = percent, fill = factor(not_elig_reason))) + 
    geom_col() +
    scale_fill_manual(values = rep("#08519C",nrow(not_elig_df))) + 
    geom_text(aes(label = paste0(round(percent * 100), '%')),
              vjust = 0.5,hjust = -0.1,
              size=text_size, fontface = "bold",
              family = "Roboto") +
    labs(y = NULL, x = NULL,
         caption = str_wrap(
           str_c("Other reasons listed include: ", other_reason_list),
           120
         )) +
    scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1),
                       limits = c(0,1)) +
    theme_minimal(base_size = base_size,
                  base_family = "Roboto") + 
    theme(legend.position = 'none',
          axis.text.y = element_text(size = rel(1.5),color = "black"),
          axis.text.x = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0))
}

discontinued_reason_func <- function(input_df, base_size_in){
  base_size <- base_size_in # set dynamically
  text_size <- base_size / 2.5
  
  discont_reason <- input_df |>
    filter(!is.na(icab_rpv_shot1_date) | 
             !is.na(icab_rpv_shot2_date),
           icab_rpv_discontinued == 1) |>
    mutate(icab_rpv_discontinued_reason = factor(
      case_when(
        icab_rpv_discontinued_reason == 1 ~ "Satisfaction with previous regimen",
        icab_rpv_discontinued_reason == 2 ~ "Fear/dislike of needles",
        icab_rpv_discontinued_reason == 3 ~ "Quantity of visit schedule",
        icab_rpv_discontinued_reason == 4 ~ "Concerns about cost",
        icab_rpv_discontinued_reason == 5 ~ "Concerns about newness of modality",
        icab_rpv_discontinued_reason == 6 ~ "Failing treatment",
        icab_rpv_discontinued_reason == 7 ~ "Lost to follow up",
        icab_rpv_discontinued_reason == 8 ~ "Concerns about side effects",
        icab_rpv_discontinued_reason == 9 ~ "Switched out of clinic",
        icab_rpv_discontinued_reason == 10 ~ "Change/loss of insurance/payor",
        icab_rpv_discontinued_reason == 20 ~ "Other",
        icab_rpv_discontinued_reason == "No dose in >90 days" ~ icab_rpv_discontinued_reason,
        .default = "Unknown"),
      levels = c("Satisfaction with previous regimen",
                 "Fear/dislike of needles",
                 "Quantity of visit schedule",
                 "Concerns about cost",
                 "Concerns about newness of modality",
                 "Failing treatment",
                 "Lost to follow up",
                 "Concerns about side effects",
                 "Switched out of clinic",
                 "Change/loss of insurance/payor",
                 "Other",
                 "No dose in >90 days",
                 "Unknown")
    )) |>
    filter(icab_rpv_discontinued == 1) |>
    group_by(icab_rpv_discontinued_reason) |>
    count() |>
    ungroup() |>
    complete(icab_rpv_discontinued_reason) |>
    replace_na(list(n = 0)) |>
    filter(icab_rpv_discontinued_reason!="Unknown" | 
             (icab_rpv_discontinued_reason=="Unknown" & n > 0)) |>
    mutate(
      percent = n / sum(n), 
      labels = paste0(icab_rpv_discontinued_reason,'\n',n,"/",sum(n)),
      labels = fct_rev(fct_infreq(labels, w = n))
    ) 
  
  # Get "other" reasons
  other_df <-  input_df |>
    filter(!is.na(icab_rpv_shot1_date) | 
             !is.na(icab_rpv_shot2_date),
           icab_rpv_discontinued == 1) |>
    group_by(icab_rpv_discontinued_reason_other) |>
    count() |>
    drop_na() |>
    arrange(-n) |>
    mutate(text = str_c(icab_rpv_discontinued_reason_other," (",n,")"))
  
  other_reason_list <- other_df |>
    pull(text) |>
    str_c(collapse = ", ")
  
  discont_reason |>
    ggplot(aes(y = labels, x = percent, fill = factor(icab_rpv_discontinued_reason))) + 
    geom_col() +
    scale_fill_manual(values = rep("#08519C",nrow(discont_reason))) + 
    geom_text(aes(label = paste0(round(percent * 100), '%')),
              vjust = 0.5,hjust = -0.1,
              size=text_size, fontface = "bold",
              family = "Roboto") +
    labs(y = NULL, x = NULL,
         caption = str_wrap(
           str_c("Other reasons listed include: ", other_reason_list),
           120
         )) +
    scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1),
                       limits = c(0,1.1)) +
    theme_minimal(base_size = base_size,
                  base_family = "Roboto") + 
    theme(legend.position = 'none',
          axis.text.y = element_text(size = rel(1.5),color = "black"),
          axis.text.x = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0))
}

prepare_cab_master_df <- function(input_df, interval_1, interval_2){
 
    # start withh shot data in long format
   input_df |>
    select(alai_up_uid,site,
           contains("icab_rpv_discontinued"),
           contains("icab_rpv_shot")&(contains("date")|contains("dose")),
           contains("bridge")) |>
    mutate(across(!alai_up_uid,as.character)) |> 
    pivot_longer(cols = contains("icab_rpv_shot")&(contains("date")|contains("dose")),
                 names_to = c("shot_index",".value"),
                 names_pattern = "icab_rpv_shot(\\d+)_(date|dose)") |>
    filter(!is.na(date)) |>
    # join to VL data in long format
    full_join(
      input_df |>
        select(alai_up_uid,site,    
               contains("icab_rpv_discontinued"),
               contains("hiv_vl")) |>
        mutate(across(!alai_up_uid,as.character)) |> 
        pivot_longer(cols = contains("hiv_vl")&!contains("pre_icab")&!contains("dx"),
                     names_to = c("vl_index",".value"),
                     names_pattern = "hiv_vl_(.*)_(date|result)") |> 
        filter(!is.na(result) | !is.na(date)) 
    ) |>
    arrange(alai_up_uid,date) |>
    mutate(shot_appt = if_else(!is.na(shot_index) & !is.na(date), 1, 0),
           vl_appt = if_else(!is.na(vl_index), 1, 0),
           date = as.Date(date)) |>
    # get first shot data, time from cab start, ind for on cab
    mutate(.by = alai_up_uid,
           first_shot_date = min(date[shot_appt == 1]),
           time_from_cab = as.numeric(difftime(date,as.Date(first_shot_date),units = "days")),
           on_cab = case_when(
             date >= first_shot_date & is.na(icab_rpv_discontinued_date) ~ 1,
             date >= first_shot_date & date <= icab_rpv_discontinued_date ~ 1,
             .default = 0)) |>
    # section to get pre-cab VL. either 3 months before to 2 weeks after, or recorded
    filter(time_from_cab > -93) |>
    mutate(
      .by = alai_up_uid,
      closest_vl_before_cab = if_else(
        vl_appt == 1 & time_from_cab <=14 & time_from_cab == max(time_from_cab[time_from_cab <= 14 & vl_appt == 1]),1,0),
      pre_icab_vl_date = case_when(any(closest_vl_before_cab == 1) ~
                                     min(date[closest_vl_before_cab == 1],na.rm = T),
                                   any(!is.na(hiv_vl_pre_icab_date)) ~
                                     min(as.Date(hiv_vl_pre_icab_date),na.rm = T),
                                   .default = NA),
      pre_icab_vl_result = case_when(any(closest_vl_before_cab == 1) ~
                                       min(result[closest_vl_before_cab == 1],na.rm = T),
                                     any(!is.na(hiv_vl_pre_icab_date)) ~
                                       min(hiv_vl_pre_icab_result,na.rm = T),
                                     .default = NA)) |>
    group_by(alai_up_uid) |> 
    fill(pre_icab_vl_date,pre_icab_vl_result, .direction = "downup") |>
    ungroup() |> 
    # only want data on cab
    filter(on_cab == 1)  |> 
    # section to cover VL variables
    mutate(.by = alai_up_uid,
           starting_vl = if_else(pre_icab_vl_result <= 2,"<50",">=50"),
           any_suppressed = case_when(pre_icab_vl_result <= 2 ~ 1,
                                      any(result <= 2,na.rm = T) ~ 1,
                                      .default = 0),
           all_suppressed = if_else(pre_icab_vl_result <= 2 & all(result <= 2,na.rm = T),1,0),
           
           first_under50 = if_else(time_from_cab == min(time_from_cab[result <= 2], na.rm = T),1,0),
           first_under50_time = case_when(
             pre_icab_vl_result <= 2 ~ 0,
             first_under50 == 1 ~ time_from_cab,
             .default = NA),
           first_under200 = if_else(time_from_cab == min(time_from_cab[result <= 3],na.rm = T),1,0),
           first_under200_time = case_when(
             pre_icab_vl_result <= 3 ~ 0,
             first_under200 == 1 ~ time_from_cab,
             .default = NA)) |>
    group_by(alai_up_uid) |>
    fill(first_under50_time, first_under200_time,.direction = "down") |> 
    ungroup() |>
    mutate(.by = alai_up_uid,
           elevated_time_ref50 = case_when(
             pre_icab_vl_result <= 2 & result >= 3 ~ time_from_cab,
             result >= 3 ~ time_from_cab - first_under50_time,
             .default = NA 
           ),
           elevated_time_ref200 = case_when(
             pre_icab_vl_result <= 3 & result >= 4 ~ time_from_cab,
             result >= 4 ~ time_from_cab - first_under200_time,
             .default = NA
           ),
           failure_time_ref50 = case_when(
             pre_icab_vl_result <= 2 & result >= 5 ~ time_from_cab,
             pre_icab_vl_result <= 2 & result >= 4 & lag(result) >= 4 ~ time_from_cab,
             result >= 5 ~ time_from_cab - first_under50_time,
             result >= 4 & lag(result) >= 4 ~ time_from_cab - first_under50_time,
             .default = NA
           ),
           failure_time_ref200 = case_when(
             pre_icab_vl_result <= 3 & result >= 5 ~ time_from_cab,
             pre_icab_vl_result <= 3 & result >= 4 & lag(result) >= 4 ~ time_from_cab,
             result >= 5 ~ time_from_cab - first_under200_time,
             result >= 4 & lag(result) >= 4 ~ time_from_cab - first_under200_time,
             .default = NA
           ),
           first_elevated_vl_ref50 = if_else(elevated_time_ref50 == min(elevated_time_ref50,na.rm = T),1,0),
           num_elevated_vl_ref50 = sum(result >= 3 & time_from_cab >= first_under50_time,na.rm = T),
           first_elevated_vl_ref50_time = if_else(first_elevated_vl_ref50 == 1, elevated_time_ref50,NA),
           first_elevated_vl_ref200 = if_else(elevated_time_ref200 == min(elevated_time_ref200,na.rm = T),1,0),
           num_elevated_vl_ref200 = sum(result >= 4 & time_from_cab >= first_under200_time,na.rm = T),
           first_elevated_vl_ref200_time = if_else(first_elevated_vl_ref200 == 1, elevated_time_ref200,NA),
           first_failure_ref50 = if_else(failure_time_ref50 == min(failure_time_ref50,na.rm = T),1,0),
           num_failure_ref50 = sum((result >= 5 |(result >= 4 & lag(result >= 4))) & 
                                     time_from_cab >= first_under50_time,na.rm = T),
           first_failure_ref50_time = if_else(first_failure_ref50 == 1,failure_time_ref50,NA),
           first_failure_ref200 = if_else(failure_time_ref200 == min(failure_time_ref200,na.rm = T),1,0),
           num_failure_ref200 = sum((result >= 5 |(result >= 4 & lag(result >= 4))) & 
                                      time_from_cab >= first_under200_time,na.rm = T),
           first_failure_ref200_time = if_else(first_failure_ref200 == 1,failure_time_ref200,NA),
           discontinued_time = if_else(icab_rpv_discontinued == 1,
                                       as.numeric(difftime(as.Date(icab_rpv_discontinued_date),
                                                           as.Date(first_shot_date),units = "days")),
                                       NA),
           last_cab_time = max(time_from_cab),
           time_from_50 = last_cab_time - first_under50_time,
           time_from_200 = last_cab_time - first_under200_time,
           any_elevated_vl = if_else(any(first_elevated_vl_ref50 == 1,na.rm = T),1,0),
           any_failure = if_else(any(first_failure_ref50 == 1,na.rm = T),1,0),
           vl_outcome = case_when(all_suppressed == 1 ~ "All <50",
                                  any_suppressed == 0 ~ "Never achieved <50",
                                  any_failure == 0 & any_elevated_vl == 0 ~ "All <50",
                                  any_failure == 0 & any_elevated_vl == 1 ~ "Any VL 50-1000, no failure",
                                  any_failure == 1 & any_elevated_vl == 1 ~ "Failure (VL >1000 or 2 VL >200)")) |>
    # section to cover cab dose variables
    mutate(shot_index = as.numeric(shot_index)) |>
    arrange(alai_up_uid,shot_index) |>
    group_by(alai_up_uid) |>
    fill(dose,.direction = "updown") |> # fill based on what's there, but check this with sites
    mutate(shot_date = if_else(shot_appt == 1, date, NA)) |>
    mutate(interval = if_else(!dose%in%c(4,5,6), shot_date-lag(shot_date),NA),
           late = case_when(dose %in% c(4,5,6) ~ NA,
                            lag(shot_index) == 1 & lag(dose) != 6 ~ interval > interval_1+7,
                            lag(dose) %in% c(1,3,4,5) ~ interval > interval_1+7,
                            lag(dose) %in% c(2,6) ~ interval > interval_2+7,
                            is.na(lag(dose)) ~ interval > interval_2 + 7,
                            .default = FALSE),
           early = case_when(dose %in% c(4,5,6) ~ NA,
                             lag(shot_index) == 1 & lag(dose) != 6  ~ interval < interval_1-7,
                             lag(dose) %in% c(1,3,4,5) ~ interval < interval_1-7,
                             lag(dose) %in% c(2,6) ~ interval < interval_2-7,
                             is.na(lag(dose)) ~ interval < interval_1 - 7,
                             .default = FALSE),
           bridged = case_when(date >= icab_rpv_bridge1_start_date & 
                                 date <= icab_rpv_bridge1_end_date ~ TRUE,
                               date >= icab_rpv_bridge2_start_date & 
                                 date <= icab_rpv_bridge2_end_date ~ TRUE,
                               .default = NA),
           on_time = case_when(late == TRUE & is.na(bridged) ~ "Late",
                               early == TRUE ~ "Early",
                               late == FALSE & early == FALSE ~ "On time",
                               late == TRUE & bridged == TRUE ~ "On time")) |>
    mutate(lag_dose = lag(dose),
           monthly = case_when(lag(shot_index) == 1 & lag_dose %in% c(1,2) ~ "Second injection",
                               lag_dose %in% c(3,4) ~ "Second injection",
                               lag_dose %in% c(1,5) ~ "Monthly",
                               lag_dose %in% c(2,6) ~ "Bimonthly",
                               .default = NA)) |>
    ungroup() |>
    arrange(alai_up_uid,date)
}

int_elig_table_func <- function(input_df, percent_type){
  
  temp <- get_IC_df(input_df) |>
    mutate(
      Educated = case_when(
        Educated == 1 ~ 1,
        .default = 0),
      Interested = case_when(
        Interested == 1 ~ 1,
        Interested == 2 ~ 2,
        .default = 0),
      Screened = case_when(
        Screened == 1 ~ 1,
        .default = 0),
      Eligible = case_when(
        Eligible == 1 ~ 1,
        .default = 0)) |>
    filter(Assessed == 1) |> 
    mutate(Eligible = if_else(Screened == 1, Eligible,NA),
           Interested = if_else(Educated == 1,Interested,NA),
           Interest_label = case_match(
             Interested,
             0 ~ "Not interested",
             1 ~ "Interested",
             2 ~ "Maybe interested",
             .default = "Not educated"
           ),
           interest_order = case_match(
             Interest_label,
             "Not educated" ~ 1,
             "Not interested" ~ 2,
             "Maybe interested" ~ 3,
             "Interested" ~ 4
           ),
           Eligibility_label = case_match(
             Eligible,
             0 ~ "Not eligible",
             1 ~ "Eligible",
             .default = "Not screened"
           ),
           elig_order = case_match(
             Eligibility_label,
             "Not screened" ~ 1,
             "Not eligible" ~ 2,
             "Eligible" ~ 3
           ),
           Interest_label = fct_reorder(Interest_label,interest_order),
           Eligibility_label = fct_reorder(Eligibility_label, elig_order)) |>
    group_by(Interest_label, Eligibility_label) |>
    count() |>
    ungroup() |>
    pivot_wider(names_from = Eligibility_label,
                values_from = n) |>
    janitor::adorn_totals(where = c("row","col")) |>
    as_tibble() |>
    pivot_longer(-Interest_label) 
  
  if (percent_type == "Table"){
    temp <- temp |>
      mutate(value = str_c(value, " (",
                           round(100*value/max(value,na.rm = T)),
                           "%)")) 
  } else if (percent_type == "Row"){
    temp <- temp |>
      mutate(.by = Interest_label,
             value = str_c(value, " (",
                           round(100*value/max(value,na.rm = T)),
                           "%)")) 
  } else if (percent_type == "Column"){
    temp <- temp |>
      mutate(.by = name,
             value = str_c(value, " (",
                           round(100*value/max(value,na.rm = T)),
                           "%)")) 
  }
  
  temp |>
    pivot_wider(names_from = name, values_from = value) |>
    select(any_of(c("Interest_label","Not screened","Not eligible","Eligible","Total"))) |>
    gt(rowname_col = "Interest_label") |>
    sub_missing() |>
    tab_spanner(label = "Screening outcome",
                columns = any_of(c("Not screened","Not eligible","Eligible"))) |>
    cols_align("center") |>
    tab_stubhead(label = "Education outcome") |>
    tab_footnote(
      footnote = "Interested and eligible, as in above chart",
      locations = cells_body(columns = "Eligible",
                             rows = Interest_label == "Interested"),
      placement = "right"
    ) |>
    tab_style(
      style = list(cell_text(weight = "bold"),
                   cell_fill(color = "#9ECAE1")),
      locations = cells_body(columns = "Eligible",
                             rows = Interest_label == "Interested")
    ) |>
    tab_footnote(
      footnote = "Opportunities for improvement.",
      locations = list(cells_body(columns = any_of("Eligible"),
                             rows = Interest_label == "Not educated"),
                       cells_body(columns = any_of("Eligible"),
                                  rows = Interest_label == "Maybe interested"),
                       cells_body(columns = any_of("Not screened"),
                                  rows = Interest_label == "Interested"),
                       cells_body(columns = any_of("Not screened"),
                                  rows = Interest_label == "Maybe interested")),
      placement = "right"
    ) |>
    tab_options(table.font.color = "black",
                table.font.names = "Roboto",
                table.width = pct(100))
  
}

ontime_plot_func <- function(input_df,interval_1,interval_2,
                             current_period){
  base_size <- 14
  text_size <- base_size / 3.5
  
  window_df <- tibble(
    monthly = factor(c("Monthly injection interval", "Bimonthly injection interval"),
                     levels =  c("Monthly injection interval","Bimonthly injection interval")),
    window_start = if_else(monthly == "Bimonthly injection interval",interval_2-7.5,interval_1-7.5),
    window_end = if_else(monthly == "Bimonthly injection interval",interval_2+7.5,interval_1+7.5))
  
  on_time_df <- input_df |>
    filter(!is.na(monthly)) |> 
    mutate(interval = as.numeric(interval),
           lag_dose = lag(dose),
           monthly = case_when(monthly == "Monthly" ~ "Monthly injection interval",
                               monthly == "Second injection" ~ "Monthly injection interval",
                               monthly == "Bimonthly" ~ "Bimonthly injection interval",
                               .default = NA)) |> 
    mutate(interval = if_else(interval > 100,101,interval)) |>
    group_by(interval, monthly, on_time) |>
    summarize(n = n()) |>
    filter(!is.na(interval)) |> 
    mutate(monthly = factor(monthly,
                            levels = c("Monthly injection interval","Bimonthly injection interval")),
           target_day = if_else(monthly == "Bimonthly injection interval",interval_2,interval_1),
           time_from_target = interval - target_day,
           shade_101 = if_else(interval == 101, "1","0"),
           label = if_else(interval == 101, ">100 days",NA)) |>
    ungroup() |>
    filter(monthly == current_period)
  
  p <- on_time_df |>
    ggplot(aes(x = interval, y = n, fill = on_time)) +
    geom_rect(data = window_df |>
                filter(monthly == current_period),
              aes(xmin = window_start, xmax = window_end), 
              fill = "gray",
              ymin = -Inf, ymax = Inf,
              alpha = 0.5,inherit.aes = FALSE) + 
    geom_col(aes(alpha = shade_101)) +
    geom_segment(data = function(d) d |> filter(!is.na(label)),
                 aes(x = interval, xend = interval, 
                     y = n, yend = n + 4.9),
                 color = "black") +
    # Text annotation above the bar
    geom_text(data = function(d) d |> filter(!is.na(label)),
              aes(label = label, y = n + 5),
              vjust = 0, hjust = 1, size = text_size,
              family = "Roboto") +
    scale_fill_manual(name = NULL,
                      values = c("Late" = "#fE5000",
                                 "On time" = "#08519C",
                                 "Early" = "#9ECAE1")) +
    scale_alpha_manual(values = c("1" = 0.5,
                                  "0" = 1),
                       guide = "none") + 
    scale_x_continuous(breaks = scales::breaks_width(10)) + 
    scale_y_continuous(breaks = scales::breaks_pretty()) + 
    labs(x = "Days since prior injection",
         y = "Number of injections administered",
         caption = str_wrap("Gray shaded area indicates on time injection window. On time doses outside of the injection window are due to oral bridging.",120)) + 
    theme_minimal(base_size = base_size,
                  base_family = "Roboto") +
    facet_wrap(.~monthly, ncol = 1) + 
    theme(legend.position = "right",
          panel.border = element_rect(color = "gray",fill = NA),
          axis.text = element_text(size = rel(1.1)),
          axis.title = element_text(size = rel(1)),
          legend.text = element_text(size = rel(1.1)),
          strip.text = element_text(size = rel(1.1)),
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0),
          panel.spacing = unit(30,"pt")
    )
  
  return(p)
}

full_report_table <- function(summary_df, cab_df){
  col_list <- c("Age" = "age_cat",
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
                "Incarceration history" = "incarceration_history")
  
  ic_outcomes <- summary_df |>
    pivot_wider(names_from = "Variable", values_from = "Value") |>
    summarize(Variable = "Overall",
              Value = "Overall",
              Assessed = sum(Assessed),
              Educated = sum(Educated),
              Interested = sum(Interested),
              Screened = sum(Screened),
              Eligible = sum(Eligible),
              `Interested & Eligible` = sum(`Interested & Eligible`),
              Prescribed = sum(Prescribed),
              Initiated = sum(Initiated),
              Sustained = sum(Sustained))
  
  for (i in 1:length(col_list)){
    col <- sym(col_list[i])
    col_name <- names(col_list)[i]
    
    temp <- summary_df |>
      pivot_wider(names_from = "Variable", values_from = "Value") |>
      group_by({{col}}) |>
      summarize(Assessed = sum(Assessed),
                Educated = sum(Educated),
                Interested = sum(Interested),
                Screened = sum(Screened),
                Eligible = sum(Eligible),
                `Interested & Eligible` = sum(`Interested & Eligible`),
                Prescribed = sum(Prescribed),
                Initiated = sum(Initiated),
                Sustained = sum(Sustained)) |>
      ungroup() |>
      rename(Value = {{col}}) |>
      mutate(Variable = col_name)
    
    ic_outcomes <- bind_rows(ic_outcomes,temp)
  }
  return(ic_outcomes)
}
