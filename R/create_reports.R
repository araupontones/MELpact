#' Create reports for dashboards and tables
#'
#' This function downloads the reports from Zoho, stores the raw data in downloads
#' and saves the clean data in clean data
#'
#' @param dir_downloads A directory where downloaded reports will be stored
#' @param dir_clean A directory where the clean reports will be stored


create_reports <- function(dir_downloads = "downloads",
                           dir_clean = "clean_data",
                           reports =c("Download_outputs", "all_projects","All_Quarters",
                                     "expected_outputs","expected_int", "expected_outcomes", "KPIs_Report" ),
                           refresh_token = "1000.b11df28b89daaeb2df10fa2c43178db6.6f953944b607f0ff366915cb9a770edc",
                           ...){

  #check if dir clean exists
  if(!dir.exists(dir_clean)){

    dir.create(dir_clean)
  }



  #download reports into dir_downloads -------------------------------------------
  lapply(reports,function(report){

    download_report(dir_downloads = dir_downloads,
                    zoho_report = report,
                    access_token =  refresh_token
    )

  })


#import reports into environment -----------------------------------------------
  downloaded_reports <- lapply(reports, function(report){

    rds <- paste0(report,".rds")
    refile <- file.path(dir_downloads, rds)
    dd_report <- rio::import(refile)

    message(glue::glue("{report} observations: {nrow(dd_report)}"))
    return(dd_report)

   })


  names(downloaded_reports) <- reports

  data_projects <- downloaded_reports$all_projects


  data_resultados <- downloaded_reports$Download_outputs

  #define levels of quarters
  quarters <- sort(c(downloaded_reports$All_Quarters$quarter, "Unexpected", NA))


  #Clean and count reported results -------------------------------------------
  clean_resultados <- data_resultados %>%
    select(Project, Description_new, starts_with("training"),
           Result_Title, result_logframe, type_result,
           Quarter, Status, funds, Modified_Time, ID) %>%
    rename(Quarter_reported = Quarter) %>%
    ## count indicator (the variable counts, counts the number of times and indicator has been met)
    mutate(count = if_else(str_detect(result_logframe, "Int. Out. 4|Outcome 4"), as.numeric(funds), 1),
           Quarter_reported = case_when(str_detect(Quarter_reported, "list") ~ str_extract(Quarter_reported, '(?<=display_value = \")(.*?)(?=\\")'),
                                        T ~ Quarter_reported
           )
    ) %>%
    select(-funds)


  ## create indicators of training
  skills = clean_resultados %>%
    #'keep only trainings -----------------------------
    filter(result_logframe == "Output 1. 'Training of key actors'",
           !is.na(Status)) %>%
    select(-count) %>%
    mutate(across(starts_with("training"), as.numeric)) %>%
    rowwise() %>%
    #count participants to trainings -------------------
  mutate(training_total = sum(c(training_female, training_male, training_unknown), na.rm = T),
         training_female_prop = round(training_female/(training_male + training_female), digits = 1),
         training_personDays = training_total * training_duration) %>%
    pivot_longer(cols = starts_with("training"),
                 names_to = 'indicator',
                 values_to = 'count') %>%
    filter(indicator != "training_duration") %>%
    #' name indicator as they are expressed in the logframe
    mutate(result_logframe = case_when(indicator == "training_total" ~ "Output 1.1 Key individuals trained",
                                       indicator == "training_personDays"~  "Output 1.2 Person days of training",
                                       indicator == "training_female" ~ "Output 1.1.1 Key individuals (females) trained",
                                       indicator == "training_male" ~  "Output 1.1.1 Key individuals (males) trained",
                                       indicator == "training_unknown" ~ "Output 1.1.1 Key individuals (unknown) trained",
                                       indicator == "training_female_prop" ~  "Output 1.1.1 Key individuals (female prop) trained"
    )
    #drop redundant variable
    ) %>%
    select(- indicator)



  ## Append resultados con skills (This contains all the results reported so far)
  achieved_results= clean_resultados %>%
    #'drop training unclean indicators
    select(-starts_with("training")) %>%
    #'bind with training indicators
    rbind(skills) %>%
    #'All of these results were reported
    mutate(Type = "In_reported")



  ## Read the expected results ---------------------------------------------------

  ## Append expected results
  expected = str_detect(reports, "expected")


  expected_results = do.call(plyr::rbind.fill,downloaded_reports[expected]) %>%
    select(-c(Project.Country, ID, ID_Date_field, ID_Project)) %>%
    rename(Quarter_expected = Date_field) %>%
    #Identify results that were expected
    mutate(Was_expected = T,
           #for some tables the quarter is exported as a character in a list form
           Quarter_expected = case_when(str_detect(Quarter_expected, "list") ~
                                          str_extract(Quarter_expected, '(?<=\")(.*?)(?=\\")'),
                                        Quarter_expected =="" ~NA_character_,
                                        T ~ Quarter_expected)
    )




  #### Reporte con resultados alcanzados y expected ------------------------------
  reporte_raw=
    #' results reported
    achieved_results %>%
    #'join with expected
    full_join(expected_results, by=c("Project", c("Result_Title"="Title"), c("result_logframe"= "Expected")),
              suffix = c("_reported", "_expected")) %>%
    #' clean indicators for expected results
    mutate(
      #'set count to 0 if the result was expected
      count = if_else(is.na(count) & Was_expected == T, 0, count),
      #' identify if the result was expected
      Was_expected = case_when(is.na(Was_expected) ~ F,
                               T ~ T),
      Quarter_expected = case_when(Was_expected == F ~ "Unexpected",
                                   T ~ Quarter_expected)

    )




  ### Clean for good presentation -----------------------------------------------


  #'Define elements of the logframe
  elements = c("Output 1: Skills enhanced",
               "Output 2: Recommendations proposed",
               "Output 3: Knowledge generated and disseminated",
               "Output 4: Networks created and strengthened",
               "Intermediate Outcomes",
               "Outcomes"

  )



  reporte_clean = reporte_raw %>%
    rename(Indicator = result_logframe) %>%
    mutate(

      #' transform type to ordered factor ----------------------------------
      type_result = factor(type_result,
                           levels = c("Output",
                                      "Intermediate Outcome",
                                      "Outcome"),
                           ordered = T),
      #'clean Indicator text -------------------------------------------
      Indicator = str_replace(Indicator, "Int. Out.", "Int. Out"),
      Indicator = str_replace(Indicator, "Out2", "Out 2"),
      Indicator = str_replace(Indicator, "Out3", "Out 3"),
      Indicator = str_replace(Indicator, "Out4", "Out 4"),

      #'clean status for those that are expected but not reported yet
      Status = if_else(is.na(Status), "Not reported yet", Status),

      #' categorise indicators based on the elements of the logframe
      Element = case_when(str_detect(Indicator,"Output 1")~elements[1],
                          str_detect(Indicator,"Output 2")~elements[2],
                          str_detect(Indicator,"Output 3")~elements[3],
                          str_detect(Indicator,"Output 4")~elements[4],
                          str_detect(Indicator,"Outcome")~elements[6],
                          str_detect(Indicator,"Int.")~elements[5]),
      #'sort elements ------------------------------------------------------
      Sort = case_when(str_detect(Element,"1") ~ 1, #so we can sort by indicator level
                       str_detect(Element,"2")~2,
                       str_detect(Element,"3")~3,
                       str_detect(Element,"4")~4,
                       str_detect(Element,"Int")~5,
                       str_detect(Element,"Outcome")~6),
      #transform element to orered factor
      Element = factor(Element,
                       levels = elements,
                       ordered = T),
      #Transform quarters to factor
      across(c(Quarter_reported, Quarter_expected), function(x){
        factor(x,
               levels = quarters,
               ordered = T)
      })
    )



  ##Fetch key variables from other tables -----------------------------------------------------------------
  reporte_clean$Implementor = data_projects$Implementor[match(reporte_clean$Project, data_projects$Project_Name)]
  reporte_clean$Country = data_projects$Country[match(reporte_clean$Project, data_projects$Project_Name)]
  reporte_clean$component = data_projects$Lot[match(reporte_clean$Project, data_projects$Project_Name)]
  reporte_clean$ID_Project = data_projects$ID[match(reporte_clean$Project, data_projects$Project_Name)]




  ### Re-formatear para poder contar Expected and Approved por quarter
  reporte_cuenta = reporte_clean %>%
    pivot_longer(cols = c("Quarter_expected", "Quarter_reported"),
                 names_to = "Tipo",
                 values_to = "Quarter") %>%
    ##no contar los resultados que han sido rechazados
    filter(!is.na(Quarter),
           Quarter != "Unexpected",
           !(Status=="Rejected" & Tipo =="Quarter_reported")) %>%
    ##Empezar a contar
    mutate(Was_expected = if_else(Tipo == "Quarter_reported", F, Was_expected),
           count = if_else(Tipo == "Quarter_expected", 0, count)) %>%
    ##Crear cuenta de expected y achieved
    group_by(component,Country,Element, Indicator, Quarter) %>%
    summarise(Approved = sum(count, na.rm = T),
              Expected = sum(Was_expected),
              .groups = 'drop')




  ### Export data for power bi ---------------------------------------------------------------------

  indicators = achieved_results %>%
    rename(Description = Description_new,
           value = count,
           indicator = result_logframe,
           Quarter = Quarter_reported)



  indicators$Implementor = data_projects$Implementor[match(indicators$Project, data_projects$Project_Name)]
  indicators$Country = data_projects$Country[match(indicators$Project, data_projects$Project_Name)]
  indicators$Component = data_projects$Lot[match(indicators$Project, data_projects$Project_Name)]

  refreshed_time <- paste("Last refreshed:", format(Sys.time(), "%d %B %Y at %d %X", usetz = TRUE))


  #export files
  write_rds(refreshed_time, file.path(dir_clean,"refreshed_time.rds"))
  write.csv(indicators, file.path(dir_clean, "indicators.csv"))
  write.csv(reporte_cuenta, file.path(dir_clean, "reporte_cuenta.csv"))
  write_rds(reporte_cuenta, file.path(dir_clean,"reporte_cuenta.rds"))


  #drop skills from reporte to dashboard (it was creating duplicates)
  reporte_clean <- reporte_clean %>%
    filter(!str_detect(Indicator, "Output 1.1|Output 1.2"))


  write.csv(reporte_clean, file.path(dir_clean, "reporte_clean.csv")) #for power bi
  export(reporte_clean, file.path(dir_clean, "reporte_clean.rds")) #for shiny

  #export reports
  return(list(indicators = indicators,
              reporte_cuenta = reporte_cuenta,
              reporte_clean = reporte_clean,
              quarters = quarters,
              refreshed_time = paste("Last refreshed:", format(Sys.time(), "%d %B %Y at %d %X", usetz = TRUE))
  ))



}
