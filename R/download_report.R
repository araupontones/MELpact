#' Downloads report from zoho
#'
#' Checks when the latest records in downloads was modified and exports
#' records from zoho that have been modified since that date
#'
#' @inheritParams api_get_report
#' @param dir_downloads A directory. Directory to store the download data
#'
#'@result A tibble with all the records of that reported saved as zoho_report.rds in dir_downloads


download_report <- function(dir_downloads,
                            zoho_report,
                            access_token,
                            ...){

  message(glue("Dowloading: {zoho_report} ..."))
  #check if dir_downloads exist ------------------------------------------
  if(!dir.exists(dir_downloads)){

    dir.create(dir_downloads)

  }


  #define exfile ------------------------------------------------------------
  exfile <- file.path(dir_downloads, paste0(zoho_report,".rds"))


  #refresh token -----------------------------------------------------------


  #refresh if it has never been refreshed
  if(!exists("last_token_refreshed")){
    message("Creating token")


    refresh_pact_token(refresh_token = access_token)
  }


  now <- Sys.time() #time now
  time_diff <- lubridate::time_length(now - last_token_refreshed, unit = "seconds")



  #refresh if more thatn 3,580 seconds have passed since the last time it was refreshed
  if(time_diff > 3580){


    refresh_pact_token(refresh_token = access_token)
    message(glue::glue("Token was refrehed {time_diff} seconds ago: Refreshing token!"))

  }

  #Download for the first time if file doesn't exist in dir_download -----------------
  #2. download report
  reporte <- get_report(
    url_app = "https://creator.zoho.com" ,
    account_owner_name = "araupontones" ,
    app_link_name = "uk-pact",
    report_link_name = zoho_report,
    access_token = new_token,
    criteria = "ID != 0",
    from = 1
  )

  rows <- nrow(reporte)
  from <- nrow(reporte)

  while(rows >=200){

    reporte_2 <- get_report(
      url_app = "https://creator.zoho.com" ,
      account_owner_name = "araupontones" ,
      app_link_name = "uk-pact",
      report_link_name = zoho_report,
      access_token = new_token,
      criteria = "ID != 0",
      from = from
    )

    rows <- nrow(reporte_2)
    reporte <- rbind(reporte, reporte_2)

    from <- nrow(reporte) +1

  }


  message(glue("Observations: {from-1} ..."))
  rio::export(reporte, exfile)


}
