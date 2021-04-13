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

  #check if dir_downloads exist ------------------------------------------
  if(!dir.exists(dir_downloads)){

    dir.create(dir_downloads)

  }

  #define exfile ------------------------------------------------------------
  exfile <- file.path(dir_downloads, paste0(zoho_report,".rds"))


  #refresh token -----------------------------------------------------------

  refresh_if_needed <- function(){
    new_token <- refresh_pact_token(refresh_token = access_token) #create new token
    assign("new_token", new_token, envir = globalenv()) ##save token in global environment
    assign("last_token_refreshed", Sys.time(), envir = globalenv()) #last time token was generated
  }

  #refresh if it has never been refreshed
  if(!exists("last_token_refreshed")){
    message("Creating token")

    refresh_if_needed()

  }


  now <- Sys.time() #time now
  time_diff <- lubridate::time_length(now - last_token_refreshed, unit = "seconds")



  #refresh if more thatn 3,580 seconds have passed since the last time it was refreshed
  if(time_diff > 3580){

    refresh_if_needed()
    message(glue::glue("Token was refrehed {time_diff} seconds ago: Refreshing token!"))

  }

  #Download for the first time if file doesn't exist in dir_download -----------------
  if(!file.exists(exfile)){

    #get report from zoho
    reporte <- api_get_report(zoho_report = zoho_report,
                              criteria = "ID!=0",
                              new_token = new_token)


    #export report to downloads
    rio::export(reporte, exfile)
    message(glue::glue("Creating {zoho_report} in {dir_downloads}"))


  } else {

    #IN CASE REPORT EXISTS IN DOWNLOADS, get last modified time of report ---------------
    reference_data <- rio::import(exfile)
    modified_time <- criteria_time(reference_data)

    #download latest modified records
    download_data <- api_get_report(zoho_report = zoho_report,
                                    criteria = modified_time,
                                    new_token = new_token)

    #if at least one record has been modified since the last time
    if(!is.null(download_data)){

      #drop from reference records that have been updated
      old_records <- reference_data %>%
        anti_join(download_data, by=c("ID"))

      ##append data
      reference_appended <- plyr::rbind.fill(old_records, download_data)

      #export
      rio::export(reference_appended, exfile)
      message(glue::glue("{zoho_report} has been updated in {dir_downloads}"))

    } else {


      message(glue::glue("{zoho_report} has not been modified in Zoho since {modified_time}"))
    }



  }


}
