#' Define criteria to dowload records modified since the last download
#'
#' Gets the last time a record was modified in the existing data and
#' creates the criteria to donwload records after that time. The search criteria
#' is based on: \href{https://www.zoho.com/creator/help/api/v2/get-records.html}{Defining the search criteria}
#' @param reference_data A tible. Last download of the report
#' @return A string to be passed to the query that downloads the reports from zoho. "Modified_Time>'10-Apr-2021 11:37:16'"


criteria_time <- function(reference_data){
  #get the time for the last modification in the reference
  modified_time = reference_data[["Modified_Time"]]
  #sort based on date and get the latest
  reference_data[["sorted"]] <- clock::date_time_parse(modified_time, zone = "UTC", format = "%d-%b-%Y %H:%M:%S")
  last_modified_time<- last(reference_data[["Modified_Time"]], order_by =  reference_data[["sorted"]])

  criteria_to_export = glue::glue("Modified_Time>='{last_modified_time}'")

  return(criteria_to_export)
}
