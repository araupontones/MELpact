#' Gets report from Zoho based on criteria
#'
#' @param zoho_report A string. Report link name as it is defined in Zoho
#' @param access_token A string. The refresh token that belongs to the requesting user,
#' which is returned when the access_type=offline is included in the query string of the authorization request
#' See \href{https://www.zoho.com/creator/help/api/v2/refresh-the-access-token.html}{Refresh the Access Token}
#' @return A tibble. Tibble format of the requested report

api_get_report <- function(zoho_report,
                           new_token,
                           criteria

){
  report <- zohor::get_report(
    url_app = "https://creator.zoho.com" ,
    account_owner_name = "araupontones" ,
    app_link_name = "uk-pact",
    report_link_name = zoho_report,
    access_token = new_token,
    criteria = criteria
  )

  return(report)


}
