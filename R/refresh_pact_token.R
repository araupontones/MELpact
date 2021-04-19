#' Refreshes the token for zoho uk-pact
#'
#' @param refresh_token A string. Token that allows refreshing the access_token


refresh_pact_token <- function(refresh_token){
new_token <- zohor::refresh_token(
  base_url = "https://accounts.zoho.com",
  client_id = "1000.V0FA571ML6VV7YFWRC4Q7OKQ32U5PZ",
  client_secret = "c551969c7d49a7a945ac2da12d1a3fe5f241b8dae6",
  refresh_token = refresh_token
)

#return(new_token)

}

