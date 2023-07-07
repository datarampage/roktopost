#' oktopost_auth
#'
#' Authenticate your Oktopost API account
#' @param account_id The Oktopost ID
#' @param access_token The Oktopost API token
#' @export

oktopost_auth <- function(account_id,access_token) {

  Sys.setenv('OKTOPOST_ACCOUNT_ID' = account_id)
  Sys.setenv('OKTOPOST_ACCESS_TOKEN' = access_token)

}
