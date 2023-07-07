#' oktopost_followers
#'
#' Get a daily count of social followers by profile
#' @param profile_id The social profile ID
#' @param start_date The start date you want to check - defaults to -30 days
#' @param end_date The end date you want to check - defaults to today
#' @export

oktopost_followers <- function(profile_id,start_date = NULL, end_date = NULL) {

  require(httr)
  require(purrr)
  require(tidyverse)

  base_url = "https://api.oktopost.com/v2"
  endpoint <- "followers"

  url <- paste(base_url,endpoint,profile_id,sep='/')

  if (is_empty(start_date) == F) {

    url <- paste(url,'?fromDate=',start_date,'&toDate=',end_date,sep='')

  }

  account_id <- Sys.getenv('OKTOPOST_ACCOUNT_ID')
  access_token <- Sys.getenv('OKTOPOST_ACCESS_TOKEN')

  result <- GET(url,authenticate(account_id,access_token))

  result2 <- content(result,'parsed')

  df <- do.call(bind_rows,map(result2$Data,as_tibble)) %>%
    tidycols() %>%
    mutate(profile_id = profile_id)

  return(df)
}
