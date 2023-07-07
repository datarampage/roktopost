#' oktopost_campaign_list
#'
#' Get a list of all of your campaigns
#' @export

oktopost_campaign_list <- function() {

  require(httr)
  require(purrr)
  require(tidyverse)

  base_url = "https://api.oktopost.com/v2"
  endpoint <- "campaign"

  url <- paste(base_url,endpoint,sep='/')

  account_id <- Sys.getenv('OKTOPOST_ACCOUNT_ID')
  access_token <- Sys.getenv('OKTOPOST_ACCESS_TOKEN')

  result <- GET(url,authenticate(account_id,access_token))

  result2 <- content(result,'parsed')

  df <- do.call(bind_rows,map(result2$Items,as_tibble)) %>%
    tidycols()

  return(df)

}
