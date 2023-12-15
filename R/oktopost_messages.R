#' oktopost_messages
#'
#' Get information about an individual message
#' @param message_id The post ID
#' @export
#'

oktopost_messages <- function(message_id) {

  require(httr)
  require(snakecase)
  require(tidyverse)

  base_url = "https://api.oktopost.com/v2"
  endpoint <- "message"

  url <- paste(paste(base_url,endpoint,message_id,sep='/'),'withTags=1',sep='?')

  account_id <- Sys.getenv('OKTOPOST_ACCOUNT_ID')
  access_token <- Sys.getenv('OKTOPOST_ACCESS_TOKEN')

  result <- GET(url,authenticate(account_id,access_token))

  result2 <- content(result,'parsed')

  message <- result2$Message

  message$Media <- NULL
  message$AltTexts <- NULL
  message$Tags <- NULL

  message <- as_tibble(nullToNA(message))

  names(message) <- to_snake_case(names(message))

  return(message)

}
