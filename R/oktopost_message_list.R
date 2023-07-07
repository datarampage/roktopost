#' oktopost_message_list
#'
#' Get a list of all of the messages you've passed
#' @param campaign_id The campaign ID - mandatory field
#' @param with_tags If you want to add your tags or not
#' @param network The social network that you want to get messages for. Optional.
#' @export

oktopost_message_list <- function(campaign_id,with_tags = NULL,network = NULL) {

  require(httr)
  require(purrr)
  require(tidyverse)

  #generate the URL

  base_url = "https://api.oktopost.com/v2"
  endpoint <- "message"

  url <- paste(paste(base_url,endpoint,sep='/'),'/?campaignId=',campaign_id,sep='')

  if (is_empty(with_tags) == F) {

    url <- paste(url,'withTags=1',sep='&')

  }

  if (is_empty(network) == F) {

    url <- paste(url,'&network=',network,sep='')

  }

  #get credentials

  account_id <- Sys.getenv('OKTOPOST_ACCOUNT_ID')
  access_token <- Sys.getenv('OKTOPOST_ACCESS_TOKEN')

  #make API call

  result <- GET(url,authenticate(account_id,access_token))

  result2 <- content(result,'parsed')

  #deal with tags

  if (with_tags == T) {

    tags <- tibble()

    result_items <- map(result2$Items,nullToNA)

    for (i in 1:length(result_items)) {

      tags_temp <- tibble(tags = paste(result_items[[i]]$Tags,collapse = ' | '))
      result_items[[i]]$Tags <- NULL

      tags <- bind_rows(tags,tags_temp)

    }

    df <- do.call('bind_rows',map(result_items,as_tibble)) %>%
      bind_cols(tags) %>%
      tidycols()

  } else {

    result_items <- map(result2$Items,nullToNA)

    df <- do.call('bind_rows',map(result2$Items,as_tibble) %>%
      tidycols()

  }

  df[df == ''] <- NA

  return(df)

}
