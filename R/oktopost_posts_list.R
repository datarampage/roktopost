#' oktopost_posts_list
#'
#' Get a list of all of your posts
#' @param message_id The message ID
#' @param campaign_id The campaign ID
#' @param status The post status
#' @param created_by Who created the post
#' @param source The social source
#' @param before Start date
#' @param after End date
#' @export

oktopost_posts_list <- function(message_id=NULL,campaign_id = NULL,status = NULL,created_by = NULL,source = NULL,before = NULL,after = NULL) {

  require(httr)
  require(purrr)
  require(tidyverse)

  base_url = "https://api.oktopost.com/v2"
  endpoint <- "post"

  url <- paste(paste(base_url,endpoint,sep='/'),'?_count=100',sep='')

  #add all possible parameters to query

  if (is_empty(message_id) == F) {

    url <- paste(url,'&messageId=',message_id,sep='')

  }

  if (is_empty(campaign_id) == F) {

    url <- paste(url,'&campaignId=',campaign_id,sep='')

  }

  if (is_empty(status) == F) {

    url <- paste(url,'&status=',status,sep='')

  }

  if (is_empty(created_by) == F) {

    url <- paste(url,'&createdBy=',created_by,sep='')

  }

  if (is_empty(source) == F) {

    url <- paste(url,'&source=',campaign_id,sep='')

  }

  if (is_empty(before) == F) {

    url <- paste(url,'&before=',before,sep='')

  }

  if (is_empty(after) == F) {

    url <- paste(url,'&after=',after,sep='')

  }

  #get credentials

  account_id <- Sys.getenv('OKTOPOST_ACCOUNT_ID')
  access_token <- Sys.getenv('OKTOPOST_ACCESS_TOKEN')

  #make API call

  result <- GET(url,authenticate(account_id,access_token))

  result2 <- content(result,'parsed')

  #create initial df

  items <- map(result2$Items,nullToNA)

  df <- do.call(bind_rows,map(items,as_tibble))

  total <- result2$Total

  total_pages <- seq(1,ceiling(total / 100) -1,by=1)

  if (total > 100) {

    print(paste(total,'posts to retrieve'))

    for (i in 1:length(total_pages)) {

      new_url <- paste(url,'&_page=',i,sep='')

      new_result <- GET(new_url,authenticate(account_id,access_token))

      temp_result2 <- content(new_result,'parsed')

      temp_items <- map(temp_result2$Items,nullToNA)

      temp_df <- do.call(bind_rows,map(temp_items,as_tibble))

      df <- bind_rows(df,temp_df)

      print(paste('Page ',i, ' processed',sep=''))

    }


  }

  df <- df %>%
    tidycols()

  return(df)


}
