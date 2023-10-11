#' oktopost_post_analytics
#'
#' Get information about an individual post
#' @param post_id The post ID
#' @export

oktopost_post_analytics <- function(post_id) {

  require(httr)
  require(purrr)
  require(tidyverse)

  base_url = "https://api.oktopost.com/v2"
  endpoint <- "analytics"

  url <- paste(base_url,endpoint,post_id,sep='/')

  account_id <- Sys.getenv('OKTOPOST_ACCOUNT_ID')
  access_token <- Sys.getenv('OKTOPOST_ACCESS_TOKEN')

  result <- GET(url,authenticate(account_id,access_token))

  result2 <- content(result,'parsed')

  if (is_empty(result2$Stats$EngagementsNetworkSpecific) == F) {

    network_df <- as_tibble(result2$Stats$EngagementsNetworkSpecific)

  }

  result2$Stats$EngagementsNetworkSpecific <- NULL

  df <- as_tibble(result2$Stats) %>%
    bind_cols(network_df) %>%
    tidycols() %>%
    mutate(post_id = post_id) %>%
    select(post_id, everything())

  return(df)

}

#' oktopost_post_analytics_list
#'
#' Get information about many posts
#' @param post_ids The post IDs
#' @param verbose Add confirmation that each post_id has been processed
#' @export

oktopost_post_analytics_list <- function(post_ids,verbose = F) {

  require(tidyverse)

  analytics_df <- tibble()

  for (i in 1:length(post_ids)) {

    temp_df <- oktopost_post_analytics(post_ids[[i]])

    analytics_df <- bind_rows(analytics_df,temp_df)

    if (verbose == T) {

      print(paste(i,': ',all_post_ids[[i]],' processed',sep=''))

    }


  }

  return(analytics_df)


}

#' oktopost_social_post
#'
#' Get information about a social post
#' @param post_id The post ID
#' @export

oktopost_social_post <- function(post_id) {

  require(httr)
  require(purrr)
  require(tidyverse)

  base_url = "https://api.oktopost.com/v2"
  endpoint <- "postlog"

  stats_url <- paste(paste(base_url,endpoint,sep='/'),'?postId=',post_id,sep='')

  account_id <- Sys.getenv('OKTOPOST_ACCOUNT_ID')
  access_token <- Sys.getenv('OKTOPOST_ACCESS_TOKEN')

  result <- GET(stats_url,authenticate(account_id,access_token))

  result2 <- content(result,'parsed')

  df <- do.call('bind_rows',map(result2$Postlogs,as_tibble)) %>%
    tidycols() %>%
    select(-link_ids) %>%
    distinct()

  return(df)

}

#' oktopost_social_post_analytics
#'
#' Get more detailed information about a social post
#' @param social_post_id The social post ID
#' @export

oktopost_social_post_analytics <- function(social_post_id) {

  require(httr)
  require(purrr)
  require(tidyverse)

  base_url = "https://api.oktopost.com/v2"
  endpoint <- "postlog"

  stats_url <- paste(paste(base_url,endpoint,social_post_id,sep='/'),'?stats=1',sep='')

  account_id <- Sys.getenv('OKTOPOST_ACCOUNT_ID')
  access_token <- Sys.getenv('OKTOPOST_ACCESS_TOKEN')

  result <- GET(stats_url,authenticate(account_id,access_token))

  result2 <- content(result,'parsed')

  result2$Postlog$LinkIds <- NULL

  postlog_df <- as_tibble(result2$Postlog)

  stats_df <- as_tibble(flatten(result2$Stats))

  df <- bind_cols(postlog_df,stats_df) %>%
    tidycols()

  return(df)
}

#' oktopost_social_posts_all
#'
#' Get all of the social posts in a single DF
#' @param post_df The output of the oktopost_posts_list function
#' @export

oktopost_social_posts_all <- function(post_df) {

  require(tidyverse)
  require(purrr)

  social_posts <- tibble()

  post_ids <- post_df %>%
    pull(id)

  for (i in 1:length(post_ids)) {

    social_temp <- oktopost_social_post(post_ids[[i]])

    social_posts <- bind_rows(social_posts,social_temp)

    print(paste(i,': ',post_ids[[i]],' processed',sep=''))

  }

  return(social_posts)

}

#' oktopost_social_analytics_all
#'
#' Get all of the social analytics posts in a single DF
#' @param social_df The output of the oktopost_social_post function (or oktopost_social_post_all)
#' @export

oktopost_social_analytics_all <- function(social_df) {

  require(tidyverse)
  require(purrr)

  social_analytics <- tibble()

  social_post_ids <- social_df %>%
    pull(id)

  for (i in 1:length(social_post_ids)) {

    analytics_temp <- oktopost_social_post_analytics(social_post_ids[[i]])

    social_analytics <- bind_rows(social_analytics,analytics_temp)

    print(paste(i,': ',social_post_ids[[i]],' processed',sep=''))

  }

  return(social_analytics)

}
