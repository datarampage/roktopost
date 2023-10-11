#' nulltoNA
#'
#' Replace NULL with NA
#' @param x Your value
#' @export


nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

#' tidycols
#'
#' An easy function for cleaning up column names
#' @param df Your data frame
#' @export

tidycols <- function(df) {
  require(snakecase)

  dfnames <- colnames(df)

  dfnames <- gsub('#','nr',dfnames,fixed=TRUE)

  dfnames <- gsub('%','pct',dfnames,fixed=TRUE)

  dfnames <- gsub('Ø','avg',dfnames,fixed=TRUE)

  dfnames <- gsub('$','usd',dfnames,fixed=TRUE)

  dfnames <- gsub('€','eur',dfnames,fixed=TRUE)

  dfnames <- gsub('£','gbp',dfnames,fixed=TRUE)

  dfnames <- to_snake_case(dfnames,sep_out = "_")

  dfnames <- tolower(gsub(" ","_",dfnames))

  dfnames <- gsub(".","_",dfnames,fixed=TRUE)

  dfnames <- gsub("/","_per_",dfnames,fixed=TRUE)

  colnames(df) <- dfnames

  return(df)
}

#' parquet_write
#'
#' Dump a dataframe to a parquet file for loading into Snowflake
#' @param df The dataframe
#' @param file_name The name of the file
#' @param path The filepath
#' @export

parquet_write <- function(df,file_name=NULL,path = '/tmp') {

  require(arrow)
  require(tidyverse)
  require(purrr)

  #generate the filename and path

    options(digits.secs=3)
    # get date + time in UTC
    ts_utc <- as.POSIXlt(Sys.time(), tz = "UTC")
    # format it
    ts_utc_formated <- gsub(".", "", format(ts_utc, format = "%Y%m%d-%H%M%OS3"),fixed=TRUE)
   
    # final filename

    if (is_empty(file_name) == F) {

        filename <- paste(ts_utc_formated,'_',file_name,'.parquet',sep='')
    } else {

        filename <- paste(ts_utc_formated,'.parquet',sep='')

    }

    filepath <- paste(path,filename,sep = '/')

    #save the file to temp storage

    write_parquet(df,sink = filepath)

}