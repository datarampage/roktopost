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
