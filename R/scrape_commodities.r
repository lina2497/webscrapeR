#' Scrape historical commodity prices
#'
#' @param URL A url from "https://uk.investing.com/commodities/".
#'
#' @return A dataframe of historical prices for the selected commodity
#' @examples
#' scrape_commodities(
#' URL="https://uk.investing.com/commodities/gold-historical-data"
#' )
#' scrape_commodities(
#' URL="https://uk.investing.com/commodities/silver-historical-data"
#' )
#' @export
#' @import rvest
#' @import lubridate
#' @import xml2

#function to scrape the historical price of commodities from uk.investing.com



scrape_commodities<-function(URL){

  df<-(xml2::read_html(URL)%>%
         rvest::html_nodes(css = "#curr_table")%>%
         rvest::html_table())[[1]]

  df$Date<-lubridate::parse_date_time(df$Date, orders = c("b", "dmy", "mdy"))

  return(df)

}



