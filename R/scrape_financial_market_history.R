#' Scrape historical values for different financial markets
#'
#' @param url A url from "https://www.sharesmagazine.co.uk/indices/index/".
#'
#' @return A dataframe of historical values for the selected market.
#@examples
# scrape_financial_market_history(
#   url="https://www.sharesmagazine.co.uk/indices/index/IXIC/historic-prices"
# )
#
# scrape_financial_market_history(
#   url="https://www.sharesmagazine.co.uk/indices/index/UKX/historic-prices"
# )
#
# scrape_financial_market_history(
#   url="https://www.sharesmagazine.co.uk/indices/index/MCX/historic-prices"
# )
#' @export
#'
#' @importFrom purrr map_df
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @importFrom dplyr mutate
#' @importFrom stringi stri_replace



##function to scrape historical prices from financial markets
scrape_financial_market_history<-function(url){

  market<-xml2::read_html(url)%>%
  rvest::html_node(css=".footable-table")%>%
  rvest::html_table()%>%
  purrr::map_df(~stringi::stri_replace(.x,fixed = ",",replacement = ""))%>%
  dplyr::mutate(Date = lubridate::mdy(Date),
         Close = as.numeric(Close),
         High = as.numeric(High),
         Low = as.numeric(Low),
         Open = as.numeric(Open),
         Time=Sys.time())

return(market)

}





