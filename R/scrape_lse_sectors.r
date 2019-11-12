#' Scrape share prices from a particular sector from the London Stock Exchange website.
#'
#' @param url A url from "https://www.londonstockexchange.com".
#' @param pause Indicates how long the function should sleep for before returning a value.
#' @return A dataframe of historical share statistics from a particular sector Use \code{pause} when vectorising this function
#' over a list of urls to minimise nuisance to target website.
# @examples
# scrape_lse_sectors(url="https://www.londonstockexchange.com/exchange/prices-and-markets/stocks/indices/constituents-indices.html?index=&industrySector=451020&page=1")
# scrape_lse_sectors(url="https://www.londonstockexchange.com/exchange/prices-and-markets/stocks/indices/constituents-indices.html?index=&industrySector=451010&page=1")
#' @export



scrape_lse_sectors<-function(url,pause=0){

  print("Scraping started")
  print(url)

  x<-xml2::read_html(url)%>%
    rvest::html_node(".table_dati")%>%
    rvest::html_table()%>%
    .[,1:6]%>%
    tidyr::as_tibble()%>%
    dplyr::mutate(Price= gsub(",","",Price)%>%
             as.numeric(),
           date=Sys.Date(),
           URL=url)


  print("Scraping successful")
  if(pause>0){
    print(paste("Pausing for",pause,"seconds"))
    Sys.sleep(pause)
  }



  return(x)

}


