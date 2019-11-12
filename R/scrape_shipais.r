#' Scrape ship data from "http://www.shipais.com/"
#'
#' @param url A search recult url from "http://www.shipais.com/"
#' @param pause Indicates how long the function should sleep for before returning a value.
#' @return A dataframe of ship name, speed, status, destination, ETA and details. Use \code{pause} when vectorising this function
#' over a list of urls to minimise nuisance to target website.
#' @examples
#' scrape_shipais(url="http://www.shipais.com/currentmap.php?map=Channel")
#' scrape_shipais(url="http://www.shipais.com/index.php?map=IrishSea")
#' scrape_shipais(url="http://www.shipais.com/index.php?map=uk")
#' @export
#' @import rvest
#' @import dplyr
#' @import xml2
#' @import tidyr
#' @import purrr



scrape_shipais<-function(url,pause=0){
  x<-xml2::read_html(url)

  im<-rvest::html_node(x,css="#im")


  Ship_tables<-rvest::html_children(im)%>%
    rvest::html_children()%>%
    rvest::html_children()%>%
    rvest::html_children()%>%
    rvest::html_table()%>%
    purrr::map_df(~.x%>%
             t()%>%
             as.data.frame(stringsAsFactors=F,row.names = F)%>%
               setNames(.[1,])%>%
             .[-1,])

  print("Scraping successful")
  if(pause>0){
    print(paste("Pausing for",pause,"seconds"))
    Sys.sleep(pause)
  }


  return(Ship_tables%>%
           dplyr::mutate(time=Sys.time(),
                         url=url))

}



