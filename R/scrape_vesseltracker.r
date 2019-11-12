#' Scrape ship data from "https://www.vesseltracker.com/"
#'
#' @param url A search recult url from "https://www.vesseltracker.com/"
#' @param pause Indicates how long the function should sleep for before returning a value.
#' @return A list containing two dataframes, PortStatus contians general information about the port, PortCalls contains a list of recent arrivals. Use \code{pause} when vectorising this function
#' over a list of urls to minimise nuisance to target website.
#' @examples
#' scrape_vesseltracker(url="https://www.vesseltracker.com/en/Port/Dover/Dashboard.html")
#' scrape_vesseltracker(url="https://www.vesseltracker.com/en/Port/Calais/Dashboard.html")
#' scrape_vesseltracker(url="https://www.vesseltracker.com/en/Port/Belfast/Dashboard.html")
#' @export
#' @importFrom tidyr fill

#function for scraping port data
scrape_vesseltracker<-function(url,pause=0)
{
  x<-xml2::read_html(url)

  PortStatus<-rvest::html_nodes(x,css=".key-value-table")%>%
    rvest::html_children()%>%
    rvest::html_text()%>%
    strsplit(":")%>%
    do.call(rbind, .)%>%
    t()%>%
    data.frame(stringsAsFactors = F)%>%
    purrr::set_names(.[1,]%>%
                unlist()%>%
                as.character())%>%
    .[-1,]%>%
    dplyr::mutate(time_collected=Sys.time(),
           url=url)%>%
    dplyr::select(-`Local time`)


  PortCalls<-rvest::html_nodes(x,css=".data-table")%>%
    rvest::html_children()%>%
    purrr::map(rvest::html_children)%>%
    purrr::map(~rvest::html_text(.x,trim = T))%>%
    do.call(rbind,.)%>%
    .[,-1]%>%
    data.frame(stringsAsFactors = F)

  PortCalls$table<-NA
  PortCalls$table[PortCalls$X1=="Name"]<-PortCalls$X3[PortCalls$X1=="Name"]
  PortCalls<-tidyr::fill(PortCalls,table)

  PortCalls[PortCalls$X1!="Name",]%>%
    purrr::set_names(c("Name","Type","Time","Status"))%>%
    dplyr::mutate(time_collected=Sys.time(),
           url=url)->PortCalls
  print("Scraping successful")
  if(pause>0){
    print(paste("Pausing for",pause,"seconds"))
    Sys.sleep(pause)
  }
  return(list(Status=PortStatus,
              Calls=PortCalls))

}


