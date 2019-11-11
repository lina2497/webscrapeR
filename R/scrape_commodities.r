
#function to scrape the historical price of commodities from uk.investing.com
scrape_commodities<-function(URL="https://uk.investing.com/commodities/crude-oil-historical-data"){

  df<-(xml2::read_html(URL)%>%
         rvest::html_nodes(css = "#curr_table")%>%
         rvest::html_table())[[1]]

  df$Date<-lubridate::parse_date_time(df$Date, orders = c("b", "dmy", "mdy"))

  return(df)

}



