##function to scrape historical prices from financial markets
scrape_financial_market_history<-function(url="https://www.sharesmagazine.co.uk/indices/index/UKX/historic-prices"){

  market<-read_html(url)%>%
  html_node(css=".footable-table")%>%
  html_table()%>%
  map_df(~stringi::stri_replace(.x,fixed = ",",replacement = ""))%>%
  mutate(Date = lubridate::mdy(Date),
         Close = as.numeric(Close),
         High = as.numeric(High),
         Low = as.numeric(Low),
         Open = as.numeric(Open),
         Time=Sys.time())

return(market)

}



