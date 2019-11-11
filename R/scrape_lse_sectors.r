

scrape_lse_sectors<-function(url="https://www.londonstockexchange.com/exchange/prices-and-markets/stocks/indices/constituents-indices.html?index=&industrySector=451020&page=1"){

  print("Scraping started")
  print(url)

  x<-read_html(url)%>%
    html_node(".table_dati")%>%
    html_table()%>%
    .[,1:6]%>%
    as_tibble()%>%
    mutate(Price= gsub(",","",Price)%>%
             as.numeric(),
           date=Sys.Date(),
           URL=url)

  pause <- round(runif(1, 3, 9), 2)

  print("Scraping successful")
  print(paste("Pausing for", pause, "seconds"))

  Sys.sleep(pause)


  return(x)

}


