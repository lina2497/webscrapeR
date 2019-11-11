
##Function to scrape historical exchange rates from exchangerates.org


#library(dplyr)
scrape_exchange_rates<-function(URL="https://www.exchangerates.org.uk/GBP-EUR-exchange-rate-history.html"){

  x<-xml2::read_html(URL)%>%as.character()


  pagetree <- XML::htmlTreeParse(x, error=function(...){}, useInternalNodes = TRUE)


  results <- XML::xpathSApply(pagetree, '//*/table[//*[@id="hd-maintable"]]/tr/td', xmlValue)


  content <- as.data.frame(matrix(results, ncol = 3, byrow = TRUE),stringsAsFactors = FALSE)


  content<-content[-1,-3]


  date<-  as.Date(do.call(rbind, strsplit(content[, 2], " for "))[, 2], "%d/%m/%Y")

  rate_and_unit<-do.call(rbind,strsplit(content[,1]," "))[,4:5]

  out<-data.frame(Date = date, Rate = as.numeric(rate_and_unit[,1]), Currency = as.character(rate_and_unit[,2]))

  return(out)

}




