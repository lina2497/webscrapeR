#' Scrape historical exchange rates from exchangerates.org.uk
#'
#' @param URL A url from ""https://www.exchangerates.org.uk/".
#'
#' @return A dataframe of historical exchange rates for the selected currency.
# @examples
# scrape_exchange_rates(
# URL="https://www.exchangerates.org.uk/GBP-USD-exchange-rate-history.html"
# )
#
# scrape_exchange_rates(
# URL="https://www.exchangerates.org.uk/GBP-CHF-exchange-rate-history.html"
# )
#
# scrape_exchange_rates(
# URL="https://www.exchangerates.org.uk/GBP-CAD-exchange-rate-history.html"
# )
#' @export
#' @importFrom XML htmlTreeParse xpathSApply xmlValue
#' @importFrom xml2 read_html




#function to scrape the historical price of commodities from uk.investing.com
scrape_exchange_rates<-function(URL){



  x<-as.character(xml2::read_html(URL))


  pagetree <- XML::htmlTreeParse(x, error=function(...){}, useInternalNodes = TRUE)


  results <- XML::xpathSApply(pagetree, '//*/table[//*[@id="hd-maintable"]]/tr/td', XML::xmlValue)


  content <- as.data.frame(matrix(results, ncol = 3, byrow = TRUE),stringsAsFactors = FALSE)


  content<-content[-1,-3]


  date<-  as.Date(do.call(rbind, strsplit(content[, 2], " for "))[, 2], "%d/%m/%Y")

  rate_and_unit<-do.call(rbind,strsplit(content[,1]," "))[,4:5]

  out<-data.frame(Date = date, Rate = as.numeric(rate_and_unit[,1]), Currency = as.character(rate_and_unit[,2]))

  return(out)

}




