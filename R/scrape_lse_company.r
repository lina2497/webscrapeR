scrape_lse_company<-function(url="https://www.londonstockexchange.com/exchange/prices-and-markets/stocks/summary/company-summary/GB0008847096GBGBXSET1.html?lang=en",
                             pause=0){


  print("Scraping started")
  print(url)
  page<- xml2::read_html(url)

  code_name<-rvest::html_node(page,css=".tesummary")%>%
    rvest::html_text(trim = T)%>%
    strsplit("\r\n")%>%
    unlist()%>%
    set_names("Code","Name")%>%
    as.list()

  code_name$Name<-strsplit(code_name$Name," PLC")%>%
    unlist()%>%
    .[1]

  even<-rvest::html_nodes(page, css=".even")%>%
    rvest::html_children()%>%
    rvest::html_text(trim = T)

  odd<-rvest::html_nodes(page, css=".odd")%>%
    rvest::html_children()%>%
    rvest::html_text(trim = T)

  x<-tolower(c(even,odd))

  Price<-x[grep("price",x)+1]%>%
    gsub(",","",.)%>%
    as.numeric()

  Cur<-x[grep("price",x)]%>%
    strsplit("\\(|\\)")%>%
    unlist()%>%
    .[2]%>%
    toupper()

  Changes<-x[grep("var",x)+1]%>%
    strsplit("%|\r\n|\\)")%>%
    unlist()%>%
    .[c(1,length(.))]%>%
    purrr::set_names("%+/-","+/-")%>%
    as.list()

  out<-tibble(Code = code_name$Code,
              Name = code_name$Name,
              Cur,
              Price,
              `+/-` = as.numeric(Changes$`+/-`),
              `%+/-` = as.numeric(Changes$`%+/-`),
              date = Sys.Date(),
              URL = url
  )



  print("Scraping successful")
  if(pause>0){
  print(paste("Pausing for",pause,"seconds"))
  Sys.sleep(pause)
  }
  return(out)

}


