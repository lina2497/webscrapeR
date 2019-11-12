#' Scrape supermarket prices from mysupermarket.co.uk
#'
#' @param url A search recult url from "https://www.mysupermarket.co.uk/".
#' @param pause Indicates how long the function should sleep for before returning a value.
#' @return A dataframe of prices, pack sizes, and product ids. Use \code{pause} when vectorising this function
#' over a list of urls to minimise nuisance to target website.
#' @examples
#' scrape_mysupermarket_prices(url="https://www.mysupermarket.co.uk/Shopping/FindProducts.aspx?Query=cadburys%20chocolate")
#' scrape_mysupermarket_prices(url="https://www.mysupermarket.co.uk/Shopping/FindProducts.aspx?query=baked+beans&store=Tesco")
#' scrape_mysupermarket_prices(url="https://www.mysupermarket.co.uk/Shopping/FindProducts.aspx?query=ice+cream&store=Tesco&_fcategory=Ice_Lollies")
#' @export
#' @import rvest
#' @import dplyr
#' @import xml2
#' @import tidyr
#' @import purrr



##Function for scraping price data from mysupermarket.co.uk
scrape_mysupermarket_prices<-function(url,pause=0){

  ###### function to extract supermarket data from a list of URLs e.g multiple pages ###################################
  extract_from_list<-function(Scraped){
    #Parse download using CSS labels and functions from rvest
    Images <- Scraped %>%
      purrr::map(~rvest::html_nodes(.x,"#ProductImage"))

    Product_ID<- Images %>%
      purrr::map(~rvest::html_attr(.x,"src"))

    Full_Name <-Images %>%
      purrr::map(~rvest::html_attr(.x,"alt"))

    store_and_product<-Scraped %>%
      purrr::map(~rvest::html_text(rvest::html_nodes(.x,".Prefix")))

    price <- Scraped %>%
      purrr::map(~rvest::html_text(rvest::html_nodes(.x,".Price")))

    price_per_unit <-Scraped %>%
      purrr::map(~rvest::html_text(rvest::html_nodes(.x,"#PPU")))

    suffix <- Scraped %>%
      purrr::map(~rvest::html_text(rvest::html_nodes(.x,".Suffix")))

    #Tidy parsed data from internet into columns of equal length and combine into a dataframe

    Product_ID<-unlist(Product_ID)[!is.na(unlist(Product_ID))]
    Product_ID<-do.call(rbind,strsplit(do.call(rbind,strsplit(Product_ID,"/"))[,7],".jpg"))[,1]

    Full_Name<-unlist(Full_Name)[!is.na(unlist(Full_Name))]


    product<- unlist(store_and_product)[unlist(store_and_product)!=""]

    price <-unlist(price)

    price_per_unit <-unlist(price_per_unit)

    out<-data.frame(Product_ID,
                    Full_Name,
                    product,
                    price,
                    price_per_unit,
                    stringsAsFactors = F)

    return(out)

  }

  #### function to extract supermarkt date from a single URL e.g. if the results don't span several pages #################

  extract_from_page<-function(Scraped){
    #Parse download using CSS labels and functions from rvest
    Images <- Scraped %>%
      rvest::html_nodes("#ProductImage")

    Product_ID<- Images %>%
      rvest::html_attr("src")

    Full_Name <-Images %>%
      rvest::html_attr("alt")

    store_and_product<-Scraped %>%
      rvest::html_nodes(".Prefix")%>%
      rvest::html_text()

    price <- Scraped %>%
      rvest::html_nodes(".Price")%>%
      rvest::html_text()

    price_per_unit <-Scraped %>%
      rvest::html_nodes("#PPU")%>%
      rvest::html_text()

    #Tidy parsed data from internet into columns of equal length and combine into a dataframe

    Product_ID<-unlist(Product_ID)[!is.na(unlist(Product_ID))]
    Product_ID<-do.call(rbind,strsplit(do.call(rbind,strsplit(Product_ID,"/"))[,7],".jpg"))[,1]

    Full_Name<-unlist(Full_Name)[!is.na(unlist(Full_Name))]


    product<- unlist(store_and_product)[unlist(store_and_product)!=""]

    price <-unlist(price)

    price_per_unit <-unlist(price_per_unit)

    out<-data.frame(Product_ID,
                    Full_Name,
                    product,
                    price,
                    price_per_unit,
                    stringsAsFactors = F
    )

    return(out)

  }

  tidy_prices<-function(out){


    pence<-grep("p",out$price)#locate prices that have pence rather than pounds
    Price<-as.numeric(gsub("\\\u00A3|p|\r\n|\\ ","",out$price))#remove pound symbols, pence and other artifacts
    Price[pence]<-Price[pence]/100#convert pence to pounds
    Out_tidy<-out#Store result in dataframe
    Out_tidy$price<-Price #Store result in dataframe

    ######Wrangle price per unit from currency to number###########################################################################
    PPU<-strsplit(as.character(Out_tidy$price_per_unit)," / ")#seperate price per unit from offer price (stored in same column)

    Offer_Price<-rep(NA,length(PPU))#create vector or NA equal in lenght to price per unit
    Offer_Price[purrr::map_int(PPU,length)>2]<-do.call(rbind,PPU[purrr::map_int(PPU,length)>2])[,2]#extract offer price from price per unit
    Offer_Price_Pence<-grep("p",Offer_Price)#locate prices in pence
    Offer_Price_tidy<-as.numeric(gsub("\\\u00A3|p","",Offer_Price))#remove pence and pounds symbols
    Offer_Price_tidy[Offer_Price_Pence]<-Offer_Price_tidy[Offer_Price_Pence]/100#convert pence to pounds

    Units<-unlist(PPU)[cumsum(purrr::map(PPU,length))]#seperate unts from price
    PPU_out<-as.character(lapply(PPU, `[[`, 1))#take just prices and store them
    PPU_pence<-grep("p",PPU_out)#locate pence
    PPU_tidy<-as.numeric(gsub("\\\u00A3|p","",PPU_out))#remove pound symbols and pence
    PPU_tidy[PPU_pence]<-PPU_tidy[PPU_pence]/100#convert pence to pounds

    PPU_out<-data.frame(Price_per_unit=PPU_tidy,Unit=as.character(Units), stringsAsFactors = F)#store output in data frame

    Suffix<-strsplit(as.character(Out_tidy$Full_Name),"\\(")
    Units<-purrr::map(Suffix,length)==2
    size<-rep("1",nrow(Out_tidy))
    size[Units]<-gsub("\\)","",do.call(rbind,Suffix[Units])[,2])

    Out_tidy$Product_ID<-gsub("w_74.png","NA",Out_tidy$Product_ID)#remove erronious product IDs

    TidyPrices <-bind_cols(product_ID = Out_tidy$Product_ID,
                           full_name = Out_tidy$Full_Name,
                           category = Out_tidy$category,
                           item = Out_tidy$product,
                           retailer = Out_tidy$store,
                           shelf_Price = Out_tidy$price,
                           price_per_unit=PPU_out$Price_per_unit,
                           offer_price_per_unit=Offer_Price_tidy,
                           unit=PPU_out$Unit,
                           size_quant = size)

    return(TidyPrices)

  }


  x <-error_proof(xml2::read_html(url))#try to read url


  y <-
    error_proof(rvest::html_nodes(x, xpath = '//*[@id="PagerText"]'))#get number of pages

  if (length(y) == 0) {
    #if no results
    print("no items found")  #print the number of results to the console
    # urls$results[i] <- 0#store the number of results
    pages<-0
  }

  if (length(y) > 0) {
    #if there are some results

    items <- as.numeric(strsplit(rvest::html_text(y), " ")[[1]][3])
    print(paste(items,"items found"))
    pages <-round(items / 41) + 1#This looks random but mysupermarket show 41 items per page by default so dividing items by 41 gives page count
    #print(paste(pages,"pages"))
  }


  if (pages > 1) {
    xpages <-
      paste0(url, "&page=", c(1:pages))#then generate the required urls to scrape all pages
    xlist <- error_proof(purrr::map(xpages, read_html))#read the pages
    xlist[[pages + 1]] <- x#store them in a list
    out <- error_proof(extract_from_list(xlist))#extract the data
  }



  if (pages == 1) {
      #if there is only one page of results
      out <- error_proof(extract_from_page(x))#extract that page
  }

  print("Scraping successful")
  if(pause>0){
    print(paste("Pausing for",pause,"seconds"))
    Sys.sleep(pause)
  }


  return(out%>%
           tidy_prices()%>%
           mutate(date=Sys.Date(),
                  url=url)%>%
           unique()
           )
}


