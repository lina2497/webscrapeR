#' Scrape share prices from a particular company from the financial times.
#'
#' @param url A url from "https://markets.ft.com/data/equities/tearsheet/".
#' @param pause Indicates how long the function should sleep for before returning a value.
#' @return A dataframe of historical share statistics from a particular company. Use \code{pause} when vectorising this function
#' over a list of urls to minimise nuisance to target website.
# @examples
# scrape_ft_company(url="https://markets.ft.com/data/equities/tearsheet/summary?s=KRZ:ISE")
# scrape_ft_company(url="https://markets.ft.com/data/equities/tearsheet/summary?s=BA:NYQ")
# scrape_ft_company(url="https://markets.ft.com/data/equities/tearsheet/summary?s=WBA:NSQ", pause=2)
#' @export
#' @importFrom purrr map map_chr set_names
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table html_text html_children
#' @importFrom tidyr tibble
#' @importFrom dplyr "%>%"




scrape_ft_company <- function(url,pause=0) {
  print("Scraping started")
  print(url)

  page <- xml2::read_html(url)

  Name <-
    rvest::html_nodes(page, css = ".mod-tearsheet-overview__header__name--large") %>%
    rvest::html_text()

  Code <- rvest::html_nodes(page, css = ".mod-ui-symbol-chain__trigger") %>%
    rvest::html_text()


  quote_table <-
    rvest::html_nodes(page, css = ".mod-tearsheet-overview__quote__bar") %>%
    rvest::html_children() %>%
    purrr::map(rvest::html_children) %>%
    purrr::map(rvest::html_text) %>%
    purrr::set_names(purrr::map_chr(.,  ~ .[1])) %>%
    purrr::map( ~ .[2])

  Cur <- names(quote_table[1]) %>%
    strsplit("\\(|\\)") %>%
    unlist() %>%
    .[2]

  Price <- quote_table[[1]] %>%
    gsub(",", "", .) %>%
    as.numeric()

  changes <- quote_table$`Today's Change` %>%
    strsplit(" / |%") %>%
    purrr::map(as.numeric) %>%
    unlist()

  out <-tidyr::tibble(
    Code,
    Name,
    Cur,
    Price,
    `+/-` = changes[1],
    `%+/-` = changes[2],
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

