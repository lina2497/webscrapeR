scrape_ft_company <- function(url="https://markets.ft.com/data/equities/tearsheet/summary?s=KRZ:ISE",pause=0) {
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

  out <- tibble(
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

