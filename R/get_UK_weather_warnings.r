#' Parse the met office RSS feed for weather warnings into a tidy data frame.
#' "http://www.metoffice.gov.uk/public/data/PWSCache/WarningsRSS/Region/UK"
#'

#' @return Returns a tidy dataframe of all UK weather warnings disaggregated by region..
#' @examples
#' get_UK_weather_warnings()
#' @export
#' @import tidyr
#' @import dplyr
#' @import xml2
#' @import purrr



get_UK_weather_warnings<-function(){
  rss_parse <- function(doc){

    safe_xml_find_first <- purrr::safely(xml2::xml_find_first)
    safe_xml_find_all <- purrr::safely(xml2::xml_find_all)
    safe_run <- function(res) {
      if (is.null(res$error)) {
        ret <- res$result
      } else {
        ret <- xml2::read_xml("<p></p>")
      }
      return(ret)
    }


    channel <- xml2::xml_find_all(doc, "channel")

    if(identical(length(channel), 0L)){
      if(any(names(xml2::xml_ns(doc)) == "d1")){
        ns <- xml2::xml_ns_rename(xml2::xml_ns(doc), d1 = "rss")
      } else{
        ns <- xml2::xml_ns(doc)
      }

      channel <- xml2::xml_find_all(doc, "rss:channel", ns = ns)
      site <- xml2::xml_find_all(doc, "rss:item", ns = ns)

      categories <- function(item){
        xx <- xml2::xml_find_all(item, "rss:category", ns = ns) %>% xml2::xml_text()
        if(length(xx) < 1){
          return(FALSE)
        } else {
          return(TRUE)
        }
      }

      res <- suppressWarnings({tibble(
        feed_description = safe_xml_find_first(channel,
                                               "rss:description", ns = ns) %>%
          safe_run() %>%
          xml2::xml_text(),
        item_description = safe_xml_find_first(site,
                                               "rss:description", ns = ns) %>%
          safe_run() %>%
          xml2::xml_text()
      )})

      if(categories(site) == TRUE) {
        res$item_categories <- safe_xml_find_all(
          site, "rss:category/..",
          ns = ns
        ) %>%
          safe_run()
      }
    } else{

      site <- xml2::xml_find_all(channel, "item")

      res <- suppressWarnings({
        tibble(
          feed_description = safe_xml_find_first(channel, "description") %>%
            safe_run() %>%
            xml2::xml_text(),
          item_description = safe_xml_find_first(site, "description") %>%
            safe_run() %>%
            xml2::xml_text()
        )})


      return(res)
    }
  }

  url<-"http://www.metoffice.gov.uk/public/data/PWSCache/WarningsRSS/Region/UK"


  xml_file<-xml2::read_xml(url)
  rss_parse(xml_file)%>%
    dplyr::select(feed_description,item_description)%>%
    dplyr::mutate(timestamp=Sys.time())%>%
    tidyr::separate(item_description,into=c("region","areas"),sep=" : ")%>%
    tidyr::separate(areas,into=c("areas","valid_from"), sep=" valid from ")%>%
    tidyr::separate(valid_from,into=c("valid_from","valid_to"), sep=" to ")%>%
    dplyr::mutate(url=url)->parsed

  return(parsed)

}
