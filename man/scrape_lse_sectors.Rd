% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrape_lse_sectors.r
\name{scrape_lse_sectors}
\alias{scrape_lse_sectors}
\title{Scrape share prices from a particular sector from the London Stock Exchange website.}
\usage{
scrape_lse_sectors(url, pause = 0)
}
\arguments{
\item{url}{A url from "https://www.londonstockexchange.com".}

\item{pause}{Indicates how long the function should sleep for before returning a value.}
}
\value{
A dataframe of historical share statistics from a particular sector Use \code{pause} when vectorising this function
over a list of urls to minimise nuisance to target website.
}
\description{
Scrape share prices from a particular sector from the London Stock Exchange website.
}
