


baseSelector <- 'body .home_page_body_ctn '

visited <- list()
urlsToVisit <- urlsToVisit[!urlsToVisit %in% visited]

scrape <- function() {
  library(rvest)
  
  url <- 'http://store.steampowered.com/'
  
  
  
  getAllGenres <- function() {
    selectorFilters <- '.home_page_gutter_block'
    genres <- getAllUrlsfromWithinElement(url, selectorFilters, 2)
    genres[grepl('/tag/', genres)]
  }
  
  showAllFromGenre <- function(url) {
    selectorShowAll <- '.btnv6_blue_hoverfade.btn_small_thin'
    showAllLink <- getUrlOfElement(url, selectorShowAll)
    showAllLink
  }
  
  getUrlOfElement <- function(url, selector, numberOfElement = -1) {
    webPage <- read_html(url) #get html
    filtersUrls <-
      html_nodes(webPage, selector) #get nodes
    if (numberOfElement != -1) {
      filtersUrls <- filtersUrls[numberOfElement]
    }
    urlsToVisit <- html_attr(filtersUrls, "href")
    urlsToVisit
  }
  
  getAllUrlsfromWithinElement <-
    function(url, selector, numberOfElement = -1) {
      hrefSelectors <- '[href*="store.steampowered.com"], [href^="/"]'
      webPage <- read_html(url) #get html
      webFilters <- html_nodes(webPage, selector) #get body
      if (numberOfElement == -1) {
        filtersUrls <-
          html_nodes(webFilters, hrefSelectors) #get nodes
      } else {
        filtersUrls <-
          html_nodes(webFilters[numberOfElement], hrefSelectors) #get nodes
      }
      urlsToVisit <- unique(html_attr(filtersUrls, "href"))
      urlsToVisit
    }
  
  listOfAllInGenres <- list()
  for (genre in getAllGenres()) {
    listOfAllInGenres <-
      append(unlist(listOfAllInGenres), showAllFromGenre(genre))
  }
  
  gameUrls <- list()
  i <- 1
  print(listOfAllInGenres)
  while (length(gameUrls) < 1000) {
    for (genrePage in listOfAllInGenres) {
      print(genrePage)
      elements <-
        getUrlOfElement(genrePage,
                        '#search_result_container > div> .search_result_row')
      gameUrls <-
        append(unlist(gameUrls), elements[!elements %in% gameUrls])
    }
    pattern <- paste('page', i, sep = '=')
    i <- i + 1
    replacement <- paste('page', i, sep = '=')
    listOfAllInGenres <-
      sub(pattern, replacement, listOfAllInGenres)
    if (i %% 10 == 0) {
      print(paste('Current number of games is ', length(gameUrls)))
    }
  }
  gameUrls
}
gameUrls <- scrape()

# getUrls <- function(urlToVisit) {
#   visited <<- append(visited, urlToVisit)
#   if (grepl("^/", urlToVisit)) {
#     urlToVisit <- paste(url, urlToVisit, sep = "")
#   }
#   try({
#     webPage <- read_html(urlToVisit) #get html
#     webBody <- html_nodes(webPage, baseSelector) #get body
#     bodyUrls <- html_attr(html_nodes(webBody, hrefSelectors),
#                           "href") #get nodes
#     if (grepl("^/", urlToVisit)) {
#       urlToVisit <- paste(url, urlToVisit, sep = "")
#     }
#     bodyUrls <- unique(bodyUrls[!bodyUrls %in% visited])
#     for (urlToVisit in bodyUrls) {
#
#       print(paste(length(visited), urlToVisit, sep = ' | '))
#       if (length(visited) > 500)
#         return
#       getUrls(urlToVisit)
#     }
#     uniqueBodyUrlsNew <- unique(html_attr(bodyUrls, "href"))
#     urlsToVisit <-
#       unique(append(urlsToVisit, uniqueBodyUrlsNew[!uniqueBodyUrlsNew %in% visited]))
#   })
# }
getUrls(url)