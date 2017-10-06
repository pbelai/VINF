scrape <- function(numberOfGames = 1000, fromInternet = T, fromFile = T) {
  library(rvest)
  library(stringr)
  library(data.table)
  url <- 'http://store.steampowered.com/'
  gameUrls <<- list()
  if (fromInternet && !fromFile){
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
    sub('[?]snr(.*)','',urlsToVisit)
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
  
  
  i <- 1
  while (length(gameUrls) < numberOfGames) {
    for (genrePage in listOfAllInGenres) {
      print(genrePage)
      elements <-
        getUrlOfElement(genrePage,
                        '#search_result_container > div> .search_result_row')
      gameUrls <<-
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
  } else if (fromFile) {
    gameUrls <<-fread('urls', sep = ';', header = F)$V1
  }
  else {
    gameUrls <<- paste('pages/',list.files('pages/'), sep = '')
  }
  createDataTable <- function() {
    data.table(
      'URL' = character(),
      'APP_NAME' = character(),
      'APP_DESC' = character(),
      'APP_PRICE' = numeric(),
      'RECENT_NUMBER_OF_REVIEWS' = numeric(),
      'RECENT_REVIEW_SUMMARY' = character(),
      'RECENT_SCORE' = numeric(),
      'OVERALL_NUMBER_OF_REVIEWS' = numeric(),
      'OVERALL_REVIEW_SUMMARY' = character(),
      'OVERALL_SCORE' = numeric(),
      'APP_TAGS' = list(),
      'APP_GENRES' = list(),
      'APP_DEVELOPER' = character(),
      'APP_PUBLISHER' = character(),
      'APP_YEAR' = numeric()
    )
  }
  
  getAppDetails <- function(webPage, url) {
    gameRow <- createDataTable()
    
    getAppName <- function(webPage) {
      appName <-
        html_text(html_nodes(webPage, '.page_content_ctn .apphub_AppName'),
                  trim = T) #appname
      if (length(appName)==0) {return('-')}
      appName
    }
    
    getAppDesc <- function(webPage) {
      appDesc <-
        sub("About This Game\r\n\t\t\t\t\t\t\t","",html_text(html_nodes(webPage, '.page_content_ctn #game_area_description'),
                        trim = T)) #desc
      if (length(appDesc)==0) {return('-')}
      appDesc
      
    }
    
    getAppPrice <- function(webPage) {
      appPrice <-
        html_text(html_node(webPage, '.game_purchase_price.price'),
                  trim = T)
      if (is.na(appPrice)) {
        appPrice <-
          html_text(html_node(webPage, '.discount_final_price'),
                    trim = T)
      }
      if (grepl('Free', appPrice))
        return(0)
      appPrice <-
        if (is.na(str_extract(appPrice, '[0-9]+,[0-9]+')))
          str_extract(appPrice, '[0-9]+,')
      else
        str_extract(appPrice, '[0-9]+,[0-9]+')
      price <- as.numeric(sub(',', '.', appPrice))
      if (length(price)==0) {return(0)}
      price
    }
    
    getAppGenres <- function(webPage) {
      genres <- as.list(html_text(html_nodes(webPage,'.details_block a[href*=genre]')))
      if (length(genres)==0) {return('-')}
      genres
    }
    
    getAppDeveloper <- function(webPage) {
      developers <- as.list(html_text(html_nodes(webPage,'.details_block a[href*=developer]')))
      if (length(developers)==0) {return('-')}
      developers
    }
    
    getAppPublisher <- function(webPage) {
      publishers <- as.list(html_text(html_nodes(webPage,'.details_block a[href*=publisher]')))
      if (length(publishers)==0) {return('-')}
      publishers
    }
    
    getAppReleaseDate <- function(webPage) {
      releaseDate <- str_extract(html_text(html_node(webPage,'.date')),'[0-9]{4}')
      if (is.na(releaseDate) || length(releaseDate)==0) {return('-')}
      releaseDate
    }
    
    getAppReviewsScores <- function(webPage) {
      webNodes <-
        html_nodes(webPage, '.user_reviews .user_reviews_summary_row')
      reviewData <-
        data.table(
          'RECENT_NUMBER_OF_REVIEWS' = numeric(),
          'RECENT_REVIEW_SUMMARY' = character(),
          'RECENT_SCORE' = numeric(),
          'OVERALL_NUMBER_OF_REVIEWS' = numeric(),
          'OVERALL_REVIEW_SUMMARY' = character(),
          'OVERALL_SCORE' = numeric()
        )
      RECENT_NUMBER_OF_REVIEWS <- -1
      RECENT_REVIEW_SUMMARY <- 'NA'
      RECENT_SCORE <- -1
      OVERALL_NUMBER_OF_REVIEWS <- -1
      OVERALL_REVIEW_SUMMARY <- 'NA'
      OVERALL_SCORE <- -1
      for (node in webNodes) {
        if (html_text(html_nodes(node, '.subtitle '), trim = T) == 'Recent Reviews:') {
          # get number of reviews
          if (!is.na(html_text(html_node(node, '.responsive_hidden'), trim = T))) {
            RECENT_NUMBER_OF_REVIEWS <-
              paste(str_extract_all(html_text(
                html_node(node, '.responsive_hidden'), trim = T
              ), '[0-9]+')[[1]],
              collapse = '')
          }
          if (!is.na(html_text(html_node(node, '.game_review_summary'), trim = T))) {
            # get review word
            if (!grepl('not_enough_reviews',
                       html_attr(html_node(
                         node, '.game_review_summary'
                       ), 'class'))) {
              RECENT_REVIEW_SUMMARY <-
                html_text(html_node(node, '.game_review_summary'), trim = T)
            }
          }
          if (!is.na(html_text(html_node(
            node, '.responsive_reviewdesc'
          )))) {
            # get all percentage
            score <- sub('%', '', str_extract(html_text(
              html_node(node, '.responsive_reviewdesc'), trim = T
            ), '[0-9]+%'))
            RECENT_SCORE <- if (is.na(score))
              - 1
            else
              score
          }
        } else if (html_text(html_nodes(node, '.subtitle '), trim = T) == 'All Reviews:') {
          if (!is.na(html_text(html_node(node, '.responsive_hidden'), trim = T))) {
            # get number of reviews
            OVERALL_NUMBER_OF_REVIEWS <-
              paste(str_extract_all(html_text(
                html_node(node, '.responsive_hidden'), trim = T
              ), '[0-9]+')[[1]],
              collapse = '')
          }
          if (!is.na(html_text(html_node(node, '.game_review_summary'), trim = T))) {
            # get review word
            if (!grepl('not_enough_reviews',
                       html_attr(html_node(
                         node, '.game_review_summary'
                       ), 'class'))) {
              OVERALL_REVIEW_SUMMARY <-
                html_text(html_node(node, '.game_review_summary'), trim = T)
            }
          }
          if (!is.na(html_text(html_node(
            node, '.responsive_reviewdesc'
          )))) {
            # get all percentage
            score <- sub('%', '', str_extract(html_text(
              html_node(node, '.responsive_reviewdesc'), trim = T
            ), '[0-9]+%'))
            OVERALL_SCORE <- if (is.na(score))
              - 1
            else
              score
          }
        }
      }
      rbindlist(
        list(
          reviewData,
          data.table(
            'RECENT_NUMBER_OF_REVIEWS' = RECENT_NUMBER_OF_REVIEWS,
            'RECENT_REVIEW_SUMMARY' = RECENT_REVIEW_SUMMARY,
            'RECENT_SCORE' = RECENT_SCORE,
            'OVERALL_NUMBER_OF_REVIEWS' = OVERALL_NUMBER_OF_REVIEWS,
            'OVERALL_REVIEW_SUMMARY' = OVERALL_REVIEW_SUMMARY,
            'OVERALL_SCORE' = OVERALL_SCORE
          )
        ),
        use.names = T,
        fill = F,
        idcol = F
      )
    }
    
    getAppTags <- function(webPage) {
      webNodes <- html_nodes(webPage, '.game_area_details_specs, .app_tag:not(.add_button)')
      tags <- list()
      for (node in webNodes) {
        tags <- append(unlist(tags), html_text(node, trim = T))
      }
      if (length(tags)==0) {return('-')}
      unlist(tags)
    }
    
    reviewsScore <- getAppReviewsScores(webPage)

    rbindlist(
      list(
        gameRow,
        data.table(
          'URL' = url,
          'APP_NAME' = getAppName(webPage),
          'APP_DESC' = getAppDesc(webPage),
          'APP_PRICE' = getAppPrice(webPage),
          'RECENT_NUMBER_OF_REVIEWS' = reviewsScore$RECENT_NUMBER_OF_REVIEWS,
          'RECENT_REVIEW_SUMMARY' = reviewsScore$RECENT_REVIEW_SUMMARY,
          'RECENT_SCORE' = reviewsScore$RECENT_SCORE,
          'OVERALL_NUMBER_OF_REVIEWS' = reviewsScore$OVERALL_NUMBER_OF_REVIEWS,
          'OVERALL_REVIEW_SUMMARY' = reviewsScore$OVERALL_REVIEW_SUMMARY,
          'OVERALL_SCORE' = reviewsScore$OVERALL_SCORE,
          'APP_TAGS' = list(getAppTags(webPage)),
          'APP_GENRES' = list(getAppGenres(webPage)),
          'APP_DEVELOPER' = getAppDeveloper(webPage),
          'APP_PUBLISHER' = getAppPublisher(webPage),
          'APP_YEAR' = getAppReleaseDate(webPage)
          )
      ),
      use.names = T,
      fill = F,
      idcol = F
    )
  }
  
  gamesDetails <- createDataTable()
  
  for (gameUrl in gameUrls) {
    if (fromInternet || fromFile) {
      webPage <- html_session(gameUrl,httr::set_cookies('birthtime'='283993201', 'mature_content'= '1')) %>% read_html()
    } else {
      webPage <- read_html(gameUrl)
    }
    a <<- webPage
    write_xml(webPage,paste('pages/',sub('/','',sub('app/','',str_extract(gameUrl,'app/[0-9]*/'))), sep = ""))
    
    if (is.na(html_node(webPage, '#agecheck_form')) && is.na(html_node(webPage, '#app_agegate'))) {
      print(paste('downloading:',gameUrl))
      
      {
        
        gamesDetails <- rbindlist(
          list(gamesDetails,
               getAppDetails(webPage, gameUrl)),
          use.names = T,
          fill = F,
          idcol = F
        )
      }
    } else {
      print(paste('age restriction in:',gameUrl))
    }
    if (nrow(gamesDetails) >= numberOfGames) {
      break;
    }
  }
  gamesDetails
}
a <- NULL
test <- scrape(40000,F,T)
