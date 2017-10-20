
getAvgValueForGenres <- function(sort = 'desc', size = 10, avgScore = 85){
  quer <- query(paste('{"range": {
                      "overall_score.keyword": {
                      "gte": ',avgScore,'
                      }
}}',sep = ''))

  agg <- aggs(paste('{
    "2": {
                    "terms": {
                    "field": "app_genres.keyword",
                    "size": ',size,',
                    "order": {
                    "1": "',sort,'"
                    }
                    },
                    "aggs": {
                    "1": {
                    "avg": {
                    "field": "app_price"
                    }
                    }
                    }
}
}',sep = ''))
  elastic("http://localhost:9200", "steamgame", "data") %search% (quer + agg)
  }

getCountForYear <- function(avgScore = 0){
  quer <- query(paste('
                      {"bool": {
                      "must": [
                      { "range": { "app_price": { "gt": "',avgScore,'" }}}
                      ],
                      "must_not": [
                      {"match": {"app_year.keyword": "-"}}
                      ]
                      }}',sep = ''))

  agg <- aggs(paste('
                    {
                    "2": {
                    "terms": {
                    "field": "app_year.keyword",
                    "size": 100
                    }}}',sep = ''))
  elastic("http://localhost:9200", "steamgame", "data") %search% (quer + agg)
}

getPricesFromBuckets <- function(avgScore = 0){
  quer <- query(paste('{"range": {
                      "overall_score.keyword": {
                      "gte": ',avgScore,'
  }
  }}',sep = ''))

  agg <- aggs(paste('
                    {"2": {
                      "range": {
                        "field": "app_price",
                        "ranges": [
                        {
                        "from": 5,
                        "to": 10
                        },
                        {
                        "from": 10,
                        "to": 20
                        },
                        {
                        "from": 20,
                        "to": 30
                        },
                        {
                        "from": 30,
                        "to": 40
                        },
                        {
                        "from": 40,
                        "to": 50
                        },
                        {
                        "from": 50,
                        "to": 60
                        },
                        {
                        "from": 60
                        },
                        {
                        "from": 1,
                        "to": 5
                        },
                        {
                        "from": 0,
                        "to": 1
                        }
                        ]
        }
      }
  }',sep = ''))
  elastic("http://localhost:9200", "steamgame", "data") %search% (quer + agg)
}

getQueryResults <- function(nameString = "a",descString = "",boostName = 1,boostDesc = 1){
  quer <- query(paste('{
    "bool": {
      "should": [
        { "match": { "app_name": {"query": "',nameString,'", "boost":',boostName,'}}},
        { "match": { "app_desc": {"query": "',descString,'", "boost":',boostDesc,'}}}
        ]
    }
  } ', sep = ''))
  tryCatch({
    table <- elastic("http://localhost:9200", "steamgame", "data") %search% (quer)
    table[,c('url','app_name','app_price','overall_score',"app_year")]
    }, error=function(cond) {return(data.table())}, warning=function(cond) {
      return(data.table())
    })
}

getPricesForReviewsAndYears <- function(from = 2005, to = 2015){
  print(from)
  print(to)
  quer <- query(paste('
                      {"bool": {
                    "must": [
                    { "range": {           
                      "app_year": {
                        "gte": ',from,',
                        "lte": ',to,'
                      }}}
                    ],
                    "must_not": [
                    {"match": {"app_year.keyword": "-"}}
                    ]
                    }}',sep = ''))

  agg <- aggs(paste('
                    {
                      "3": {
      "terms": {
                  "field": "app_year.keyword",
                  "size": 100,
                  "order": {
                  "_term": "asc"
                  }
                  },
                  "aggs": {
                  "2": {
                  "terms": {
                  "field": "overall_review_summary.keyword",
                  "size": 100,
                  "order": {
                  "1": "asc"
                  }
                  },
                  "aggs": {
                  "1": {
                  "avg": {
                  "field": "app_price"
                  }
                  }
                  }
                  }
                  }
                  }}',sep = ''))
  table <-  elastic("http://localhost:9200", "steamgame", "data") %search% (quer + agg)
  library(data.table)
  t <- as.data.table(data.frame("year" = character(), "key" = character(),"price" = numeric()))
  for (i in 1:nrow(table)) {
    for (j in 1:nrow(table[i,]$`2.buckets`[[1]])) {
      t <- rbind(t, list(table[i,]$key, table[i,]$`2.buckets`[[1]][j,]$key, table[i,]$`2.buckets`[[1]][j,]$`1`$value))
    }
  }
t[is.na(key), key := "NA"]
}

visualizeHeatmapPlot <- function(table){
  table$key <- factor(table$key, levels=c(
    "Overwhelmingly Positive",
    "Very Positive",
    "Positive",
    "Mostly Positive",
    "NA",
    "Mixed",
    "Mostly Negative",
    "Negative",
    "Very Negative",
    "Overwhelmingly Negative"
    ))
  g <- ggplot(table, aes(table$year, table$key))
  g <- g + geom_tile(aes(fill = price),colour = "white") +
    scale_fill_gradient(low = "steelblue",  high = "yellow") + xlab("year") +
    ylab("review") + guides(fill=guide_legend(title="Average\nPrice")) + theme(axis.text.x = element_text(angle = -45))
  g
  }

visualizePiePlot <- function(table){
  plot_ly(table, labels = ~key, values = ~doc_count, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent') %>% layout(showlegend = FALSE)
}

visualizeLinePlot <- function(table){
  g <- ggplot(table)
  g <- g + geom_line(aes(x = table$key,y = table$doc_count, group = 1), stat = "identity") + xlab('Year') + ylab('Number of added games')
  g
}

visualizeBarPlot <- function(table){
  g <- ggplot(table)
  g <- g + geom_bar(aes(reorder(key, `1.value`), `1.value`, fill = key), stat = "identity") + xlab('Genre') + ylab('Price')
  g
}