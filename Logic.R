
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
  table <- elastic("http://localhost:9200", "steamgame", "data") %search% (quer)
  table[,c('url','app_name','app_price','overall_score',"app_year")]
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