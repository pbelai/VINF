library(elasticsearchr)
library(ggplot2)
library(plotly)
library(data.table)
source("../Logic.R")

server <- function(input, output) {
  output$lineChart <- renderPlotly({
    ggplotly(visualizeLinePlot(getCountForYear(avgScore = input$lineChartSliderScore)))
  })
  
  output$barChart <- renderPlotly({
    ggplotly(visualizeBarPlot(getAvgValueForGenres(size = input$barChartSliderNumber, avgScore = input$barChartSliderScore)))
  })
  
  output$pieChart <- renderPlotly({
    ggplotly(visualizePiePlot(getPricesFromBuckets(input$pieChartSlider)))
  })
  
  output$gamesOutput <- renderDataTable({
    getQueryResults(input$gameName,input$gameDesc, input$gameNameBoostSlider, input$gameDescBoostSlider)
  })
  
  output$heatmapChart <- renderPlotly({
    visualizeHeatmapPlot(getPricesForReviewsAndYears(input$heatmapYear[1], input$heatmapYear[2]))
  })
}