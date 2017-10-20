library(shinydashboard)
library(plotly)
library(DT)

tabLineChart<- tabItem(tabName = "lineChart",
                      fluidRow(
                        plotlyOutput('lineChart')
                      ), fluidRow(
                        box(sliderInput("lineChartSliderScore",min = 0, max = 100, step = 1, value = 10, label = "Average score higher than")
                      ))
)

tabBarChart <- tabItem(tabName = "barChart",
                       fluidRow(
                         plotlyOutput('barChart')
                       ), fluidRow(
                         box(sliderInput("barChartSliderScore",min = 0, max = 100, step = 1, value = 10, label = "Average score higher than"),
                             sliderInput("barChartSliderNumber",min = 0, max = 25, step = 1, value = 10, label = "Number of genres")
                       ))
)

tabPieChart <- tabItem(tabName = "pieChart",
                       fluidRow(
                         plotlyOutput('pieChart', height = 800)
                       ), fluidRow(
                         box(sliderInput("pieChartSlider",min = 0, max = 100, step = 1, value = 10, label = "Average score higher than"))
                       )
)

tabFindGames <- tabItem(tabName = "findGames",
                       fluidRow(
                          dataTableOutput("gamesOutput")
                       ),
                       fluidRow(
                         box(textInput("gameName", "Search in game name"),
                         textInput("gameDesc", "Search in game desc")),
                         box(sliderInput("gameNameBoostSlider",min = 0, max = 10, step = 1, value = 1, label = "Name boost slider"),
                             sliderInput("gameDescBoostSlider",min = 0, max = 10, step = 1, value = 1, label = "Desc boost slider"))
                       )
)

heatmapChart<- tabItem(tabName = "heatmapChart",
                       fluidRow(
                         plotlyOutput('heatmapChart')
                       ), fluidRow(
                         box(sliderInput("heatmapYear",min = 1983, max = 2018, step = 1, value = c(2005, 2015),dragRange = T, label = "Between years")
                         ))
)
ui <- dashboardPage(
  dashboardHeader(title = "Steam Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Find games", tabName = "findGames", icon = icon("info")),
      menuItem("Average prices per genre", tabName = "barChart", icon = icon("bar-chart")),
      menuItem("Games per year", tabName = "lineChart", icon = icon("line-chart")),
      menuItem("Price buckets", tabName = "pieChart", icon = icon("pie-chart")),
      menuItem("Prices heatmap", tabName = "heatmapChart", icon= icon("map-o"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      tabLineChart,
      tabPieChart,
      tabBarChart,
      tabFindGames,
      heatmapChart
    )
  )
)
