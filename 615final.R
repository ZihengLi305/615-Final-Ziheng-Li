#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(readr)


# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Ireland: A Comprehensive Overview"),
  sidebarLayout(
    sidebarPanel(
      h4("Explore Ireland"),
      helpText("This app provides an in-depth overview of Ireland, including maps, key facts, projections, and comparisons."),
      selectInput(
        "info", 
        "Select Information to View:", 
        choices = c(
          "Map of Ireland", 
          "World Location", 
          "Key Facts", 
          "Projection of Key Variable", 
          "Comparison with Other Island States", 
          "SWOT Analysis"
        ), 
        selected = "Map of Ireland"
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.info == 'Map of Ireland'",
        leafletOutput("irelandMap", height = "600px")
      ),
      conditionalPanel(
        condition = "input.info == 'World Location'",
        leafletOutput("worldMap", height = "600px")
      ),
      conditionalPanel(
        condition = "input.info == 'Key Facts'",
        htmlOutput("keyFacts")
      ),
      conditionalPanel(
        condition = "input.info == 'Projection of Key Variable'",
        plotOutput("projectionPlot")
      ),
      conditionalPanel(
        condition = "input.info == 'Comparison with Other Island States'",
        tableOutput("comparisonTable"),
        plotOutput("rainfallProjection"),
        htmlOutput("comparisonLink")
      ),
      conditionalPanel(
        condition = "input.info == 'SWOT Analysis'",
        htmlOutput("swotAnalysis")
      )
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output, session) {
  # Map of Ireland
  output$irelandMap <- renderLeaflet({
    ireland_bounds <- list(
      lng1 = -10.5, lat1 = 51.5,
      lng2 = -5.5, lat2 = 55.5
    )
    
    leaflet() %>%
      addTiles() %>%
      fitBounds(
        lng1 = ireland_bounds$lng1, lat1 = ireland_bounds$lat1,
        lng2 = ireland_bounds$lng2, lat2 = ireland_bounds$lat2
      )
  })
  
  # World location map
  output$worldMap <- renderLeaflet({
    # 定义爱尔兰的大致边界
    ireland_bounds <- list(
      lng1 = -10.5, lat1 = 51.5, # 西南角
      lng2 = -5.5, lat2 = 55.5  # 东北角
    )
    
    leaflet() %>%
      addTiles() %>%
      # 添加矩形覆盖爱尔兰区域
      addRectangles(
        lng1 = ireland_bounds$lng1, lat1 = ireland_bounds$lat1,
        lng2 = ireland_bounds$lng2, lat2 = ireland_bounds$lat2,
        color = "blue", # 边框颜色
        fillColor = "blue", # 填充颜色
        fillOpacity = 0.3, # 填充透明度
        weight = 2 # 边框线条粗细
      ) %>%
      # 将视图聚焦到爱尔兰
      fitBounds(
        lng1 = ireland_bounds$lng1, lat1 = ireland_bounds$lat1,
        lng2 = ireland_bounds$lng2, lat2 = ireland_bounds$lat2
      )
  })
  
  
  # Key facts about Ireland
  output$keyFacts <- renderUI({
    HTML(
      "<h3>Key Facts About Ireland</h3>",
      "<ul>",
      "<li><strong>Government:</strong> The Government of Ireland is the executive authority of the Republic of Ireland, headed by the Taoiseach, the head of government. The government – also known as the cabinet – is composed of ministers, each of whom must be a member of the Oireachtas, which consists of Dáil Éireann and Seanad Éireann. Most ministers have a portfolio of specific responsibilities such as departments or policy areas, although ministers without portfolio can be appointed.</li>",
      "<li><strong>Population:</strong> The population of Ireland as of December 11, 2024, is estimated to be 5,278,577 according to Worldometer's analysis of the latest United Nations data. The mid-year estimate was slightly lower at 5,255,017 people. Ireland makes up 0.06% of the total world population and ranks 124th in terms of population size globally. The population density is 76 people per square kilometer, and the country covers a total land area of 68,890 square kilometers. Urban residents constitute 62% of the population, totaling 3,257,857 people. The median age in Ireland is 38.7 years.</li>",
      "</ul>"
    )
  })
  
  # Projection of key variable (example: GDP growth)
  values <- reactiveValues(data = NULL, forecast = NULL)
  
  observe({
    if (input$info == "Projection of Key Variable") {
      original_data <- read.table("armaghdata march21.txt", header = FALSE, sep = "", fill = TRUE, col.names = c("Year", "Month", "Tmax", "Tmin", "Af", "Rain", "Sun"), skip = 7, stringsAsFactors = FALSE)
      original_data$Year <- as.numeric(as.character(original_data$Year))
      original_data$Rain <- as.numeric(as.character(original_data$Rain))
      original_data <- na.omit(original_data)
      
      original_data$FiveYearPeriod <- cut(original_data$Year, breaks = seq(1925, 2025, by = 5), labels = seq(1929, 2024, by = 5), include.lowest = TRUE)
      five_year_data <- aggregate(Rain ~ FiveYearPeriod, data = original_data, FUN = mean)
      values$data <- five_year_data
      
      model <- lm(Rain ~ as.numeric(FiveYearPeriod), data = five_year_data)
      future_periods <- data.frame(FiveYearPeriod = as.character(seq(2029, 2034, by = 5)))
      predictions <- predict(model, newdata = future_periods)
      future_periods$Rain <- predictions
      values$forecast <- future_periods
    }
  })
  
  output$projectionPlot <- renderPlot({
    req(values$data, values$forecast)
    combined_data <- rbind(values$data, values$forecast)
    combined_data$FiveYearPeriod <- as.numeric(as.character(combined_data$FiveYearPeriod))
    
    ggplot(combined_data, aes(x = FiveYearPeriod, y = Rain)) +
      geom_point(color = "red") +
      geom_line(color = "steelblue") +
      labs(title = "Rainfall Projection by Five-Year Periods", x = "Five-Year Period", y = "Average Rain (mm)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Comparison with other island states
  # Reactive values to store data and forecasts
  # Load and prepare the data
  output$rainfallProjection <- renderPlot({
    # Load data from CSV
    file_path <- "CambridgeMonthlyWeather.csv"
    weather_data <- read_csv(file_path)
    
    # Clean data
    data_clean <- weather_data %>%
      filter(!grepl("^year", tolower(year))) %>%
      mutate(
        year = as.numeric(year),
        rain = as.numeric(rain)
      ) %>%
      na.omit()
    
    # Create five-year periods and calculate mean rain
    data_clean$FiveYearPeriod2 <- cut(
      data_clean$year, 
      breaks = seq(1925, 2025, by = 5), 
      labels = seq(1929, 2024, by = 5), 
      include.lowest = TRUE
    )
    five_year_data2 <- aggregate(rain ~ FiveYearPeriod2, data = data_clean, FUN = mean)
    
    # Build a linear model to predict rain based on FiveYearPeriod
    model <- lm(rain ~ as.numeric(FiveYearPeriod2), data = five_year_data2)
    
    # Prepare future periods for predictions
    future_periods2 <- data.frame(FiveYearPeriod2 = seq(2029, 2034, by = 5))
    future_periods2$FiveYearPeriod2 <- as.numeric(as.character(future_periods2$FiveYearPeriod2))
    predictions <- predict(model, newdata = future_periods2)
    future_periods2$rain <- predictions
    
    # Combine historical and forecast data
    combined_data2 <- rbind(five_year_data2, future_periods2)
    
    # Plot the data
    ggplot(combined_data2, aes(x = as.numeric(as.character(FiveYearPeriod2)), y = rain)) +
      geom_point(color = "red") +
      geom_line(color = "steelblue") +
      labs(
        title = "Rainfall Projection by Five-Year Periods", 
        x = "Five-Year Period", 
        y = "Average Rainfall (mm)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$comparisonLink <- renderUI({
    HTML(
      "<p>For additional insights, visit <a href='https://www.coface.com/news-economy-and-insights/business-risk-dashboard/compare-country'>Compare Countries Dashboard</a>.</p>"
    )
  })
  
  
  
  # SWOT analysis
  output$swotAnalysis <- renderUI({
    HTML(
      "<h3>SWOT Analysis of Ireland</h3>",
      "<h4>Strengths</h4>",
      "<ul><li>Flexible labour and goods markets.</li><li>Favourable business environment, attractive taxation.</li><li>Presence of multinational companies, particularly from the United States, which account for 22% of employment and 63% of value-added in the non-financial business sector.</li><li>Presence (through multinationals) in sectors with high value-added, including pharmaceuticals, IT and medical equipment.</li></ul>",
      "<h4>Weaknesses</h4>",
      "<ul><li>Dependent on the economic situation and tax regimes of the United States and Europe, particularly the United Kingdom.</li><li>Vulnerable to changes in the strategies of foreign companies.</li><li>Private corporate debt levels still high.</li><li>Banking sector still vulnerable to shocks.</li></ul>",
      "<h4>Opportunities</h4>",
      "<ul><li>Presence in pharmaceuticals, IT and medical devices will remain strong in the coming year.</li><li>Interest rate cuts by the Fed and ECB will also improve investment by multinationals in 2024.</li></ul>",
      "<h4>Threats</h4>",
      "<ul><li>Ireland business insolvency rate rises sharply.</li><li>Underbuilding and rising house prices.</li></ul>",
      "<p>For more details, visit <a href='https://www.coface.com/news-economy-and-insights/business-risk-dashboard/country-risk-files/ireland' >Ireland Country Risk Profile</a>.</p>"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

