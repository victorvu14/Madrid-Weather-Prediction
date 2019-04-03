###### FUNCTIONS #######

# There’s 72 csv files, each one containing a piece of hourly pollution data from Madrid, in the period 2011 to 2016.
# Specifically, each csv contains the raw data for a concrete month of a concrete year (6 years, 12 months per year).
# There are 2 more separates dataset: 
   ## Weather.xlsx, containing daily time series for min, average, and max temperature, precipitations, humidity and wind in Madrid.
   ## parameters.png (image), containing the key for every pollutant code (eg. “08” - NO2)

# Our task is to: Reading every piece of raw data and creating the whole initial raw_data set. 
    ## Processing raw_data to create a daily dataset, by averaging each hourly measure, and containing also the weather variables
    ##  and the names for each pollutant parameter. 
    ## Create ShinyApp to analyze the interactions of different weather variables 


readAllHourlyDataCSVs <- function(years,months) {
  ## Initialize empty data frame to store all data
  hourlyPollutants <- data.frame();
  ## Loop through every year and month passed in to function
  for(year in years) {
    for(month in months) {
      ## Get monthly data using helper function
      monthlyData <- readHourlyDataCSV(year,month)
      ## Merge info 
      hourlyPollutants <- rbind(hourlyPollutants, monthlyData )
    }
  }
  return(hourlyPollutants)
}


readHourlyDataCSV <- function(year, month) {
  ## Get filename of CSV file for this year/month
  fileName <- paste0('hourly_data_', year, '_', month, '.csv')
  ## Load CSV for this year/month
  csvContents <- read.csv(fileName)
  ## Transform necessary variables into factors for simpler calculations
  csvContents$station <- as.factor(csvContents$station)
  csvContents$parameter <- as.factor(csvContents$parameter)
  ## Add date column and remove day column
  csvContents$date <- as.Date(paste0('20', year, '-', month, '-', csvContents$day))
  csvContents$day <- NULL
  return(csvContents)
}

getWeatherData <- function() {
  weatherData <- read_excel("weather.xlsx")
  weatherData <- as.data.frame(weatherData)
  weatherData$date <- as.Date(weatherData$date)
  return(weatherData)
}

getPollutantNames <- function() {
  matrix <- matrix(data = c('1', 'SO2', 
                            '6', 'CO',
                            '7', 'NO',
                            '8', 'NO2',
                            '9', 'PM2.5',
                            '10', 'PM10',
                            '12', 'NOx',
                            '14', 'O3',
                            '20', 'TOL',
                            '30', 'BEN',
                            '35', 'EBE',
                            '37', 'MXY',
                            '38', 'PXY',
                            '39', 'OXY',
                            '42', 'TCH',
                            '43', 'CH4',
                            '44', 'NMHC'), nrow=17, ncol = 2,byrow=TRUE)
  pollutantFrame <- as.data.frame(matrix)
  colnames(pollutantFrame) <- c('parameter', 'formula')
  return(pollutantFrame)
}


numberOfDaysInMonth <- function(month, year) {
  date <- as.Date(paste0('20', year, '-', month, '-01'))
  while (as.integer(format(date, format="%m")) == month) {
    date <- date + 1
  }
  return(as.integer(format(date - 1, format="%d")))
}

getMonthlyAverages <- function(dataFrame) {
  dataFrame$month = format(dataFrame$date, "%m")
  dataFrame$year = format(dataFrame$date, "%Y")
  aggregatedDataFrame = aggregate(dataFrame[,!colnames(dataFrame)%in%c('year','month','date')], FUN=mean,by=list(month=dataFrame$month, year=dataFrame$year))
  aggregatedDataFrame$date = as.Date(paste0(aggregatedDataFrame$year, '-', aggregatedDataFrame$month, '-01'))
  return(aggregatedDataFrame)
}

runShinyApp <- function(formulaOptions, dailyAverageDataSet, monthlyAvgDataSet, startDateValue, endDateValue, correlationMatrix) {
  ui <- fluidPage(
    
    titlePanel("Pollutants and Weather Measures in Madrid"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput('aggregation',
                    'Monthly/Daily:', 
                    c('Daily', 'Monthly'), selected='Monthly'),
        dateInput("start_date",
                  "Start date:",
                  min = startDateValue,
                  max = endDateValue,
                  value = startDateValue),
        dateInput("end_date",
                  "End Date:",
                  min = startDateValue,
                  max = endDateValue,
                  value = endDateValue),
        selectInput('measure',
                    'Measure:', 
                    formulaOptions, multiple = T, selected='NO2')
      ),
      
      mainPanel(
        dygraphOutput("weatherPollutantPlot"),
        d3heatmapOutput("heatmap", width = "100%", height="600px")
      )
    )
  )
  
  server <- function(input, output) {
    output$heatmap <- renderD3heatmap({d3heatmap(correlationMatrix)})
    output$weatherPollutantPlot <- renderDygraph({
      
      if(input$aggregation == 'Daily') {
        start <- input$start_date
        end   <- input$end_date
        orderme <- seq(as.Date(start), as.Date(end), by=1)
        data <- dailyAverageDataSet
        data <- data[data$date>=start&data$date<=end,c(input$measure)]
        data <- xts(data, orderme)
      } else if (input$aggregation == 'Monthly') {
        startDate <- input$start_date
        startMonth <- format(as.Date(startDate), '%m')
        startYear <- format(as.Date(startDate), '%Y')
        realStartDate <- as.Date(paste0(startYear, '-', startMonth, '-01'))
        endDate <- input$end_date
        endMonth <- format(as.Date(endDate), '%m')
        endYear <- format(as.Date(endDate), '%Y')
        realEndDate <- as.Date(paste0(endYear, '-', endMonth, '-01'))
        
        orderme <- seq(realStartDate, realEndDate, by='months')
        data <- monthlyAvgDataSet
        data <- data[data$date>=realStartDate&data$date<=realEndDate,c(input$measure)]
        data <- xts(data, orderme)
      }
      dygraphs::dygraph(data)%>%dyRangeSelector()
    })
  }  
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}

mapShinyApp <- function(yearOptions, pollutantOptions, yearlyAveragesPollutantByStation) {
  ui <- fluidPage(
    titlePanel("Pollutants Throughout Stations in Madrid"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput('year',
                    'Year:', 
                    yearOptions, selected=yearOptions),
        selectInput('measure',
                    'Measure:', 
                    pollutantOptions, selected='NO2')
      ),
      
      mainPanel(
        leafletOutput("mymap")
      )
    )
  )
  
  server <- function(input, output, session) {
    data <- reactive({
      x <- as.data.frame(yearlyAveragesPollutantByStation[yearlyAveragesPollutantByStation$year==input$year&yearlyAveragesPollutantByStation$formula==input$measure,])
    })
    
    rescale <- function(values) {
      v <- ((values-min(values))/(max(values)-min(values)))*1000
      return(v)
    }
    output$mymap <- renderLeaflet({
      test <- data()
      m <- leaflet(data=test) %>%
        addTiles() %>%
        addCircles(lng =~lon, lat =~lat, radius = ~rescale(mean_value), popup =~paste0(name, ' Value: ', round(mean_value,2)))
      m #has to rescale as some pollutant is too small or too large
    })
  }  
  shinyApp(ui = ui, server = server)
}

