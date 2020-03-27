# First load the relevan libraries
library(plotly)
library(data.table)
library(tidyr)
library(leaflet)
library(shiny)
library(forecast)


# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Load the data that we need
  timeseriesconfirmed <- read.csv("time_series_covid19_confirmed_global.csv")
  timeseriesdead <- read.csv("time_series_covid19_deaths_global.csv")
  timeseriesrecovered <- read.csv("time_series_covid19_recovered_global.csv")
  
  # Get the global totals
  addtotal <- function(df){
    sums <- colSums(df[,5:ncol(df)])
    sums <- as.integer(sums)
    levels(df[,1]) <- c(levels(df[,1]),"Total")
    levels(df[,2]) <- c(levels(df[,2]),"Total")
    totalsums <- data.frame(Province.State = "Total", Country.Region = "Total", Lat = 30.97560, Long = 112.270700,t(colSums(df[,5:ncol(df)])))
    df[nrow(df)+1,] <- totalsums
    return(df)}
  
  timeseriesconfirmed <- addtotal(timeseriesconfirmed)
  timeseriesdead <- addtotal(timeseriesdead)
  timeseriesrecovered <- addtotal(timeseriesrecovered)
  
  data <- reactive({
    # We will need to calculate totals for the input given
    country <- as.character(input$country)
    
    subsetdata <- timeseriesconfirmed[timeseriesconfirmed$Country.Region == country,]
    totalconfirmed <- as.data.frame(colSums(subsetdata[,5:ncol(subsetdata)]))
    subsetdata <- timeseriesdead[timeseriesdead$Country.Region == country,]
    totaldead <- as.data.frame(colSums(subsetdata[,5:ncol(subsetdata)]))
    subsetdata <- timeseriesrecovered[timeseriesrecovered$Country.Region == country,]
    totalrecovered <- as.data.frame(colSums(subsetdata[,5:ncol(subsetdata)]))
    
    total <- cbind(totalconfirmed,totaldead,totalrecovered)
    colnames(total) <- c("Confirmed", "Dead", "Recovered")
    rownames(total) <- 1:nrow(total)
    setDT(total, keep.rownames = TRUE)[]
    totalgathered <- total
    totalgathered <- gather(total,"Status", "Amount",-rn)
    totalgathered$Status <- as.factor(totalgathered$Status)
    data <- totalgathered
      })
  
  fit <- reactive({
    country <- as.character(input$country)
    
    subsetdata <- timeseriesconfirmed[timeseriesconfirmed$Country.Region == country,]
    totalconfirmed <- as.data.frame(colSums(subsetdata[,5:ncol(subsetdata)]))
    subsetdata <- timeseriesdead[timeseriesdead$Country.Region == country,]
    totaldead <- as.data.frame(colSums(subsetdata[,5:ncol(subsetdata)]))
    subsetdata <- timeseriesrecovered[timeseriesrecovered$Country.Region == country,]
    totalrecovered <- as.data.frame(colSums(subsetdata[,5:ncol(subsetdata)]))
    total <- cbind(totalconfirmed,totaldead,totalrecovered)
    colnames(total) <- c("Confirmed", "Dead", "Recovered")
    
    if (input$status == "Confirmed"){timeseries <- ts(totalconfirmed)}
    if (input$status == "Dead"){timeseries <- ts(totaldead)}
    if (input$status == "Recovered"){timeseries <- ts(totalrecovered)}
    
    fit <- ets(timeseries)
    
  })
  
  # Fill in the spot we created for a plot
  output$CountryPlot <- renderPlotly({
  plot_ly(data(), y = data()$Amount, color = ~Status, type = "scatter", mode = "lines") %>% layout(
      title = as.character(input$country), xaxis = list(title = "Days Since January 22nd"),
      yaxis = list(title = "Count"))
  })
  output$Forecast <- renderPlot({
    plot(forecast(fit(), 10), type = "l", main = c("Forecast",as.character(input$status)), ylab = "Cases", xlab = "Days Since January 22nd", col = "red",lwd = 2)
  })
  

})