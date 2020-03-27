# I will plot some statistics about COVID-19 using the Johns Hopkins University github repo: 
# https://github.com/CSSEGISandData/COVID-19

# Those guys are doing a great job with daily updates of confirmed cases, 
# deaths, etc. So its a matter of pulling their data daily.


# First load the relevan libraries
library(plotly)
library(data.table)
library(tidyr)
library(leaflet)
library(shiny)

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

# Use a fluid Bootstrap layout
shinyUI(fluidPage(    
  
  # Give the page a title
  titlePanel("Cases by Country - Time Series"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
      # Define the sidebar with one input
    sidebarPanel(
      selectInput("country", "Country to Display:", 
                  choices=levels(timeseriesconfirmed$Country.Region)),
      selectInput("status", "Status to Forecast:", 
                  choices=c("Confirmed", "Dead", "Recovered")),
      hr(),
      helpText("Data from the Johns Hopkins University Repository")
    ),
      # Create a spot for the barplot
  mainPanel(
    tabsetPanel(type = "tabs", 
                tabPanel("Country Data", plotlyOutput("CountryPlot")),
                tabPanel("Forecast", plotOutput("Forecast"))
      
    )
    
  )
)))