Using APIs: Creating Vignette on Stock Data as Example
================
Evan Brown
2022-06-23

-   [**Required Functions**](#required-functions)
-   [Testing work with accessing API and some preliminary graphs,
    deciding where to take the
    project](#testing-work-with-accessing-api-and-some-preliminary-graphs-deciding-where-to-take-the-project)

Hello. This document will show how I work with an API to retrieve data,
create some functions to make data retrieval simpler, and show some data
exploration with that data. Data for this work will come from the
[Polygon Financial API](https://polygon.io/docs/stocks/getting-started)
which has data on market endpoints and reference endpoints for all stock
tickers. For the purposes of this work, a free account was used and
therefor limits the data archive data to the previous 2 years.

Idea for project direction: In this document, I will demonstrate working
with the Polygon Financial API to illustrate a key difference in buying
patterns between common stock holdings and leveraged ETFs in the Oil and
Gas exploratory industry. I will use data from 8/10 top Oil and Gas
industry tickers along with 2x leveraged bear and bull ETFs, DRIP and
GUSH. I will demonstrate that buying patterns over the past 2 years are
indiscriminate to price on oil and gas common stock holdings while in
leveraged ETFs, relative stock price determines trading. So on….

# **Required Functions**

These packages are required in order to work with the Polygon API:

[tidyverse](https://www.tidyverse.org/packages/)
[jsonlite](https://cran.r-project.org/web/packages/jsonlite/index.html)
[httr](https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html)

# Testing work with accessing API and some preliminary graphs, deciding where to take the project

``` r
#The aggregate function returns daily stock endpoints as a tibble for the provided ticker over the past ONE YEAR (june 22, 21 - june 22, 22)
#Possible to work on dates 

aggregate<- function(ticker, apikey){
  #Set URL partitions
  baseURL <- "https://api.polygon.io/v2/aggs/ticker/"
  ticker <- ticker
  adjustments <- "/range/1/day/2021-06-22/2022-06-22?adjusted=true&sort=asc&limit=5000&apiKey="
  apikey <- apikey
  #Paste partitions to get full URL with desired ticker
  fullURL <- paste0(baseURL, ticker, adjustments, apikey)
  
  #Get api data using get function
  api_data <- GET(fullURL)
  #Convert to character string with fromJSON function
  parsed_api_data <- fromJSON(rawToChar(api_data$content))
  #Save as R object
  parsed_api_data_results <- parsed_api_data$results
  #Convert to tibble
  parsed_tibble <- as_tibble(parsed_api_data_results)

  #Append open market trading days over the year
  #Create sequence of raw dates
  dates <- (seq(as.Date("2021-06-22"),as.Date("2022-06-22"),by = 1))
  #Filtering weekends (market not open)
  dates <- as.tibble((dates[!weekdays(dates) %in% c("Saturday","Sunday")]))
  #Filtering holidays (market not open)
  holidaydates <- as.tibble(as.Date(c("2020-01-01", "2020-01-20", "2020-02-17", "2020-04-10", "2020-05-25", "2020-07-03", "2020-09-07", "2020-11-26",    "2020-12-25", "2021-01-01", "2021-01-18", "2021-02-15", "2021-04-02", "2021-05-31", "2021-06-18", "2021-07-05", "2021-09-06", "2021-11-25",          "2021-12-24", "2022-01-17", "2022-02-21", "2022-04-15", "2022-05-30", "2022-06-20", "2022-07-04"))) 
  #Removing holiday dates from final dates vector
  finaldates <- anti_join(dates, holidaydates)
  #Combining data and market open dates
  finaldata <- as_tibble(cbind(parsed_tibble, finaldates))
  
  #Change column names
  colnames(finaldata) <- c("volume", "volume_weighted", "open", "close", "high", "low", "Unix_Msec", "transactions", "date")
  
  #Return tibble of data
  return(finaldata)
}

testfunction <- aggregate("DRIP", "OrlbxnjeCyqGDGkKtpIqxKKs0f8Eh77C")

str(testfunction)
```

    ## tibble [253 × 9] (S3: tbl_df/tbl/data.frame)
    ##  $ volume         : num [1:253] 325532 480233 412088 396969 505422 ...
    ##  $ volume_weighted: num [1:253] 74.3 69.7 69.8 67.8 72.2 ...
    ##  $ open           : num [1:253] 74.6 71.3 71 68.2 67.6 71.5 72.4 66.8 68.2 70.3 ...
    ##  $ close          : num [1:253] 72.8 71.1 69 67.2 73.3 73.9 71 67.7 70.4 76 ...
    ##  $ high           : num [1:253] 76.5 71.3 72 69 74 ...
    ##  $ low            : num [1:253] 72.5 68.2 68.6 66.9 67.6 ...
    ##  $ Unix_Msec      : num [1:253] 1.62e+12 1.62e+12 1.62e+12 1.62e+12 1.62e+12 ...
    ##  $ transactions   : int [1:253] 6167 8449 7110 6069 8743 6276 5883 9012 5961 8575 ...
    ##  $ date           : Date[1:253], format: "2021-06-22" "2021-06-23" "2021-06-24" ...
