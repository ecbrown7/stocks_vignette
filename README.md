Using APIs: Creating Vignette on Stock Data as Example
================
Evan Brown
2022-06-23

-   [**Required Functions**](#required-functions)
-   [**Functions for API Interaction**](#functions-for-api-interaction)
    -   [‘aggregate’ function](#aggregate-function)
    -   [‘marketcap’ Function](#marketcap-function)
-   [Data Exploration](#data-exploration)

This is a vignette to show how to retrieve data from an
[API](https://aws.amazon.com/what-is/api/#:~:text=API%20stands%20for%20Application%20Programming,other%20using%20requests%20and%20responses.).
To demonstrate, I’ll be working with the [Polygon Financial
API](https://polygon.io/docs/stocks/getting-started). For the purposes
of this work, I used a free account with Polygon, limiting my data
retrieval “from-date” to 2 years.

In this vignette, I will work to pull data from the [S&P Oil & Gas
Exploration & Production Industry
Index](https://www.spglobal.com/spdji/en/indices/equity/sp-oil-gas-exploration-production-select-industry-index/#data)
and compare endpoints of some top index constituents to industry
[ETF’s](https://www.investopedia.com/terms/e/etf.asp#:~:text=An%20exchange%2Dtraded%20fund%20(ETF)%20is%20a%20type%20of,that%20a%20regular%20stock%20can.)
and see if I can find any similarities or differences over the past 2
years.

# **Required Functions**

These R packages are required in order to work with the Polygon API:

-   [tidyverse](https://www.tidyverse.org/packages/)  
-   [jsonlite](https://cran.r-project.org/web/packages/jsonlite/index.html)
-   [httr](https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html)

# **Functions for API Interaction**

This portion of the vignette will show where I define functions for
interacting with the Polygon Financial API.

## ‘aggregate’ function

This function returns daily stock endpoints from 06-21-2021 to
06-22-2022 as a tibble for the user provided tibble. apikey default is
my key, but users of the function should pass their own key in the
function call options. User keys can be generated
[here](https://polygon.io/).

``` r
aggregate<- function(ticker, apikey = "OrlbxnjeCyqGDGkKtpIqxKKs0f8Eh77C"){
  
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

  #Create sequence of raw dates
  dates <- (seq(as.Date("2021-06-22"),as.Date("2022-06-22"),by = 1))
    #Filtering weekends (market not open)
    dates <- as.tibble((dates[!weekdays(dates) %in% c("Saturday","Sunday")]))
      #Filtering holidays (market not open)
        holidaydates <- as.tibble(as.Date(c("2020-01-01", "2020-01-20", "2020-02-17", "2020-04-10", "2020-05-25", "2020-07-03", "2020-09-07",     "2020-11-26", "2020-12-25", "2021-01-01", "2021-01-18", "2021-02-15", "2021-04-02", "2021-05-31", "2021-06-18", "2021-07-05", "2021-09-06", "2021-11-25", "2021-12-24", "2022-01-17", "2022-02-21", "2022-04-15", "2022-05-30", "2022-06-20", "2022-07-04"))) 
          #Removing holiday dates from final dates vector
          finaldates <- anti_join(dates, holidaydates)
            #Combining data and market open dates
            finaldata <- as_tibble(cbind(parsed_tibble, finaldates))
  
  #Change column names
  colnames(finaldata) <- c("volume", "volume_weighted", "open", "close", "high", "low", "Unix_Msec", "transactions", "date")
  
  #Return tibble of data
  return(finaldata)
}
```

## ‘marketcap’ Function

This function returns the [market
cap](https://www.investopedia.com/terms/m/marketcapitalization.asp) for
the user provided ticker and date. apikey default is my key, but users
of the function should pass their own key in the function call options.
User keys can be generated [here](https://polygon.io/).

``` r
markettest <- GET("https://api.polygon.io/v3/reference/tickers/DRIP?date=2022-06-22&apiKey=OrlbxnjeCyqGDGkKtpIqxKKs0f8Eh77C")
markettestcontent <- fromJSON(rawToChar(markettest$content))
parsedmarket <- as_tibble(markettestcontent$results)
selectedmarket <- select(parsedmarket, "ticker", "type", "share_class_shares_outstanding")

 
marketprice <- GET("https://api.polygon.io/v1/open-close/DRIP/2022-06-22?adjusted=true&apiKey=OrlbxnjeCyqGDGkKtpIqxKKs0f8Eh77C")
marketpricecontent <- as_tibble(fromJSON(rawToChar(marketprice$content)))
selectedprice <- select(marketpricecontent, "symbol", "close")
colnames(selectedprice) <- c("ticker", "close_price")

fulldataprice <- full_join(selectedmarket, selectedprice)
fulldataprice$market_cap = fulldataprice$share_class_shares_outstanding * fulldataprice$close_price
```

# Data Exploration

This portion fo the vignette will now use the function for interacting
with the Polygon Financial API to retrieve some data and look at some of
the endpoints.

The S&P Oil & Gas Exploration & Production Industry Index is comprised
of 61 constituents.
