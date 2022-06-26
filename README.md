Using APIs: Creating Vignette on Stock Data as Example
================
Evan Brown
2022-06-23

-   [**Required Functions**](#required-functions)
-   [**Functions for API Interaction**](#functions-for-api-interaction)
    -   [‘aggregate’ function](#aggregate-function)
    -   [‘marketcap’ Function](#marketcap-function)
-   [**Data Exploration**](#data-exploration)

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
      adjustments <- "/range/1/day/2020-06-25/2022-06-25?adjusted=true&sort=asc&limit=5000&apiKey="
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
  dates <- (seq(as.Date("2020-06-25"),as.Date("2022-06-25"),by = 1))
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
marketcap <- function(ticker, date, apikey = "OrlbxnjeCyqGDGkKtpIqxKKs0f8Eh77C"){
  
  #Set URL partitions
  baseURL <- "https://api.polygon.io/v3/reference/tickers/"
    ticker <- ticker
      dateadjustment <- "?date="
        date <- date
          adjustments <- "&apiKey="
            apikey <- apikey
          
  #Paste partitions to get full URL with desired ticker
  fullURL <- paste0(baseURL, ticker, dateadjustment, date, adjustments, apikey)   
  
  #Get api data using get function
  api_data <- GET(fullURL)
    #Convert to character string with fromJSON function
    parsed_api_data <- fromJSON(rawToChar(api_data$content))
      #Save as R object
      parsed_api_data_results <- parsed_api_data$results
      results <- as_data_frame(parsed_api_data_results[names(parsed_api_data_results) != "address"])
      ticker <- results$ticker
      marketcap <- results$market_cap
      year <- substr(date, 1, 4)
    
      finaldata <- as_data_frame(cbind(ticker, marketcap, year))
      finaldata <- finaldata[1,]
      
      return(finaldata)
}
```

# **Data Exploration**

This portion fo the vignette will now use the functions for interacting
with the Polygon Financial API to retrieve some data and look at some of
the endpoints.

The S&P Oil & Gas Exploration & Production Industry Index is comprised
of 61 constituents. In this, we’ll take a look at 3 of the biggest
common stock contributors: Valero Energy Corporation (VLO), Marathon Oil
Corporation (MRO), and Southwestern Energy Company (SWN). Additionally
in the latter part of this section, we’ll take a look at 2 ETF’s that
follow this industry index. DRIP is a 2x leveraged *inverse* ETF,
meaning DRIP returns 2x the inverse of the daily oil industry index
performance. Conversely, GUSH tracks with the index and is a standard 2x
leveraged ETF. Using data from the api, we’ll look into some basic stock
endpoints, look at trends since 2020, and show some differences in the
“Big 3” common stocks versus ETFs.

Let’s start with some simple application of the market cap function.
This function queries the polygon api and returns the ticker of your
search, marketcap, and year of you search. Here, I’m going to get the
market cap value for each Big 3 ticker in 2020, 2021 and 2022 (in June
of each year). Then, I’m going to make a bar plot of the market caps by
year to look for any trends.

Starting with VLO:

``` r
#Getting market cap for 2020 (non-weekend)
VLOcap_20 <- marketcap("VLO", "2020-06-26")
  #Getting marketcap for 2021
  VLOcap_21 <- marketcap("VLO", "2021-06-25")
    #Getting market cap for 2022
    VLOcap_22 <- marketcap("VLO", "2022-06-24")

#Putting these caps together to create a dataset to plot from
VLOcaps <- as_tibble(bind_rows(VLOcap_20, VLOcap_21, VLOcap_22))

#Changing market cap to a numeric value for better plotting
VLOcaps$marketcap <- as.numeric(VLOcaps$marketcap)

#Plotting market cap for VLO over past 2 years
plota <- ggplot(VLOcaps, aes(x = ticker, y = marketcap)) +
  #Create side-by-side bar plot filled by year
  geom_bar(aes(fill=year), stat = "identity", position = "dodge") +
  #Add title and labels
  labs( x = "Ticker", y = "Market Cap", title = "VLO Market Cap by Year") +
  #Set legend title
  scale_fill_discrete(name = "Year") 

plota
```

![](README_files/figure-gfmunnamed-chunk-5-1.png)<!-- -->

Okay, so clearly an increase in market cap for VLO each year since 2020.
Let’s see if the others follow that trend.

MRO:

``` r
#Getting market cap for 2020 (non-weekend)
MROcap_20 <- marketcap("MRO", "2020-06-26")
  #Getting marketcap for 2021
  MROcap_21 <- marketcap("MRO", "2021-06-25")
    #Getting market cap for 2022
    MROcap_22 <- marketcap("MRO", "2022-06-24")
```

    ## Warning: Unknown or uninitialised column: `ticker`.

    ## Warning: Unknown or uninitialised column: `market_cap`.

``` r
#Putting these caps together to create a dataset to plot from
MROcaps <- as_tibble(bind_rows(MROcap_20, MROcap_21, MROcap_22))

#Changing market cap to a numeric value for better plotting
MROcaps$marketcap <- as.numeric(MROcaps$marketcap)

#Plotting market cap for VLO over past 2 years
plotb <- ggplot(MROcaps, aes(x = ticker, y = marketcap)) +
  #Create side-by-side bar plot filled by year
  geom_bar(aes(fill=year), stat = "identity", position = "dodge") +
  #Add title and labels
  labs( x = "Ticker", y = "Market Cap", title = "MRO Market Cap by Year") +
  #Set legend title
  scale_fill_discrete(name = "Year") 

plotb
```

    ## Warning: Removed 1 rows containing missing values (geom_bar).

![](README_files/figure-gfmunnamed-chunk-6-1.png)<!-- -->

SWN:

Okay, so the marketcap function works in allowing us to retrieve market
cap data on the Big 3 oil companies. Plotting those values in a bar plot
by year let us see that each of the Big 3 oil companies have increased
market cap significantly each year since 2020.

Now, let’s switch gears and use the aggregate function to retrieve some
data from the oil industry regarding stock price and stock volume.
First, we’ll take a look at the oil industry Big 3 again to explore
price and volume, then take a look at the 2 popular leveraged ETF’s of
the index: DRIP (2x inverse) and GUSH (2x), to see if the ETF’s follow a
different trend than the common stocks. Since DRIP is an inverse, we
should expect the price trends to be inverse that of all other stocks.

Let’s start by exploring VLO:

``` r
#Retrieving data on DRIP ticker
VLO <- aggregate("VLO")

#Creating avg daily price column
VLO$avgprice <- ((VLO$high + VLO$low)/2)

#Quick plot of average daily price over the past 2 years
plot3 <- ggplot(VLO, aes(x=date, y=avgprice)) +
  geom_point() +
  geom_smooth() +
  labs( x = "Date", y = "Avg Daily Price", title = "VLO Average Daily Price Data")
plot3
```

![](README_files/figure-gfmunnamed-chunk-8-1.png)<!-- -->

``` r
plot4 <- ggplot(VLO, aes(x=date, y=volume)) +
  geom_point() +
  labs( x = "Date", y = "Volume", title = "VLO Volume Data")
plot4
```

![](README_files/figure-gfmunnamed-chunk-8-2.png)<!-- -->

MRO:

``` r
#Retrieving data on DRIP ticker
MRO <- aggregate("MRO")

#Creating avg daily price column
MRO$avgprice <- ((MRO$high + MRO$low)/2)

#Quick plot of average daily price over the past 2 years
plot5 <- ggplot(MRO, aes(x=date, y=avgprice)) +
  geom_point() +
  geom_smooth() +
  labs( x = "Date", y = "Avg Daily Price", title = "MRO Average Daily Price Data")
plot5
```

![](README_files/figure-gfmunnamed-chunk-9-1.png)<!-- -->

``` r
plot6 <- ggplot(MRO, aes(x=date, y=volume)) +
  geom_point() +
  labs( x = "Date", y = "Volume", title = "MRO Volume Data")
plot6
```

![](README_files/figure-gfmunnamed-chunk-9-2.png)<!-- -->

SWN:

``` r
#Retrieving data on DRIP ticker
SWN <- aggregate("SWN")

#Creating avg daily price column
SWN$avgprice <- ((SWN$high + SWN$low)/2)

#Quick plot of average daily price over the past 2 years
plot7 <- ggplot(SWN, aes(x=date, y=avgprice)) +
  geom_point() +
  geom_smooth() +
  labs( x = "Date", y = "Avg Daily Price", title = "SWN Average Daily Price Data")
plot7
```

![](README_files/figure-gfmunnamed-chunk-10-1.png)<!-- -->

``` r
plot8 <- ggplot(SWN, aes(x=date, y=volume)) +
  geom_point() +
  labs( x = "Date", y = "Volume", title = "SWN Volume Data")
plot8
```

![](README_files/figure-gfmunnamed-chunk-10-2.png)<!-- -->

All of the oil industry Big 3 companies show a solid increase in average
daily stock price and consistent volume across the price increase. Safe
to say these companies have been doing well the past two years!

Now, let’s look at those leveraged ETF’s, starting with the inverse,
DRIP:

``` r
#Retrieving data on DRIP ticker
DRIP <- aggregate("DRIP")

#Creating avg daily price column
DRIP$avgprice <- ((DRIP$high + DRIP$low)/2)

#Quick plot of average daily price over the past 2 years
plot9 <- ggplot(DRIP, aes(x=date, y=avgprice)) +
  geom_point() +
  geom_smooth() +
  labs( x = "Date", y = "Avg Daily Price", title = "DRIP Average Daily Price Data")
plot9
```

![](README_files/figure-gfmunnamed-chunk-11-1.png)<!-- -->

``` r
plot10 <- ggplot(DRIP, aes(x=date, y=volume)) +
  geom_point() +
  labs( x = "Date", y = "Volume", title = "DRIP Volume Data")
plot10
```

![](README_files/figure-gfmunnamed-chunk-11-2.png)<!-- -->

Well, that looks different! As expected, the inverse ETF has a trend
opposite the Big 3 index stocks. However, what stands out to me is the
volume scatter plot. Volume seems to have gone hyperbolic in the year
2022, perhaps because price is so low and buyers expect a pull back from
the impressive increase in the Big 3.

Let’s see about GUSH:

``` r
#Retrieving data on DRIP ticker
GUSH <- aggregate("GUSH")

#Creating avg daily price column
GUSH$avgprice <- ((GUSH$high + GUSH$low)/2)

#Quick plot of average daily price over the past 2 years
plot11 <- ggplot(GUSH, aes(x=date, y=avgprice)) +
  geom_point() +
  geom_smooth() +
  labs( x = "Date", y = "Avg Daily Price", title = "GUSH Average Daily Price Data")
plot11
```

![](README_files/figure-gfmunnamed-chunk-12-1.png)<!-- -->

``` r
plot12 <- ggplot(GUSH, aes(x=date, y=volume)) +
  geom_point() +
  labs( x = "Date", y = "Volume", title = "GUSH Volume Data")
plot12
```

![](README_files/figure-gfmunnamed-chunk-12-2.png)<!-- -->

As expected, since GUSH follows the industry, the price trend looks very
similar to that of the Big 3. However, volume has decreased over time
significantly more than those Big 3 companies. It certainly seems like
the tide is turning away from a bullish oil market and more towards a
bearish one.
