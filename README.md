Using APIs: Creating Vignette on Stock Data as Example
================
Evan Brown
2022-06-23

-   [**Required Functions**](#required-functions)
-   [**Functions for API Interaction**](#functions-for-api-interaction)
    -   [‘marketcap’ Function](#marketcap-function)
    -   [‘aggregate’ function](#aggregate-function)
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

# **Data Exploration**

This portion fo the vignette will now use the functions for interacting
with the Polygon Financial API to retrieve some data and look at some of
the endpoints.

The S&P Oil & Gas Exploration & Production Industry Index is comprised
of 61 constituents. In this, we’ll just take a look at one of the
biggest common stock contributors, Valero Energy Corporation (VLO), to
see generally how the oil industry has been doing. Additionally, in the
latter part of this section, we’ll take a look at 2 ETF’s that follow
this industry index. DRIP is a 2x leveraged *inverse* ETF, meaning DRIP
returns 2x the inverse of the daily oil industry index performance.
Conversely, GUSH tracks with the index and is a standard 2x leveraged
ETF. Using data from the api, we’ll look into some basic stock
endpoints, look at trends since 2020, and show some differences between
VLO and those ETF’s.

Let’s start with some simple application of the market cap function.
This function queries the polygon api and returns the ticker of your
search, marketcap, and year of you search. Here, I’m going to get the
market cap value for the VLO ticker in 2020, 2021 and 2022 (in June of
each year). Then, I’m going to make a bar plot of the market caps by
year to look for any trends.

VLO:

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

![](README_files/figure-gfmunnamed-chunk-140-1.png)<!-- -->

Okay, so the marketcap function works in allowing us to retrieve market
cap data on a common stock oil company. Plotting those values in a bar
plot by year let us see that Valero Oil has increased market cap
significantly each year since 2020.

Now, let’s switch gears and use the aggregate function to retrieve some
data from the oil industry regarding stock price and stock volume.
First, we’ll take a look at Valero and evaluate some summary statistics
then create a variable mapping price to annual quarter to explore which
quarter VLO performed best in over the past year. Then, we’ll create a
histogram of price data to see which range VLO has spent the most time
in over the past year. Finally, we’ll take a look at Valero again to
explore price and volume, then take a look at the 2 popular leveraged
ETF’s of the index: DRIP (2x inverse) and GUSH (2x), to see if the ETF’s
follow a different trend than the common stocks. Since DRIP is an
inverse, we should expect the price trends to be inverse that of all
other stocks.

Let’s start by exploring VLO. Here, let’s create the daily average price
variable (average of daily high and low price) and find some summary
statistics:

``` r
#Retrieving data on DRIP ticker
VLO <- aggregate("VLO")

#Creating avg daily price column
VLO$avgprice <- ((VLO$high + VLO$low)/2)

#Select variables to create summary statistics for
VLOselect <- select(VLO, -2, -7:-8)

#Use select to reorder the columns with avgprice first
selectedVLO <- select(VLOselect, 7, 1:6)

#Create basic summary statistics for VLO, excluding date
summary(selectedVLO[1:6])
```

    ##     avgprice          volume              open            close             high       
    ##  Min.   : 36.59   Min.   : 1385850   Min.   : 35.79   Min.   : 36.19   Min.   : 37.67  
    ##  1st Qu.: 58.11   1st Qu.: 3298204   1st Qu.: 57.67   1st Qu.: 57.95   1st Qu.: 59.30  
    ##  Median : 71.41   Median : 4049922   Median : 71.33   Median : 71.39   Median : 72.51  
    ##  Mean   : 73.53   Mean   : 4431341   Mean   : 73.51   Mean   : 73.54   Mean   : 74.87  
    ##  3rd Qu.: 81.72   3rd Qu.: 5085740   3rd Qu.: 81.75   3rd Qu.: 81.54   3rd Qu.: 82.91  
    ##  Max.   :144.81   Max.   :19637591   Max.   :144.43   Max.   :145.08   Max.   :146.81  
    ##       low        
    ##  Min.   : 35.44  
    ##  1st Qu.: 56.66  
    ##  Median : 70.50  
    ##  Mean   : 72.18  
    ##  3rd Qu.: 80.30  
    ##  Max.   :142.81

Good. Now we have some basic idea of some summary statistics for the VLO
ticker over the past year. Now, let’s group this into years and yearly
quarters and see where VLO performed the best.

``` r
#Create month variable to map quarters to
selectedVLO <- selectedVLO %>% mutate(month = substr(date, 6, 7))

#Create year variable
selectedVLO <- selectedVLO %>% mutate(year = substr(date, 1, 4))

#Coerc new vars to numeric
selectedVLO$month <- as.numeric(selectedVLO$month)
selectedVLO$year <- as.numeric(selectedVLO$year)

#Annual quarters are:
    #Quarter 1: Jan 1 - Mar 31
    #Quarter 2: Apr 1 - June 30
    #Quarter 3: Jul 1 - Sep 30
    #Quarter 4: Oct 1 - Dec 31
#Map an annual quarters variable based on month of data
quarteredVLO <- selectedVLO %>% mutate(quarter = if_else(month >= 10, "Quarter 4", 
                                                         if_else(month >= 7, "Quarter 3",
                                                                 if_else(month >= 4, "Quarter 2", "Quarter 1"))))

#Getting summary stats on each quarter and year
avgpriceMonthly <- quarteredVLO %>% group_by(month) %>% summarise(mean = mean(avgprice), sd = sd(avgprice)) 
avgpriceMonthly
```

    ## # A tibble: 12 × 3
    ##    month  mean    sd
    ##    <dbl> <dbl> <dbl>
    ##  1     1  70.3 11.6 
    ##  2     2  77.5 10.9 
    ##  3     3  83.4  8.66
    ##  4     4  88.2 16.9 
    ##  5     5 102.  23.0 
    ##  6     6  98.1 27.5 
    ##  7     7  62.0  7.52
    ##  8     8  59.6  6.11
    ##  9     9  56.7 10.0 
    ## 10    10  59.1 19.2 
    ## 11    11  62.6 13.4 
    ## 12    12  63.9  7.31

``` r
avgpriceYearly <- quarteredVLO %>% group_by(year) %>% summarise(mean = mean(avgprice), sd = sd(avgprice)) 
avgpriceYearly
```

    ## # A tibble: 3 × 3
    ##    year  mean    sd
    ##   <dbl> <dbl> <dbl>
    ## 1  2020  50.9  6.43
    ## 2  2021  71.7  7.06
    ## 3  2022 102.  18.9

Now we can see some summary stats (mean and standard deviation) for
VLO’s average daily price for each month and year. Now let’s plot these
and visualize that data.

``` r
plotVLOmonth <- ggplot(avgpriceMonthly, aes(x=month, y=mean)) +
  geom_bar(stat = "identity") + 
  scale_x_continuous(breaks = seq(0, 12, by=1)) +
  labs( x = "Month", y = "Avg Price", title = "VLO Monthly Average Price")

plotVLOyear <- ggplot(avgpriceYearly, aes(x=year, y=mean)) +
  geom_bar(stat = "identity") + 
  labs( x = "Year", y = "Avg Price", title = "VLO Yearly Average Price")

plotVLOmonth
```

![](README_files/figure-gfmunnamed-chunk-143-1.png)<!-- -->

``` r
plotVLOyear
```

![](README_files/figure-gfmunnamed-chunk-143-2.png)<!-- -->

That’s pretty helpful, particularly the yearly bar plot. But I think the
monthly average price bar plot leaves much to be desired. Let’s try
plotting this as a box plot so we can capture the full set of summary
statistics for each month, not just the mean.

``` r
plotVLObox <- ggplot(quarteredVLO, aes(group = month, x = month, y=avgprice)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(0, 12, by=1)) +
  labs( x = "Month", y = "Avg Price", title = "VLO Monthly Average Price Boxplot")

plotVLObox
```

![](README_files/figure-gfmunnamed-chunk-144-1.png)<!-- -->

Okay. Great. That box plot looks much better for understanding the
monthly price data. In the bar plot of average monthly price, month 5
seemed to be the highest. However with the boxplot, we can Now we can
interpret the large inter-quartile range with far reaching outliers as a
month of transition.

Since the price is so wide ranging, let’s look at grouping the stock
price into bins to see where VLO has spent the most time in. We’ll plot
this as a histogram and this should give a good idea of where the VLO
price has been hanging around at for the majority of the time.

``` r
plotVLOhistogram <- ggplot(selectedVLO, aes(x=avgprice)) +
  geom_histogram(color="black", binwidth = 3) +
  labs( x = "Average Price", y = "Count", title = "VLO Average Price Histogram")

plotVLOhistogram
```

![](README_files/figure-gfmunnamed-chunk-145-1.png)<!-- -->

This is interesting. While the daily price has occasionally spent time
north of 100, in months 4 and 5 from the box plot, the majority of daily
price counts have been between 55 and 80 dollars.

``` r
#Quick plot of average daily price over the past 2 years
plotVLO <- ggplot(VLO, aes(x=date, y=avgprice)) +
  geom_point() +
  geom_smooth() +
  labs( x = "Date", y = "Avg Daily Price", title = "VLO Average Daily Price Data")

plotVLOv <- ggplot(VLO, aes(x=date, y=volume)) +
  geom_point() +
  labs( x = "Date", y = "Volume", title = "VLO Volume Data")

plotVLO
```

![](README_files/figure-gfmunnamed-chunk-146-1.png)<!-- -->

``` r
plotVLOv
```

![](README_files/figure-gfmunnamed-chunk-146-2.png)<!-- -->

Valero has shown a solid increase in average daily stock price and
consistent volume across the price increase. Safe to say they have been
doing well the past two years!

Now, let’s look at those leveraged ETF’s, starting with the inverse,
DRIP:

``` r
#Retrieving data on DRIP ticker
DRIP <- aggregate("DRIP")

#Creating avg daily price column
DRIP$avgprice <- ((DRIP$high + DRIP$low)/2)

#Plot of average daily price over the past 2 years
plotDRIP <- ggplot(DRIP, aes(x=date, y=avgprice)) +
  geom_point() +
  geom_smooth() +
  labs( x = "Date", y = "Avg Daily Price", title = "DRIP Average Daily Price Data")

#Plot of daily volume over the past 2 years
plotDRIPv <- ggplot(DRIP, aes(x=date, y=volume)) +
  geom_point() +
  labs( x = "Date", y = "Volume", title = "DRIP Volume Data")

plotDRIP
```

![](README_files/figure-gfmunnamed-chunk-147-1.png)<!-- -->

``` r
plotDRIPv
```

![](README_files/figure-gfmunnamed-chunk-147-2.png)<!-- -->

Well, that looks different! As expected, the inverse ETF has a trend
opposite the oil industry index stocks. However, what stands out to me
is the volume scatter plot. Volume seems to have gone hyperbolic in the
year 2022, perhaps because price is so low and buyers expect a pull back
from the impressive increase in the oil industry.

Let’s see about GUSH:

``` r
#Retrieving data on DRIP ticker
GUSH <- aggregate("GUSH")

#Creating avg daily price column
GUSH$avgprice <- ((GUSH$high + GUSH$low)/2)

#Quick plot of average daily price over the past 2 years
plotGUSH <- ggplot(GUSH, aes(x=date, y=avgprice)) +
  geom_point() +
  geom_smooth() +
  labs( x = "Date", y = "Avg Daily Price", title = "GUSH Average Daily Price Data")


plotGUSHv <- ggplot(GUSH, aes(x=date, y=volume)) +
  geom_point() +
  labs( x = "Date", y = "Volume", title = "GUSH Volume Data")

plotGUSH
```

![](README_files/figure-gfmunnamed-chunk-148-1.png)<!-- -->

``` r
plotGUSHv
```

![](README_files/figure-gfmunnamed-chunk-148-2.png)<!-- -->

As expected, since GUSH follows the industry, the price trend looks very
similar to that of the Big 3. However, volume has decreased over time
significantly more than Valero. It certainly seems like the tide is
turning away from a bullish oil market and more towards a bearish one.
