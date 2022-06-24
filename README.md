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

# **Required Functions**

These packages are required in order to work with the Polygon API:

[tidyverse](https://www.tidyverse.org/packages/)
[jsonlite](https://cran.r-project.org/web/packages/jsonlite/index.html)
[httr](https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html)

# Testing work with accessing API and some preliminary graphs, deciding where to take the project

``` r
api_data <- GET("https://api.polygon.io/v2/aggs/ticker/DRIP/range/1/day/2021-06-22/2022-06-22?adjusted=true&sort=asc&limit=5000&apiKey=OrlbxnjeCyqGDGkKtpIqxKKs0f8Eh77C")

#Convert to character string with fromJSON function
parsed_api_data <- fromJSON(rawToChar(api_data$content))
#str(parsed_api_data, max.level = 1)

#Evaluate dataframe variable names
parsed_api_data$results %>% colnames()
```

    ## [1] "v"  "vw" "o"  "c"  "h"  "l"  "t"  "n"

``` r
#Save as R object
parsed_api_data_results <- parsed_api_data$results

#Select source, author and title and print tibble
parsed_tibble <- as_tibble(parsed_api_data_results)
parsed_tibble
```

    ## # A tibble: 253 x 8
    ##          v    vw     o     c     h     l           t     n
    ##      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl> <int>
    ##  1 325532.  74.3  74.6  72.8  76.5  72.5     1.62e12  6167
    ##  2 480233.  69.7  71.3  71.1  71.3  68.2     1.62e12  8449
    ##  3 412088.  69.8  71    69    72.0  68.6     1.62e12  7110
    ##  4 396969   67.8  68.2  67.2  69.0  66.9     1.62e12  6069
    ##  5 505422   72.2  67.6  73.3  74.0  67.6     1.62e12  8743
    ##  6 318393.  71.6  71.5  73.9  74.2  70       1.62e12  6276
    ##  7 315401.  71.6  72.4  71    73.1  70.7     1.63e12  5883
    ##  8 483347.  67.7  66.8  67.7  69.4  66.1     1.63e12  9012
    ##  9 312484.  69.6  68.2  70.4  70.6  67.9     1.63e12  5961
    ## 10 428755.  74.5  70.3  76    77.0  70.3     1.63e12  8575
    ## # ... with 243 more rows

``` r
#Append open market trading days over the year
dates <- (seq(as.Date("2021-06-22"),as.Date("2022-06-22"),by = 1))
dates <- as.tibble((dates[!weekdays(dates) %in% c("Saturday","Sunday")]))
holidaydates <- as.tibble(as.Date(c("2021-07-05", "2021-09-06", "2021-11-25", "2021-12-24", "2022-01-17", "2022-02-21", "2022-04-15", "2022-05-30", "2022-06-20")))
finaldates <- anti_join(dates, holidaydates)
finaldata <- cbind(parsed_tibble, finaldates)

plot <- ggplot(finaldata, aes(x=finaldata$value, y=finaldata$h))
plot + geom_point() + geom_smooth(method=lm, color = "Blue")
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
api_dataTEST <- GET("https://api.polygon.io/v2/aggs/ticker/GUSH/range/1/day/2021-06-22/2022-06-22?adjusted=true&sort=asc&limit=5000&apiKey=OrlbxnjeCyqGDGkKtpIqxKKs0f8Eh77C")

#Convert to character string with fromJSON function
parsed_api_dataTEST <- fromJSON(rawToChar(api_dataTEST$content))
#str(parsed_api_data, max.level = 1)

#Evaluate dataframe variable names
parsed_api_dataTEST$results %>% colnames()
```

    ## [1] "v"  "vw" "o"  "c"  "h"  "l"  "t"  "n"

``` r
#Save as R object
parsed_api_dataTEST_results <- parsed_api_dataTEST$results

#Select source, author and title and print tibble
parsed_tibbleTEST <- as_tibble(parsed_api_dataTEST_results)
parsed_tibbleTEST
```

    ## # A tibble: 253 x 8
    ##          v    vw     o     c     h     l           t     n
    ##      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl> <int>
    ##  1 1547425  93.0  93.0  94.9  95.4  90.4     1.62e12 15133
    ##  2 1710057  99.2  96.8  97.1 101.   96.8     1.62e12 16792
    ##  3 1795990  99.0  97.4 100.  101.   95.8     1.62e12 22111
    ##  4 1362838 102.  101.  102.  103.  100.      1.62e12 20568
    ##  5 2365226  95.1 102    93.6 102    92.3     1.62e12 31596
    ##  6 1566271  95.1  95.8  92.6  97.6  92.3     1.62e12 19106
    ##  7 1325914  95.4  94.3  96.2  96.5  93.7     1.63e12 18226
    ##  8 1902994 101.  102.  101.  103    98.5     1.63e12 18192
    ##  9 1420934  97.8 100    96.9 100    96.4     1.63e12 16009
    ## 10 2424889  90.9  97.2  89.2  97.2  87.7     1.63e12 27398
    ## # ... with 243 more rows

``` r
#Append open market trading days over the year
datesTEST <- (seq(as.Date("2021-06-22"),as.Date("2022-06-22"),by = 1))
datesTEST <- as.tibble((datesTEST[!weekdays(datesTEST) %in% c("Saturday","Sunday")]))
holidaydatesTEST <- as.tibble(as.Date(c("2020-01-01", "2020-01-20", "2020-02-17", "2020-04-10", "2020-05-25", "2020-07-03", "2020-09-07", "2020-11-26", "2020-12-25", "2021-01-01", "2021-01-18", "2021-02-15", "2021-04-02", "2021-05-31", "2021-06-18", "2021-07-05", "2021-09-06", "2021-11-25", "2021-12-24", "2022-01-17", "2022-02-21", "2022-04-15", "2022-05-30", "2022-06-20")))
finaldatesTEST <- anti_join(datesTEST, holidaydatesTEST)
finaldataTEST <- cbind(parsed_tibbleTEST, finaldatesTEST)
colnames(finaldataTEST) <- c("vol", "volweight", "open", "close", "high", "low", "timestamp", "number", "date")

plotTEST <- ggplot(finaldataTEST, aes(x=finaldataTEST$date, y=finaldataTEST$high))
plotTEST + geom_point() + geom_smooth(method=lm, color = "Blue")
```

![](README_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
plotTEST2 <- ggplot(finaldataTEST, aes(x=finaldataTEST$high, y=finaldataTEST$vol))
plotTEST2 + geom_point()
```

![](README_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

``` r
newsample <- cbind(finaldata, finaldataTEST)
newsample$mean = ((newsample$h + newsample$high) /2)

plotTEST3 <- ggplot(newsample, aes(x=newsample$value, y=newsample$mean))
plotTEST3 + geom_point()
```

![](README_files/figure-gfm/unnamed-chunk-2-4.png)<!-- -->
