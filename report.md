# Most harmful weather events with respect to US population health and the economy, 1950 to 2011.

### 24 May 2014

## Synopsis

This research set out to answer:

1. Which types of severe weather events are most harmful with respect to population health in the United States?

1. Which types of severe weather events have the greatest economic consequences in the US.

Tornados have caused the most fatalities and injuries in the period from 1950 to November 2011. Heat has also been a significant cause of death and injury. Floods caused the most damage to property and crops during the same period.

The research uses data from the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database, which records severe weather events. The [Results](#results) section presents the top event types by impact to population health and the economy.

## Tools Used

R version 3.1.0 (2014-04-10, "Spring Dance") and the following R packages were used in this analysis.


```r
library(plyr)
library(ggplot2)
```


Data processing procedures were run on a MacBook Pro, Intel Core i7 CPU with 16GB RAM and a solid state drive.

## Data Processing

The U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

The raw data can be downloaded from the NOAA website as a compressed CSV file: "repdata-data-StormData.csv.bz2". The following code reads the file. (The compressed file format is automatically unzipped.)


```r
storm.data <- read.csv("data//repdata-data-StormData.csv.bz2", header = T, nrows = 902298, 
    stringsAsFactors = F)
```


This file contains 902,297 observations of 37 variables.

In particular, the following data has been used:
* Number of fatalities (FATALITIES) and injuries (INJURIES) for each type of event (EVTYPE).
* Estimated cost of damage to property (PROPDMG) and crops (CROPDMG) for each type of event.
* LATITUDE and LONGITUDE of events. (This is not available for all event types, such as heat waves and floods, which occur over a larger geaographical area.)

Event types included instances similar in meaning for the purposes of this analysis, and duplicates as a result of variation in spelling. For example, TORNADO is represented by TORNADOS, TORNADO F1, WATERSPOUT/TORNADOS, TORNDAO, etc. Significant categories of event types have been grouped together based on the following rules.


```r
storm.data[grepl("torn", storm.data$EVTYPE, ignore.case = T), ]$EVTYPE <- "TORNADO"
storm.data[grepl("flood", storm.data$EVTYPE, ignore.case = T), ]$EVTYPE <- "FLOOD"
storm.data[grepl("(hurricane|typhoon)", storm.data$EVTYPE, ignore.case = T), 
    ]$EVTYPE <- "HURRICANE"
storm.data[grepl("(tstm|t\\w+orm)", storm.data$EVTYPE, ignore.case = T), ]$EVTYPE <- "THUNDERSTORM"
storm.data[grepl("wind", storm.data$EVTYPE, ignore.case = T), ]$EVTYPE <- "WINDS"
storm.data[grepl("heat", storm.data$EVTYPE, ignore.case = T), ]$EVTYPE <- "HEAT"
storm.data[grepl("fire", storm.data$EVTYPE, ignore.case = T), ]$EVTYPE <- "FIRE"
storm.data[grepl("frost", storm.data$EVTYPE, ignore.case = T), ]$EVTYPE <- "FROST"
storm.data$EVTYPE <- as.factor(storm.data$EVTYPE)
```


Measurements of number of fatalities and injuries was used to determine impact on population health, and the estimated cost of property and crop damage has been used to determine economic impact. The number of fatalities and injuries by event type is arranged by combined total in descending order for further analysis.


```r
summary.fatalities <- ddply(storm.data, .(EVTYPE), summarize, total.fatalities = sum(FATALITIES))
summary.fatalities <- subset(summary.fatalities, total.fatalities > 0)
by.fatalities <- arrange(summary.fatalities, desc(total.fatalities))

summary.injuries <- ddply(storm.data, .(EVTYPE), summarize, total.injuries = sum(INJURIES))
summary.injuries <- subset(summary.injuries, total.injuries > 0)
by.injuries <- arrange(summary.injuries, desc(total.injuries))

summary.total <- ddply(storm.data, .(EVTYPE), summarize, total.fatalities = sum(FATALITIES), 
    total.injuries = sum(INJURIES))
summary.total$total <- summary.total$total.fatalities + summary.total$total.injuries
summary.total <- subset(summary.total, total > 0)
by.total <- arrange(summary.total, desc(total))
```


Estimated costs for property and crop damage are each stored in two columns. The first is the amount, and the second is the unit such as "K" for thousands of dollars, "M" for millions, etc. If the unit column is empty or contains an unknown value, then a unit of 1 has been assumed. The following code normalizes the costs. The estimated cost of property and crop damage by event type is arranged by combined total in descending order.


```r
m <- c(H = 100, K = 1000, M = 1e+06, B = 1e+09, N = 1)
damage <- storm.data
ind <- match(toupper(as.character(damage$PROPDMGEXP)), names(m), nomatch = 5)
damage$property.cost <- damage$PROPDMG * m[ind]
ind <- match(toupper(as.character(damage$CROPDMGEXP)), names(m), nomatch = 5)
damage$crop.cost <- damage$CROPDMG * m[ind]
damage <- subset(damage, property.cost > 0 | crop.cost > 0)
damage.summary <- ddply(damage, .(EVTYPE), summarize, total.property = round(sum(property.cost)/1e+09, 
    2), total.crop = round(sum(crop.cost)/1e+09, 2))
damage.summary$total.cost <- damage.summary$total.property + damage.summary$total.crop
by.total.cost <- arrange(damage.summary, desc(total.cost))
```


<a name="results"></a>
## Results

### Population health impact

Tornados caused the most fatalities and injuries in the period 1950 to November, 2011.

The top 10 event types impacting population health as measured by number of fatalities and injuries are shown below.


```r
top10 <- by.total[1:10, ]
ggplot(top10, aes(total.fatalities, total.injuries)) + xlab("Number fatalities") + 
    ylab("Number injuries") + ggtitle("Impact of major events on US population health, 1950 - 2011") + 
    geom_point() + geom_text(aes(label = EVTYPE), size = 4, angle = 45, hjust = 0, 
    vjust = 0) + scale_x_log10() + scale_y_log10()
```

![plot of chunk top_10_health](figure/top_10_health.png) 

**Axes are shown on a log10 scale.**

<a name="top10health"></a>


```r
kable(head(by.total, 10))
```

|EVTYPE        |  total.fatalities|  total.injuries|  total|
|:-------------|-----------------:|---------------:|------:|
|TORNADO       |              5661|           91407|  97068|
|HEAT          |              3138|            9224|  12362|
|THUNDERSTORM  |               731|            9544|  10275|
|FLOOD         |              1525|            8604|  10129|
|LIGHTNING     |               816|            5230|   6046|
|WINDS         |               695|            1994|   2689|
|ICE STORM     |                89|            1975|   2064|
|FIRE          |                90|            1608|   1698|
|WINTER STORM  |               206|            1321|   1527|
|HURRICANE     |               135|            1333|   1468|


Sudden events such as tornados cause a great deal of damage in a short period of time and can be quite localized. Whereas events such as heat waves and floods cause damage across a larger segment of the population. The plot below show the locality and area of impact of major event types in 2011 for which geographic data exists.


```r
health <- storm.data
health$total <- health$FATALITIES + health$INJURIES
health$casualty.bracket <- cut(health$total, breaks = 10)
us.boundary <- subset(health, LATITUDE < 4924 & LATITUDE > 2431 & LONGITUDE > 
    6657 & LONGITUDE < 12446)
us.boundary$date <- strptime(as.character(us.boundary$BGN_DATE), "%m/%d/%Y %H:%M:%S")
us.2011 <- us.boundary[format(us.boundary$date, "%Y") == "2011", ]

# include top 12 across all years in intersection as some top ranked event
# types do not have geo data points
us.top.6 <- us.2011[us.2011$EVTYPE %in% by.total$EVTYPE[1:12], ]

ggplot(us.top.6, aes(LONGITUDE, LATITUDE, alpha = casualty.bracket, size = casualty.bracket)) + 
    scale_x_reverse() + facet_wrap(~EVTYPE, nrow = 3, ncol = 2) + ggtitle("Locality and area of casualties from severe weather events in 2011") + 
    geom_point(color = "red")
```

![plot of chunk us_health_events_2011](figure/us_health_events_2011.png) 


*This plot does not include non-localized events such as heat waves.*

As can be seen, tornados stand out by their magnitude at specific locations, where events such as hail and thunderstorms are more dispersed.

### Economic impact

Floods caused the most economic damage in the same period 1950 to November, 2011.

The top 10 event types with respect to total economic cost are shown below.


```r
top10 <- by.total.cost[1:10, ]
ggplot(top10, aes(reorder(EVTYPE, total.cost), total.cost)) + xlab("Event type") + 
    ylab("Est.cost of property and crop damage ($B)") + ggtitle("Top 10 Events by US Economic Impact, 1950 - 2011") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip() + 
    geom_bar(stat = "identity", fill = "steelblue")
```

![plot of chunk top_10_damage](figure/top_10_damage.png) 



```r
kable(head(by.total.cost, 10))
```

|EVTYPE          |  total.property|  total.crop|  total.cost|
|:---------------|---------------:|-----------:|-----------:|
|FLOOD           |          167.53|       12.38|      179.91|
|HURRICANE       |           85.36|        5.52|       90.88|
|TORNADO         |           58.59|        0.42|       59.01|
|STORM SURGE     |           43.32|        0.00|       43.32|
|HAIL            |           15.73|        3.03|       18.76|
|DROUGHT         |            1.05|       13.97|       15.02|
|THUNDERSTORM    |           10.98|        1.27|       12.25|
|ICE STORM       |            3.94|        5.02|        8.96|
|FIRE            |            8.50|        0.40|        8.90|
|TROPICAL STORM  |            7.70|        0.68|        8.38|

**Amounts shown in billions of dollars.**

The Napa Valley flood in 2006 had a particularly high cost (billions of dollars), which could be explained as a result of damage to vineyards and its impact on the wine industry.
