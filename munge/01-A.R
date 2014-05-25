# Example preprocessing script.
summary <- ddply(repdata.data.StormData, .(EVTYPE), summarize, total.fatalities=sum(FATALITIES), total.injuries=sum(INJURIES))
summary <- subset(summary, total.fatalities > 0 | total.injuries > 0)
summary$total <- summary$total.fatalities + summary$total.injuries
fatality.weight <- 10
summary$total.weighted <- summary$total.fatalities * fatality.weight + summary$total.injuries
by.fatalities <- arrange(summary, desc(total.fatalities))
by.injuries <- arrange(summary, desc(total.injuries))
by.total <- arrange(summary, desc(total))
by.total.weighted <- arrange(summary, desc(total.weighted))

m <- c("H"=100, "K"=1000, "M"=1000000)
damage <- repdata.data.StormData
ind <- match(as.character(damage$PROPDMGEXP), names(m))
damage$property.cost <- damage$PROPDMG * m[ind]
damage$property.cost[is.na(damage$property.cost)] <- 0
ind <- match(as.character(damage$CROPDMGEXP), names(m))
damage$crop.cost <- damage$CROPDMG * m[ind]
damage$crop.cost[is.na(damage$crop.cost)] <- 0
damage <- subset(damage, property.cost > 0 | crop.cost > 0)
damage.summary <- ddply(damage, .(EVTYPE), summarize, total.property=sum(property.cost), total.crop=sum(crop.cost))
damage.summary$total.cost <- damage.summary$total.property + damage.summary$total.crop
by.total <- arrange(damage.summary, desc(total.cost))
head(by.total, 10)

us.boundary <- subset(damage, LATITUDE < 4924 & LATITUDE > 2431 & LONGITUDE > 6657 & LONGITUDE <  12446 & property.cost > 100000)
#us.boundary <- subset(damage, LATITUDE > 2431 & LATITUDE < 4924 & LONGITUDE > 6657 & LONGITUDE <  12446)
us.boundary$cost.bracket <- cut(us.boundary$property.cost, breaks=10)
us.boundary$date <- strptime(as.character(us.boundary$BGN_DATE), "%m/%d/%Y %H:%M:%S")
#us.2011 <- us.boundary[format(us.boundary$date, "%Y") == "2011", ]
us.top.6 <- us.boundary[us.boundary$EVTYPE %in% by.total$EVTYPE[1:6], ]
ggplot(us.top.6, aes(LONGITUDE, LATITUDE, alpha=cost.bracket, size=cost.bracket)) +
  scale_x_reverse() +
  facet_wrap(~ EVTYPE, nrow=2, ncol=2) +
  geom_point(color="steelblue")

health <- repdata.data.StormData
health$total <- health$FATALITIES + health$INJURIES
health$total.weighted <- health$FATALITIES * fatality.weight + health$INJURIES
health$fatality.bracket <- cut(health$FATALITIES, breaks=10)
health$injury.bracket <- cut(health$INJURIES, breaks=10)
health$harm.bracket <- cut(health$total, breaks=10)
health$weighted.harm.bracket <- cut(health$total.weighted, breaks=10)
us.boundary <- subset(health, LATITUDE < 4924 & LATITUDE > 2431 & LONGITUDE > 6657 & LONGITUDE <  12446)
us.boundary$date <- strptime(as.character(us.boundary$BGN_DATE), "%m/%d/%Y %H:%M:%S")
us.2011 <- us.boundary[format(us.boundary$date, "%Y") == "2011", ]
us.top.6 <- us.2011[us.2011$EVTYPE %in% by.total$EVTYPE[1:10], ]
ggplot(us.top.6, aes(LONGITUDE, LATITUDE, alpha=weighted.harm.bracket, size=weighted.harm.bracket)) +
  scale_x_reverse() +
  facet_wrap(~ EVTYPE, nrow=3, ncol=2) +
  geom_point(color="red")
ggplot(us.top.6, aes(LONGITUDE, LATITUDE, alpha=harm.bracket, size=harm.bracket)) +
  scale_x_reverse() +
  facet_wrap(~ EVTYPE, nrow=3, ncol=2) +
  geom_point(color="red")
ggplot(us.top.6, aes(LONGITUDE, LATITUDE, alpha=fatality.bracket, size=fatality.bracket)) +
  scale_x_reverse() +
  facet_wrap(~ EVTYPE, nrow=3, ncol=2) +
  geom_point(color="red")
ggplot(us.top.6, aes(LONGITUDE, LATITUDE, alpha=injury.bracket, size=injury.bracket)) +
  scale_x_reverse() +
  facet_wrap(~ EVTYPE, nrow=3, ncol=2) +
  geom_point(color="red")
