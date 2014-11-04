library(ggplot2)
library(RCurl)
library(plyr)
library(RJSONIO)

#change this location to your own server log
serverlog <- "access.log"

raw <- read.table(serverlog)
raw <- raw[, c(1,4,6:10)]
names(raw) <- c('ip', 'datetime', 'request', 'http.code',
                'size', 'referrer', 'browser.os')
raw$datetime <- as.Date(raw$datetime, format="[%d/%b/%Y:%H:%M:%S")
keep <- raw[raw$http.code >= 200 & raw$http.code <= 299 &
              raw$datetime >= Sys.Date() - 7, ]

unique.lookup <- data.frame(ipaddress=unique(keep$ipaddress))

roughjson <- ddply(unique.lookup, "ipaddress", function(x) data.frame(result=getURL(paste0("ipinfo.io/", x$ipaddress))))
expandedjson <- c()
for(x in roughjson$result){
  expandedjson <- rbind(expandedjson, fromJSON(x))
}
expandedjson <- data.frame(expandedjson)
expandedjson <- merge(expandedjson, keep)

expandedjson$city <- gsub("^\\s+|\\s+$", "", expandedjson$city)
expandedjson$country <- gsub("^\\s+|\\s+$", "", expandedjson$country)

forplot <- expandedjson[c('ip', 'datetime', 'city', 'country', 'referrer')]
uniques <- forplot[!duplicated(forplot), ]
uniques$city.country <- paste(uniques$city, uniques$country, sep=",")
ggplot(data = uniques) + geom_bar(aes(x=datetime, fill=referrer), binwidth=1)

ggplot(data = uniques) + geom_bar(aes(x=city.country, fill=city.country), binwidth=1) + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) 
