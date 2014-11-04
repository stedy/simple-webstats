library(ggplot2)
library(RCurl)
library(plyr)
library(RJSONIO)

#change this location to your own server log
serverlog <- "access.log"

raw <- read.table(serverlog)
raw <- raw[, c(1,4,6:10)]
names(raw) <- c('ipaddress', 'datetime', 'request', 'http.code',
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

merged.json <- merge(roughjson, expandedjson)

merged.json$city <- gsub("^\\s+|\\s+$", "", merged.json$city)
merged.json$country <- gsub("^\\s+|\\s+$", "", merged.json$country)

uniques <- merged.json[!duplicated(merged.json), ]
uniques$city.country <- paste(uniques$city, uniques$country, sep=",")
ggplot(data = uniques) + geom_bar(aes(x=datetime, fill=referrer), binwidth=1)

ggplot(data = uniques) + geom_bar(aes(x=city.country, fill=city.country), binwidth=1) + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) 

temp <- c()
for(x in roughjson$result){
  temp <- rbind(temp, fromJSON(x))
}
