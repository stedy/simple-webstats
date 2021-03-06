library(ggplot2)
library(RCurl)
library(plyr)
library(httr)
library(RJSONIO)

#change this location to your own server log
serverlog <- "access.log"
#add the IP of your dev environment so you aren't counting yourself
regular.ips <- c()

raw <- read.table(serverlog)
raw <- raw[, c(1,4,6:10)]
names(raw) <- c('ip', 'datetime', 'request', 'http.code',
                'size', 'referrer', 'browser.os')
raw$datetime <- as.Date(raw$datetime, format="[%d/%b/%Y:%H:%M:%S")

#Assume any HTTP code 200 is a visit, ignore all others
keep <- raw[raw$http.code >= 200 & raw$http.code <= 299 &
              raw$datetime >= Sys.Date() - 7 &
              raw$referrer != "-", ]

keep$basename <- sapply(keep$referrer, function(x) parse_url(x)$hostname)
unique.lookup <- data.frame(ip=unique(keep$ip))

roughjson <- ddply(unique.lookup, "ip", function(x) data.frame(result=getURL(paste0("ipinfo.io/", x$ip))))
expandedjson <- c()
for(x in roughjson$result){
  json_file <- fromJSON(x)

  json_file[sapply(json_file, is.null)] <- NA
  json_file <- unlist(json_file)

  expandedjson <- rbind(expandedjson, json_file)
  }

expandedjson <- merge(expandedjson, keep)

expandedjson$city <- gsub("^\\s+|\\s+$", "", expandedjson$city)
expandedjson$country <- gsub("^\\s+|\\s+$", "", expandedjson$country)
expandedjson <- expandedjson[expandedjson$ip %notin% regular.ips, ]

forplot <- expandedjson[c('ip', 'datetime', 'city', 'country', 'basename')]
uniques <- forplot[!duplicated(forplot), ]
uniques$city.country <- paste(uniques$city, uniques$country, sep=",")

png('referrer.png', width=800, height=500)
ggplot(data = uniques) + geom_bar(aes(x=datetime, fill=basename), binwidth=1) +
ggtitle("Referrer data")
dev.off()

png('city_country.png', width=800, height=500)
ggplot(data = uniques) + geom_bar(aes(x=city.country, fill=city.country), binwidth=1) + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  ggtitle("Visitor Location data")
dev.off()
