library(ggplot2)
library(RCurl)
library(plyr)

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

citymap <- ddply(unique.lookup, "ipaddress", function(x) data.frame(city=getURL(paste0("ipinfo.io/", x$ipaddress, "/city"))))
countrymap <- ddply(unique.lookup, "ipaddress", function(x) data.frame(country=getURL(paste0("ipinfo.io/", x$ipaddress, "/country"))))

citymap$city <- gsub("^\\s+|\\s+$", "", citymap$city)
countrymap$country <- gsub("^\\s+|\\s+$", "", countrymap$country)
keep <- merge(keep, citymap)
keep <- merge(keep, countrymap)
uniques <- keep[!duplicated(keep), ]
uniques$city.country <- paste(uniques$city, uniques$country, sep=",")
ggplot(data = uniques) + geom_bar(aes(x=datetime, fill=referrer), binwidth=1)

ggplot(data = uniques) + geom_bar(aes(x=city.country, fill=city.country), binwidth=1) + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) 
