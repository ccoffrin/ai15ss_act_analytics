print("hello ACT")

data = read.csv(url("http://www.data.act.gov.au/api/views/e9ux-7djy/rows.csv?accessType=DOWNLOAD"), stringsAsFactors=FALSE)

print(data[1,])

data$RecordDate = as.Date(data$RecordDate, "%d/%m/%Y")

#print(data$RecordDate
trim_paren <- function (x)  sub("\\)$", "", sub("^\\(", "", x))

locations = trim_paren(data$Location.1)
locations = strsplit(locations, "[,]")
locations = do.call(rbind, locations)
locations[,1] = as.numeric(locations[,1])
locations[,2] = as.numeric(locations[,2])
colnames(locations) = c("lat","lon")

data = cbind(data, locations, stringsAsFactors = FALSE)
data$lat = as.numeric(data$lat)
data$lon = as.numeric(data$lon)

print(data[1,])

data$ScientificName = factor(data$ScientificName)

s_name = unique(data$ScientificName)
print(s_name)



### Build functions for extracting the year and month from a date value
date_year = function(date){as.numeric(format(date, "%Y"))}
date_month = function(date){as.numeric(format(date, "%m"))}

print(c("Date span", format(min(data$RecordDate)), format(max(data$RecordDate))))

### Compute the number of years and months that the dataset spans
year_span = date_year(max(data$RecordDate)) - date_year(min(data$RecordDate))
month_span = (12-date_month(min(data$RecordDate))) + date_month(max(data$RecordDate)) + 12 * (year_span-2)
print(c("Date years and months", year_span, month_span))


pdf("date_year_hist.pdf", pointsize=14, width=8, height=6)
    hist(data$RecordDate, breaks=year_span, freq=TRUE, main="Vehicles Registered per Year in ACT", xlab="Year")
dev.off()

library(rworldmap)
newmap <- getMap(resolution = "high")

s_colors = rainbow(length(s_name))
print(s_colors)
print(as.numeric(data$ScientificName))

pdf("map.pdf", pointsize=14, width=8, height=8)

x_limits = c(min(data$lat),max(data$lat))
y_limits = c(min(data$lon),max(data$lon))


print(x_limits)
print(y_limits)

plot(newmap, xlim = x_limits, ylim = y_limits, asp = 1)
    points(data$lat, data$lon, col=s_colors[data$ScientificName])
dev.off()
