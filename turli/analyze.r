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

data = data[grep("Macropus",data$ScientificName),]

data$FirstName = gsub(" .*$", "", data$ScientificName) 
data$FirstName = factor(data$FirstName)
data$ScientificName = factor(data$ScientificName)

s_name = unique(data$ScientificName)
print(s_name)

f_name = unique(data$FirstName)
print(f_name)


### Build functions for extracting the year and month from a date value
date_year = function(date){as.numeric(format(date, "%Y"))}
date_month = function(date){as.numeric(format(date, "%m"))}

print(c("Date span", format(min(data$RecordDate)), format(max(data$RecordDate))))

### Compute the number of years and months that the dataset spans
year_span = date_year(max(data$RecordDate)) - date_year(min(data$RecordDate))
month_span = (12-date_month(min(data$RecordDate))) + date_month(max(data$RecordDate)) + 12 * (year_span-2)
print(c("Date years and months", year_span, month_span))

pdf("date_year_hist.pdf", pointsize=14, width=8, height=6)
    hist(data$RecordDate, breaks=year_span, freq=TRUE, main="Kangaroo Sightings per Year in ACT", xlab="Year")
dev.off()

library(ggmap)

pdf("map.pdf", pointsize=14, width=8, height=8)

   c_map = ggmap(get_map("Canberra", zoom=10)) + geom_point(data=data, aes(x=lon, y=lat, color=CommonName)) + scale_color_hue("Animal") + ggtitle("Kangaroo Sightings")
   print(c_map)

dev.off()
