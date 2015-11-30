library(ggplot2)
library(ggmap)

air <- read.csv("Air_Quality_Monitoring_Data.csv", header=T)
# air <- read.csv(url("https://www.data.act.gov.au/api/views/94a5-zqnn/rows.csv?accessType=DOWNLOAD"), header=T)
crashes <- read.csv("Cyclist_Crashes.csv", header=T)
# crashes <- read.csv(url("https://www.data.act.gov.au/api/views/n2kg-qkwj/rows.csv?accessType=DOWNLOAD"), header=T)

Canberra <- c(lon = 149.101738, lat = -35.261414)

# element:labels|visibility:off
# style: element:geometry.stroke|visibility:off
# style: feature:landscape|element:geometry|saturation:-100
# style: feature:water|saturation:-100|invert_lightness:true

p <- ggmap(get_googlemap(Canberra, zoom=13)) +
     geom_point(data=crashes, aes(x = LONGITUDE, y = LATITUDE, color=SEVERITY)) +
     scale_color_hue("Severity") +
     ggtitle("Bicycle crashes in ACT (2012 - now)")

pdf("crashes.pdf", height=10, width=10)
print(p)
dev.off()
