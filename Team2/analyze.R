dt = read.csv(url("http://www.data.act.gov.au/api/views/pj6q-ytqp/rows.csv?accessType=DOWNLOAD"))

dt$Project.Name <- NULL
dt$Tender.Schedule.Month <- NULL
dt$Design.Feasibility.estimated.call.tender.date <- NULL
dt$Construction.estimated.call.tender.date <- NULL
dt$Show.on.Map <- NULL
dt$Comments <- NULL
dt$Location.1 <- NULL

dt <- na.omit(dt)

colnames(dt) <- c("desc", "proj_location", "type", "investment", "category", "icon")

dt$investment <- sub('\\$','',as.character(dt$investment))
dt$investment <- as.numeric(dt$investment)
# order by location
newdt <- dt[c(2,4)]
location_count <- aggregate(. ~ proj_location, newdt, sum)
location_count = location_count[order(location_count$investment, decreasing=TRUE),]
location_count_top <- head(location_count, 10)
#boxplot((location_count$investment))

pdf("location.pdf", pointsize=14, width=8, height=6)
par(xaxt="n")
bp = barplot(location_count_top$investment, main="Total Investment based on location", axes=FALSE, axisnames=FALSE)
text(bp, par("usr")[3], labels=location_count_top$proj_location, srt=45, adj=c(1.1), xpd=TRUE)
axis(2)
dev.off()

#order by type
newdt <- dt[c(3,4)]
investment.type <- aggregate(. ~ type, newdt, sum)

pdf("type.pdf", pointsize=14, width=8, height=6)
par(xaxt="n")
bp = barplot(investment.type$investment, main="Total Investment based on type", axes=FALSE, axisnames=FALSE)
text(bp, par("usr")[3], labels=investment.type$type, srt=45, adj=c(1.1), xpd=TRUE)
axis(2)
dev.off()

#group by catergory
newdt <- dt[c(5,4)]
investment.cat <- aggregate(. ~ category, newdt, sum)

pdf("category.pdf", pointsize=14, width=8, height=6)
par(xaxt="n")
bp = barplot(investment.cat$investment, ylim=c(0,500000), main="Total Investment based on category", axes=FALSE, axisnames=FALSE)
text(bp, par("usr")[3], labels=investment.cat$category, srt=45, adj=c(1.1), xpd=TRUE)
axis(2)
dev.off()
