print("Fetching Records")

data = read.csv(url("http://www.data.act.gov.au/api/views/rux9-a9a9/rows.csv?accessType=DOWNLOAD"))

print(c("Collected Records",nrow(data)))

print("Records Columns")
print(colnames(data))

print("Record Example")
print(data[1,])

print("Type Values")
print(unique(data$Type))

print("Country Values")
print(unique(data$Country))


#print(data$Start.Date) #check at raw start date format
data$Start.Date = as.Date(data$Start.Date, "%d/%m/%Y")
#print(data$Start.Date) #check at corrected start date format

#filter records with no start date
data = subset(data, !is.na(data$Start.Date))
print(c("Records with valid dates",nrow(data)))


start_date_range = quantile(data$Start.Date, c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1.0), type = 1)
print(start_date_range)
print(nrow(data))

data = data[data$Start.Date >= start_date_range[2] & data$Start.Date <= start_date_range[6], ]
print(c("Records after removing date outliers",nrow(data)))

date_year = function(date){as.numeric(format(date, "%Y"))}
date_month = function(date){as.numeric(format(date, "%m"))}

print(c("Date span",format(min(data$Start.Date)), format(max(data$Start.Date))))

year_span = date_year(max(data$Start.Date)) - date_year(min(data$Start.Date))
month_span = (12-date_month(min(data$Start.Date))) + date_month(max(data$Start.Date)) + 12 * (year_span-2)
print(c("Date years and months",year_span,month_span))


pdf("date_year_hist.pdf", pointsize=14, width=8, height=6)
    
    hist(data$Start.Date, breaks=year_span, freq=TRUE, main="Vehicles Registered per Year in ACT", xlab="Year")
    
dev.off()

pdf("date_month_hist.pdf", pointsize=14, width=8, height=6)
    
    hist(data$Start.Date, breaks=month_span, freq=TRUE, main="Vehicles Registered per Month in ACT", xlab="Month")
    
dev.off()


moving_mean = function(x,window=12){filter(x,rep(1/window,window), sides=2)}

data_startdate_count = aggregate(data, by=list(format(data$Start.Date,"%Y/%m")), FUN=length)
#print(data_startdate_count)
#print(moving_mean(data_startdate_count$Type))

pdf("reg_cycle.pdf", pointsize=14, width=8, height=6)
    #data_startdate_count$Group.1
    date_count = data_startdate_count$Type
    date_count_moving_mean = moving_mean(data_startdate_count$Type)
    
    delta = date_count - date_count_moving_mean

    x_limits = c(1, nrow(data_startdate_count))
    y_limits = c(min(delta, date_count_moving_mean, na.rm = TRUE), max(delta, date_count_moving_mean, na.rm = TRUE))

    plot(x_limits, y_limits, type="n", main="Normalized ACT Vehicle Registrations Distribution (1952-1959)", xlab="Time (months)", ylab="Registrations") #, axes=FALSE, axisnames=FALSE)

    points(1:length(delta), delta, type="l", lwd=2, col="red")
    points(1:length(date_count_moving_mean), date_count_moving_mean, type="l", lwd=2, col="blue")

    legend("topleft", c("12 Month Moving Average","Delta Off Average"), lwd=c(2,2), col=c("red","blue"), bg="white")
dev.off()


data_type_count = aggregate(data, by=list(data$Type), FUN=length)
data_type_count = data_type_count[order(data_type_count$Type, decreasing=TRUE),]
#print(data_type_count)

pdf("type_hist.pdf", pointsize=14, width=8, height=6)
    
    par(xaxt="n")
    bp = barplot(data_type_count$Type, main="ACT Vehicle Type Distribution (1952-1959)", axes=FALSE, axisnames=FALSE)
    text(bp, par("usr")[3], labels=data_type_count$Group.1, srt=45, adj=c(1.1), xpd=TRUE)
    axis(2)

dev.off()



data_special = subset(data, data$Type == "Motor Cycles" | data$Type == "Trucks" | data$Type == "Trailers")
print(c("Special Records",nrow(data_special)))


pdf("special_date_month_hist.pdf", pointsize=14, width=8, height=6)
    
    hist(data_special$Start.Date, breaks=month_span, freq=TRUE, main="Special Vehicles Registered per Month in ACT", xlab="Month")
    
dev.off()

