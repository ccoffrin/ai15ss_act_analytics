print("Fetching Records")

### Read the data into a table
data = read.csv(url("http://www.data.act.gov.au/api/views/rux9-a9a9/rows.csv?accessType=DOWNLOAD"))

print(c("Collected Records", nrow(data)))

print("Records Columns")
print(colnames(data))

print("Record Example")
print(data[1,])

print("Type Values")
print(unique(data$Type))

print("Country Values")
print(unique(data$Country))


### Convert raw date data into the Date data type
#print(data$Start.Date) #check at raw start date format
data$Start.Date = as.Date(data$Start.Date, "%d/%m/%Y")
#print(data$Start.Date) #check at corrected start date format

### Remove records that did not have parse-able dates
#filter records with no start date
data = subset(data, !is.na(data$Start.Date))
print(c("Records with valid dates", nrow(data)))


### Compute the quantiles of the date data
start_date_range = quantile(data$Start.Date, c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1.0), type = 1)
print("Date Distribution")
print(start_date_range)
#print(nrow(data))

### Remove outliers.  i.e. those below 10% and above 90% quantiles
data = data[data$Start.Date >= start_date_range[2] & data$Start.Date <= start_date_range[6], ]
print(c("Records after removing date outliers", nrow(data)))

### Build functions for extracting the year and month from a date value
date_year = function(date){as.numeric(format(date, "%Y"))}
date_month = function(date){as.numeric(format(date, "%m"))}

print(c("Date span", format(min(data$Start.Date)), format(max(data$Start.Date))))

### Compute the number of years and months that the dataset spans
year_span = date_year(max(data$Start.Date)) - date_year(min(data$Start.Date))
month_span = (12-date_month(min(data$Start.Date))) + date_month(max(data$Start.Date)) + 12 * (year_span-2)
print(c("Date years and months", year_span, month_span))


### Open a PDF file for writing
pdf("date_year_hist.pdf", pointsize=14, width=8, height=6)
    
    ### Render a histogram of the start dates
    hist(data$Start.Date, breaks=year_span, freq=TRUE, main="Vehicles Registered per Year in ACT", xlab="Year")

### Close the PDF for writing    
dev.off()


pdf("date_month_hist.pdf", pointsize=14, width=8, height=6)
    
    hist(data$Start.Date, breaks=month_span, freq=TRUE, main="Vehicles Registered per Month in ACT", xlab="Month")
    
dev.off()


### Build a function for computing a mean with a sliding window
moving_mean = function(x,window=12){filter(x,rep(1/window,window), sides=2)}

data_startdate_count = aggregate(data, by=list(format(data$Start.Date,"%Y/%m")), FUN=length)
#print(data_startdate_count)
#print(moving_mean(data_startdate_count$Type))


### Render a PDF of Normalized ACT Vehicle Registrations over time
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


### Count the number of records by "Type"
data_type_count = aggregate(data, by=list(data$Type), FUN=length)

### Sort the records by "Type"
data_type_count = data_type_count[order(data_type_count$Type, decreasing=TRUE),]
#print(data_type_count)

pdf("type_hist.pdf", pointsize=14, width=8, height=6)
    
    par(xaxt="n")
    bp = barplot(data_type_count$Type, main="ACT Vehicle Type Distribution (1952-1959)", axes=FALSE, axisnames=FALSE)
    text(bp, par("usr")[3], labels=data_type_count$Group.1, srt=45, adj=c(1.1), xpd=TRUE)
    axis(2)

dev.off()



### Filter records by three specific types
data_special = subset(data, data$Type == "Motor Cycles" | data$Type == "Trucks" | data$Type == "Trailers")
print(c("Special Records",nrow(data_special)))


pdf("special_date_month_hist.pdf", pointsize=14, width=8, height=6)
    
    hist(data_special$Start.Date, breaks=month_span, freq=TRUE, main="Special Vehicles Registered per Month in ACT", xlab="Month")
    
dev.off()

