data=read.csv(url('http://www.data.act.gov.au/api/views/q8rt-q8cy/rows.csv?accessType=DOWNLOAD'))

###Sector barplot
pdf("sector_barplot.pdf", pointsize=14, width=8, height=6)
barplot(table(data$Sector))
dev.off()

data$Suburb=tolower(data$Suburb)

selected_suburbs=names(table(data$Suburb)[table(data$Suburb)>2])
ordered_suburbs=names(sort(table(data$Suburb)[table(data$Suburb)>2],decreasing=TRUE))
suburb_data=table(data[data$Suburb %in% selected_suburbs,c("Sector","Suburb")])

pdf("suburb_sector_barplot.pdf", pointsize=14, width=8, height=6)
barplot(suburb_data[,ordered_suburbs], xlab = "Suburb",ylab = "Number of schools")
dev.off()

lat_long_list=strsplit(as.character(data$Location.1),",")
lat_long_matrix=matrix(unlist(lat_long_list),ncol=2,byrow=TRUE)
lat_long_matrix=gsub("[() ]","",lat_long_matrix)
lat_long_df=data.frame(lat=as.numeric(lat_long_matrix[,1]),long=as.numeric(lat_long_matrix[,2]),Sector=data$Sector)

#pdf("schools_location_plot.pdf", pointsize=14, width=8, height=6)
#plot(149:150,-36:-35,type='n')
#points(lat_long_df$long,lat_long_df$lat)
#dev.off()

library(ggmap)  

pdf("schools_location_map.pdf", pointsize=14, width=8, height=6)
canberra_map=qmap(location="canberra",zoom=11)
canberra_map + geom_point(aes(x=long, y=lat,colour=Sector), data=lat_long_df,size=5) + theme(legend.text=element_text(size=20),legend.title=element_text(size=20))
dev.off()

pdf("schools_density_map.pdf", pointsize=14, width=8, height=6)
canberra_map + 
geom_point(aes(x=long, y=lat), data=lat_long_df,col="blue",size=5) +
stat_density2d(data = lat_long_df, 
               aes(x = long, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
               bins = 16, geom = "polygon") + scale_fill_gradient(low = "white", high = "blue") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) + theme(legend.position = "none")
dev.off()