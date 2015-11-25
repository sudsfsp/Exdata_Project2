## Original Source of Dataset : http://www3.epa.gov/ttn/chief/eiinformation.html

## Unzip the dataset in working directory 
## Reading Data set from working directory
NEI <- readRDS("./summarySCC_PM25.rds")

## save in Png file
grDevices::png(filename = "./plot1.png", width = 480, height = 480)
dt <- transform(NEI, year= factor(year)) 

## extract sum of Emissions Pm2.5 according to year

sm <- tapply(dt$Emissions, dt$year, sum) 
sm <- sm/1000

## setting y scale range
ymax <- max(c(0, max(sm)*1.1)) 
ymin <- min(c(0, min(sm)*1.1)) 

## make barplot
brp <- barplot(sm, col = c("skyblue", "mistyrose", "lavender","lightcyan"), ylim=c(ymin, ymax), ylab = "PM2.5 Emissions (in thousand)", xlab = "Years")
text(x=brp, y=sm, label = round(as.numeric(sm),2), pos = 3, cex = 0.8, col = "brown")
text(x=brp, y=sm,label= paste(round((sm/sum(sm))*100,1), "%", sep = ""),pos =1, cex = 0.8)
lines(round(as.numeric(sm),2), lwd=2, col="purple")
title(main="Air Quatlity Standard, U.S")
dev.off()

## remove all objects because you can start fresh object in console.  
rm("dt", "sm", "brp", "ymin", "ymax")