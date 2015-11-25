
## Note : The term "Motor vehicle" source includes road vehicles, such as automobiles, vans,motorcycles,
## and trucks, as well as off-road vehicles such as self-propelled construction and farming equipment.
## More information: http://www.justice.gov/usam/criminal-resource-manual-1303-definitions-motor-vehicle-aircraft-security


## Below command work as install automatically required package for run below code smoothly 
list.of.packages <- "ggplot2"
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Reading Data set from working directory
NEI <- readRDS("./summarySCC_PM25.rds")

## Reading source data set from working directory
SCC <- readRDS("./Source_Classification_Code.rds")

## loading ggplot2 library
library(ggplot2)

## Select Baltimore city from data set
dt <- subset(NEI, fips == "24510")
tf <- transform(dt, year= factor(year))

## extract the motor vehicle emission sources from source file.
sortDF <-   SCC[grep(pattern = "Highway|Off-highway", SCC$SCC.Level.Two),]
data <- transform(sortDF, SCC= as.character(SCC))
New_dt <- data[, c(1,2)]

## merge two dataset by SCC column 
mrg <- merge(tf,New_dt, by="SCC")

## Select Los Anageles county from data set
dt1 <- subset(NEI, fips == "06037")
tf1 <- transform(dt1, type = factor(type), year= factor(year))


## merge two dataset by SCC column 
mrg1 <- merge(tf1, New_dt, by="SCC")

## merge two dataset rows from mrg and mrg1 dataset
new_merge <- rbind(mrg, mrg1)
new_merge$fips <- factor(new_merge$fips, labels = c("Los Anageles", "Baltimore"))
new_merge <- transform(new_merge, fips=factor(fips))

brp <- ggplot(new_merge, aes(x = year, y = Emissions, fill = fips, ymax=8000))
brp + stat_summary (fun.y=sum, geom= "bar", position = position_dodge()) +
      stat_summary(aes(label=round(..y..,2)), fun.y=sum, geom="text", size=4, position=position_dodge(width=0.9), vjust=-0.25) +
      scale_y_continuous(breaks=seq(0, 8000, 1000)) + theme(panel.background = element_rect(fill = "White")) +
      labs(y="PM2.5 Emissions (in tons) ",x= "Years")+ scale_fill_discrete(name="County") + 
      labs (title= ("Air Quality Standard, U.S (Source- Motor Vehicle)")) 

## save png file with the help of ggsave() function in working directory.       
dir <- getwd()
imageFile <- file.path(dir, "plot6.png")
ggsave(imageFile, width = 7, height = 5, dpi=80)

## remove all objects because you can start fresh object in console. 
rm("dt", "dt1", "new_merge","sortDF","data","New_dt", "mrg", "mrg1", "brp", "tf", "tf1", "imageFile", "dir", "SCC")



