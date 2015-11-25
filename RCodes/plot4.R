## Original Source of Dataset : http://www3.epa.gov/ttn/chief/eiinformation.html

## Below command work as install automatically required package for run below code smoothly 
list.of.packages <- "ggplot2"
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## loading ggplot2 library
library(ggplot2)

## Unzip the dataset in working directory 
## Reading Data set from working directory
NEI <- readRDS("./summarySCC_PM25.rds")

## Reading source data set from working directory
SCC <- readRDS("./Source_Classification_Code.rds")

dt <- transform(NEI, year= factor(year))

## extract the coal combustion-related sources from source file.
sortDF <-  SCC[grep(pattern = "Coal", SCC$EI.Sector),]
data <- sortDF[,c(1, 4)]

## merge two dataset by SCC column 
mrg <- merge(dt, data, by="SCC")
tf <- transform(mrg, EI.Sector = factor(EI.Sector ), year= factor(year))

brp <- ggplot (tf, aes ( year, Emissions/1000,  color=EI.Sector, shape=EI.Sector, ymax=600))
brp + stat_summary (fun.y=sum, geom= "point")+ stat_summary (fun.y= sum, geom="line", aes (group=EI.Sector, linetype=EI.Sector), size=1)+
        theme(axis.line = element_line(size = 1, colour = "red", linetype = "dotted"))+ theme(panel.background = element_rect(fill = "White")) +
        stat_summary(aes(label=round(..y..,2)), fun.y=sum, geom="text", size=4, vjust = -0.5) + 
        stat_summary(aes(label= paste(round((..y../sum(..y..))*100,1), "%", sep = "")), fun.y=sum, geom="text", hjust=-0.7, vjust=-0.4, size=4, color="black") + 
        scale_y_continuous(breaks=seq(0, 600, 50)) +
        labs(y="PM2.5 Emissions (in thousand of tons)", x= "Years") + labs(title=("Air Quality Standard, U.S")) +
        scale_colour_discrete(name  ="Coal Source Type") + ## changing the name of legend title
        scale_shape_discrete(name  ="Coal Source Type") +
        scale_linetype_discrete(name  ="Coal Source Type")

## save png file with the help of ggsave() function in working directory.       
dir <- getwd()
imageFile <- file.path(dir, "plot4.png")
ggsave(imageFile, width = 10, height = 7, dpi=75)

## remove all objects because you can start fresh object in console. 
rm("dt","sortDF","data", "mrg", "tf", "brp", "SCC", "imageFile")
