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

## Select Baltimore city from data set
dt <- subset(NEI, fips == "24510")
tf <- transform(dt, type = factor(type), year= factor(year))

## make plot with the help of point aes and line aes together 
## data show in plot according to "type" of source and year for Baltimore city.

brp <- ggplot (tf, aes ( year, Emissions,  color=type, shape=type))
brp + stat_summary (fun.y=sum, geom= "point")+ stat_summary (fun.y= sum, geom="line", aes (group=type, linetype=type), size=1)+
        theme(axis.line = element_line(size = 1, colour = "red", linetype = "dotted"))+ theme(panel.background = element_rect(fill = "White")) +
        stat_summary(aes(label=round(..y..,2)), fun.y=sum, geom="text", size=4, vjust = -0.5) + 
        stat_summary(aes(label= paste(round((..y../sum(..y..))*100,1), "%", sep = "")), fun.y=sum, geom="text", hjust=-0.7, vjust=-1.2, size=4, color="black")+
        labs(y="PM2.5 Emissions (in tons)", x= "Years") + labs(title=("Air Quality Standard, Baltimore city, U.S")) + scale_y_continuous(breaks=seq(0, 2200, 100)) +
        scale_colour_discrete(name  ="Source Type") + ## changing the name of legend title
        scale_shape_discrete(name  ="Source Type") +
        scale_linetype_discrete(name  ="Source Type")

## save png file with the help of ggsave() function in working directory.
dir <- getwd()
imageFile <- file.path(dir, "plot3.png")
ggsave(imageFile, width = 11, height = 8, dpi = 60)

## remove all objects because you can start fresh object in console. 
rm("dt", "dir", "brp", "tf", "imageFile")
