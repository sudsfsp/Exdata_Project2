##Introduction##

Fine particulate matter (PM2.5) is an ambient air pollutant for which there is strong evidence that it is harmful to human health. In the United States, the Environmental Protection Agency (EPA) is tasked with setting national ambient air quality standards for fine PM and for tracking the emissions of this pollutant into the atmosphere. Approximatly every 3 years, the EPA releases its database on emissions of PM2.5. This database is known as the National Emissions Inventory (NEI). You can read more information about the NEI at the EPA National Emissions Inventory web site.

For each year and for each type of PM source, the NEI records how many tons of PM2.5 were emitted from that source over the course of the entire year. The data that you will use for this assignment are for 1999, 2002, 2005, and 2008.

##Data##

The data for this assignment are available from the course web site as a single zip file:

[Data for Peer Assessment [29Mb]][1]    

The zip file contains two files:

**PM2.5 Emissions Data (summarySCC_PM25.rds)**: This file contains a data frame with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. For each year, the table contains number of tons of PM2.5 emitted from a specific type of source for the entire year.

**Source Classification Code Table (Source_Classification_Code.rds)**: This table provides a mapping from the SCC digit strings in the Emissions table to the actual name of the PM2.5 source. The sources are categorized in a few different ways from more general to more specific and you may choose to explore whatever categories you think are most useful. For example, source �10100101� is known as �Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal�.

You can read each of the two files using the readRDS() function in R. For example, reading in each file can be done with the following code:

 

    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")


----------
as long as each of those files is in your current working directory (check by calling dir() and see if those files are in the listing).


  [1]: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
