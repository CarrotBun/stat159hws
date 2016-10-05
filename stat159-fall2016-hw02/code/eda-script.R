# set working directory
setwd("~/stat159/hw/stat159-fall2016-hw02/code")

# setting up libraries and packages
install.packages("ggplot2")
install.packages("reader")
library(ggplot2)
library(reader)
data = read.csv("../data/Advertising.csv")

# Summary Statistics Function
my.summary = function(x, na.rm=TRUE){
  as.data.frame(cbind(Mean = mean(x, na.rm=na.rm),
                      SD = sd(x, na.rm=na.rm),
                      Median = median(x, na.rm=na.rm),
                      Min = min(x, na.rm=na.rm),
                      Max = max(x, na.rm=na.rm), 
                      N = length(x)), 
                row.names = NULL)
}

# Summary Statistics Table
stats_TV = my.summary(data$TV)
stats_Sales = my.summary(data$Sales)


# Histogram for TVs
hist_TV = ggplot(data = data, aes(data$TV)) +
	geom_histogram(aes(fill = ..count..),
	             binwidth = 15) +
	ggtitle("Histogram of TVs") +
	xlab("TVs") +
	ylab("Frequency")


# Histogram for Sales
hist_Sales = ggplot(data = data, aes(data$Sales)) +
	geom_histogram(aes(fill = ..count..),
	             binwidth = 5) +
	ggtitle("Histogram of Sales") +
	xlab("Sales") +
	ylab("Frequency")


# Save Histograms
pdf("../images/histogram-tv.pdf")
hist_TV
dev.off()

png("../images/histogram-tv.png")
hist_TV
dev.off()

pdf("../images/histogram-sales.pdf")
hist_Sales
dev.off()

png("../images/histogram-sales.png")
hist_Sales
dev.off()

# Producing eda-output.txt
sink("../data/eda-output.txt")
cat("Summary Statistics of TV ads on Sales\n\n")
stats_TV
cat("\n")
stats_Sales
sink()


