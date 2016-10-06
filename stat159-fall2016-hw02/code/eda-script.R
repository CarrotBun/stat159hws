# set working directory
setwd("~/stat159/hw/stat159-fall2016-hw02/code")

# setting up libraries and packages
#install.packages("ggplot2", repos="http://cran.rstudio.com/")
#install.packages("reader", repos="http://cran.rstudio.com/")
library(ggplot2)

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
ggsave("../images/histogram-tv.pdf", plot = hist_TV)
ggsave("../images/histogram-tv.png", plot = hist_TV)


ggsave("../images/histogram-sales.pdf", plot = hist_Sales)
ggsave("../images/histogram-sales.png", plot = hist_Sales)

# Producing eda-output.txt
sink("../data/eda-output.txt")
cat("Statistics of TV Budgets")
cat("\n")
stats_TV
cat("\n")
cat("Statistics of Product Sales")
cat("\n")
stats_Sales
sink()


