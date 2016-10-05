# set working directory
setwd("~/stat159/hw/stat159-fall2016-hw02/code")

# load necessary packages and libraries
install.packages("ggplot2")
install.packages("reader")
install.packages("stats")
install.packages("ggplot2")
install.packages("broom")
library(ggplot2)
library(reader)
library(stats)
library(ggplot2)
library(broom)

# read data
data = read.csv("../data/Advertising.csv")

########## Regression Analysis ##########
# simple linear regression of TV advertising on Sales
reg_TV.Sales = lm(Sales~TV, data = data)


########## Regression Summary ##########
# full regression summary with 5 summary statistics
reg_full_summary = summary(reg_TV.Sales)

# summary of estimate, SE, t-stat, and p-value
reg_short_summary = tidy(reg_TV.Sales)

# quality indices
r_squared = reg_full_summary$r.squared
f_stat = as.numeric(reg_full_summary$fstatistic[1])
rse = reg_full_summary$sigma

reg_quality = rbind(r_squared, f_stat, rse)
colnames(reg_quality) = c("Value")

# Save Regression Objects
save(reg_TV.Sales, reg_full_summary, reg_short_summary, reg_quality, file = "../data/regression.RData")


########## Plotting Data and Regression Line ##########
# add fitted vaules into "data" dataframe
data = transform(data, fitted = fitted(reg_TV.Sales))

# plot
scatterplot = ggplot(data = data, aes(x = data$TV, y = data$Sales)) + 
  geom_point(aes(colour="#CC0000")) + # plot as points (scatterplot)
  theme(legend.position = "none") + # remove unnecessary legend
  geom_smooth(method = lm, aes(colour = "black")) + # regression line
  ggtitle("Scatterplot of TV Ads and Sales") +
  xlab("TV Ads") +
  ylab("Sales") +
  geom_segment(aes(x = data$TV, y = data$Sales, # add residual lines
                   xend = data$TV, yend = data$fitted,
                   color = "gray"))
# call on plot
scatterplot


########## Saving and Exporting ##########
# Save Plot
# PDF
pdf("../images/scatterplot-tv-sales.pdf")  
scatterplot 
dev.off() 
# PNG
png("../images/scatterplot-tv-sales.png")
scatterplot
dev.off()
