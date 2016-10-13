library(ggplot2)
require(lattice)

# Download CSV Data ==============================================
data = read.csv("../../data/Advertising.csv")

# Summary Statistics Function ====================================
my.summary = function(x, na.rm=TRUE){
  as.data.frame(cbind(Mean = mean(x, na.rm=na.rm),
                      SD = sd(x, na.rm=na.rm),
                      Median = median(x, na.rm=na.rm),
                      Min = min(x, na.rm=na.rm),
                      Max = max(x, na.rm=na.rm), 
                      N = length(x)), 
                row.names = NULL)
}

# Summary Statistics Table =======================================
stats_TV = my.summary(data$TV)
stats_Radio = my.summary(data$Radio)
stats_Newspaper = my.summary(data$Newspaper)
stats_Sales = my.summary(data$Sales)

# Correlation Matrix =============================================
corr_binary_matrix = cor(data)
corr_binary_matrix[corr_binary_matrix > 0] = 1
corr_binary_matrix[corr_binary_matrix < 0]f = -1

# Histograms ======================================================
# Histogram for TVs 
hist_TV = ggplot(data = data, aes(data$TV)) +
	geom_histogram(aes(fill = ..count..),
	             binwidth = 5) +
	ggtitle("Histogram of TVs") +
	xlab("TVs") +
	ylab("Frequency")

# Histogram for Radio
hist_Radio = ggplot(data = data, aes(data$Radio)) +
  geom_histogram(aes(fill = ..count..),
                 binwidth = 5) +
  ggtitle("Histogram of Radio") +
  xlab("TVs") +
  ylab("Frequency")

# Histogram for Newspaper
hist_Newspaper = ggplot(data = data, aes(data$Newspaper)) +
  geom_histogram(aes(fill = ..count..),
                 binwidth = 5) +
  ggtitle("Histogram of Newspaper") +
  xlab("TVs") +
  ylab("Frequency")

# Histogram for Sales
hist_Sales = ggplot(data = data, aes(data$Sales)) +
	geom_histogram(aes(fill = ..count..),
	             binwidth = 5) +
	ggtitle("Histogram of Sales") +
	xlab("Sales") +
	ylab("Frequency")

# Save Files =====================================================
# Save Binary Correlation Matrix
save(corr_binary_matrix, file = "../../data/correlation-matrix.RData")

# Save Charts
ggsave("../../images/histogram-tv.png", plot = hist_TV)
ggsave("../../images/histogram-radio.png", plot = hist_Radio)
ggsave("../../images/histogram-newspaper.png", plot = hist_Newspaper)
ggsave("../../images/histogram-sales.png", plot = hist_Sales)
ggsave("../../images/scatterplot-matrix.png", plot = pairs(data))

# eda-output.txt output ===========================================
# Producing eda-output.txt
sink("../../data/eda-output.txt")
cat("Statistics of TV Budget")
cat("\n")
stats_TV
cat("\n")
cat("Statistics of Radio")
cat("\n")
stats_Radio
cat("\n")
cat("Statistics of Newspaper")
cat("\n")
stats_Newspaper
cat("\n")
cat("Statistics of Sales")
cat("\n")
stats_Sales
cat("\n")
cat("Correlation Matrix")
cat("\n")
corr_matrix
cat("\n")
cat("Binary Correlation Matrix")
cat("\n")
corr_binary_matrix
cat("\n")
sink()


