library(ggplot2)
source("../functions/regression-functions.R")
# Download CSV Data ==============================================
data = read.csv("../data/Advertising.csv")


# Regression Analysis ============================================
#### simple linear regression of TV advertising on Sales ####
reg_TV.Sales = lm(Sales~TV, data = data)

#### simple linear regression of Radio advertising on Sales ####
reg_Radio.Sales = lm(Sales~Radio, data = data)

#### simple linear regression of Newspaper advertising on Sales ####
reg_Newspaper.Sales = lm(Sales~Newspaper, data = data)

#### multiple regression of TV, Radio, Newspaper on Sales ####
reg_All.Sales = lm(Sales~TV + Radio + Newspaper, data = data)


# Regression Summary ==============================================
#### simple linear regression summary with 5 summary statistics ####
reg_TV_summary = summary(reg_TV.Sales)
reg_Radio_summary = summary(reg_Radio.Sales)
reg_Newspaper_summary = summary(reg_Newspaper.Sales)

#### multiple regression summary with 5 summary statistics ####
reg_mul_summary = summary(reg_All.Sales)

# quality indices
reg_RSS_TV.Sales = residual_sum_squares(reg_TV.Sales)
reg_TSS_TV.Sales = total_sum_squares(reg_TV.Sales)

reg_RSS_Radio.Sales = residual_sum_squares(reg_Radio.Sales)
reg_RSS_Newspaper.Sales = residual_sum_squares(reg_Newspaper.Sales)


# Save Regression Objects
save(,file = "../../data/regression.RData")


# Plotting Data and Regression Line ===============================
# add fitted vaules into "data" dataframe
data = transform(data, fitted_TV.Sales = fitted(reg_TV.Sales))
data = transform(data, fitted_Radio.Sales = fitted(reg_Radio.Sales))
data = transform(data, fitted_Newspaper.Sales = fitted(reg_Newspaper.Sales))

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


########## Saving and Exporting ##########
# Save Plot
# PNG
ggsave("../images/scatterplot-tv-sales.png", scatterplot)

