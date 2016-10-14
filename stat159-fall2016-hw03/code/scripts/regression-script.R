library(ggplot2)
source("../functions/regression-functions.R")

# Download CSV Data ==============================================
data = read.csv("../../data/Advertising.csv")


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

#### quality indices for the 4 regression summaries ####
## reg Sales on TV
reg_RSS_TV.Sales = residual_sum_squares(reg_TV.Sales)
reg_rsquared_TV.Sales = r_squared(reg_TV.Sales)
reg_Fstat_TV.Sales = f_statistic(reg_TV.Sales)

## reg Sales on Radio
reg_RSS_Radio.Sales = residual_sum_squares(reg_Radio.Sales)
reg_rsquared_Radio.Sales = r_squared(reg_Radio.Sales)
reg_Fstat_Radio.Sales = f_statistic(reg_Radio.Sales)

## reg Sales on Newspaper
reg_RSS_Newspaper.Sales = residual_sum_squares(reg_Newspaper.Sales)
reg_rsquared_Newspaper.Sales = r_squared(reg_Newspaper.Sales)
reg_Fstat_Newspaper.Sales = f_statistic(reg_Newspaper.Sales)

## reg Sales on TV, Radio, Newspaper
reg_RSS_All.Sales = residual_sum_squares(reg_All.Sales)
reg_rsquared_All.Sales = r_squared(reg_All.Sales)
reg_Fstat_All.Sales = f_statistic(reg_All.Sales)


# Save Regression Objects
save(reg_TV.Sales, reg_Radio.Sales, 
     reg_Newspaper.Sales, reg_All.Sales, 
     reg_TV_summary, reg_Radio_summary, 
     reg_Newspaper_summary, reg_mul_summary,
     file = "../../data/regression.RData")


# Plotting Data and Regression Line ===============================
# add fitted vaules into "data" dataframe
data = transform(data, fitted_TV.Sales = fitted(reg_TV.Sales))
data = transform(data, fitted_Radio.Sales = fitted(reg_Radio.Sales))
data = transform(data, fitted_Newspaper.Sales = fitted(reg_Newspaper.Sales))
data = transform(data, fitted_All.Sales = fitted(reg_All.Sales))

# scatter plots
scatter_TV = ggplot(data = data, aes(x = data$TV, y = data$Sales)) + 
  geom_point(aes(colour="#CC0000")) + # plot as points (scatterplot)
  theme(legend.position = "none") + # remove unnecessary legend
  geom_smooth(method = lm, aes(colour = "black")) + # regression line
  ggtitle("Scatterplot of TV Ads and Sales") +
  xlab("TV Ads") +
  ylab("Sales") +
  geom_segment(aes(x = data$TV, y = data$Sales, # add residual lines
                   xend = data$TV, yend = data$fitted_TV.Sales,
                   color = "gray"))

scatter_Radio = ggplot(data = data, aes(x = data$Radio, y = data$Sales)) + 
  geom_point(aes(colour="#CC0000")) + # plot as points (scatterplot)
  theme(legend.position = "none") + # remove unnecessary legend
  geom_smooth(method = lm, aes(colour = "black")) + # regression line
  ggtitle("Scatterplot of Radio Ads and Sales") +
  xlab("Radio Ads") +
  ylab("Sales") +
  geom_segment(aes(x = data$Radio, y = data$Sales, # add residual lines
                   xend = data$Radio, yend = data$fitted_Radio.Sales,
                   color = "gray"))

scatter_Newspaper = ggplot(data = data, aes(x = data$Newspaper, y = data$Sales)) + 
  geom_point(aes(colour="#CC0000")) + # plot as points (scatterplot)
  theme(legend.position = "none") + # remove unnecessary legend
  geom_smooth(method = lm, aes(colour = "black")) + # regression line
  ggtitle("Scatterplot of Newspaper Ads and Sales") +
  xlab("Newspaper Ads") +
  ylab("Sales") +
  geom_segment(aes(x = data$Newspaper, y = data$Sales, # add residual lines
                   xend = data$Newspaper, yend = data$fitted_Newspaper.Sales,
                   color = "gray"))

# multiple regression resids vs. fitted values
plot(reg_All.Sales, which = 1)
# multiple regression scale location
plot(reg_All.Sales, which = 3)
# normal QQ plot
plot(reg_All.Sales, which = 2)


# Saving and Exporting ==============================================
# Save Plot
# PNG
ggsave("../../images/scatterplot-tv-sales.png", scatter_TV)
ggsave("../../images/scatterplot-radio-sales.png", scatter_Radio)
ggsave("../../images/scatterplot-newspaper-sales.png", scatter_Newspaper)