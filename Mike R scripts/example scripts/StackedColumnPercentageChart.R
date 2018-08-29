library(ggplot2)
library(reshape2)

data <- textConnection("Month,Series 1,Series 2,Series 3,Series 4
Jan,7.41,9.38,5.52,6.25
Feb,5.74,8.27,7.29,3.39
Mar,6.52,5.42,7.51,6.20
Apr,2.02,0.70,0.24,1.88
May,7.90,0.35,9.99,6.84
Jun,3.22,8.01,0.91,1.61
Jul,1.43,8.54,8.08,7.62
Aug,9.80,7.79,8.71,8.21
Sep,2.36,8.17,5.70,4.48
Oct,4.39,9.71,7.19,4.96
Nov,3.24,0.26,7.65,1.37
Dec,8.44,7.78,9.44,3.65
")

data <- read.csv(data, h=T)
orderedMonths <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
data$Month <- factor(data$Month, levels=orderedMonths)
data.lng <- melt(data, id=c("Month"))

p <- ggplot(aes(x=Month, weight=value, fill=variable), data=data.lng)
p + geom_bar(position='fill') + 
  scale_fill_discrete("Legend Title") + 
  labs(x="X Label", y="Y Label", title="An Example Stacked Column Percentage Chart")
# full output: http://www.yaksis.com/static/img/03/large/StackedColumnPercentageChart.png