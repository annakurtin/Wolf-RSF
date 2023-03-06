############ Plot Univariate Regression #########
library(ggplot2)


make_plot <- function(data,xaxis, title){
  ggplot(data, aes(x=xaxis, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial")) + labs(title = title)
}