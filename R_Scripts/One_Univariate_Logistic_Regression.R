########### Looking at one univariate effect #########

logreg <- function(data, variable, variableName){
  
  # effect of elevation
  univmod <- glm(used ~ variable, 
                      family = binomial(logit),
                      data = data)
  print(summary(univmod))
  ggplot(data, aes(x=variable, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial")) + labs(title = paste(variableName,"Univariate Log Reg"))
  return(reg_plot)
}

# old print(summary(univmod)$coefficients[,1:2])