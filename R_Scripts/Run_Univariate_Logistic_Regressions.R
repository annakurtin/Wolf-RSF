########### Looking at univariate effects #########


univ_logregs <- function(data){

  # effect of elevation
  elev_univmod <- glm(used ~ Elevation2, 
                      family = binomial(logit),
                      data = data)
  # plot it
  ggplot(data, aes(x=Elevation2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial")) + labs(title = "Elevation Univariate Log Reg")
  
  
  # effect of human access
  human_univmod <- glm(used ~ DistFromHumanAccess2, 
                       family = binomial(logit),
                       data = data)
  # plot it
  ggplot(data, aes(x=DistFromHumanAccess2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial")) + labs(title = "Human Access Univariate Log Reg")
  
  # effect of high human access
  highhuman_univmod <- glm(used ~ DistFromHighHumanAccess2, 
                           family = binomial(logit),
                           data = data)
  # plot it
  ggplot(data, aes(x=DistFromHighHumanAccess2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial")) + labs(title = "High Human Access Univariate Log Reg")
  
  # ungulate H.S.I. models
  # Sheep
  sheep <- glm(used ~ sheep_w2, family=binomial(logit), data=data)
  # plot it
  ggplot(data, aes(x=sheep_w2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial")) + labs(title = "Sheep Univariate Log Reg")
  
  # Deer
  deer <- glm(used ~ deer_w2, family=binomial(logit), data=data)
  # plot it
  ggplot(data, aes(x=deer_w2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial")) + labs(title = "Deer Univariate Log Reg")
  
  # Elk
  elk <- glm(used ~ elk_w2, family=binomial(logit), data=data)
  # plot it
  ggplot(data, aes(x=elk_w2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial")) + labs(title = "Elk Univariate Log Reg")
  
  # Moose
  moose <- glm(used ~ moose_w2, family=binomial(logit), data=data)
  # plot it
  ggplot(data, aes(x=moose_w2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial")) + labs(title = "Moose Univariate Log Reg")
  
  # Goat
  goat <- glm(used ~ goat_w2, family=binomial(logit), data=data)
  # plot it
  ggplot(data, aes(x=goat_w2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial")) + labs(title = "Goat Univariate Log Reg")
  
  # make a table of coefficients from the model 
  models = rbind(summary(elev_univmod)$coefficients[,1:2], summary(human_univmod)$coefficients[,1:2], summary(highhuman_univmod)$coefficients[,1:2], summary(sheep)$coefficients[,1:2], summary(goat)$coefficients[,1:2], summary(elk)$coefficients[,1:2], summary(moose)$coefficients[,1:2], summary(deer)$coefficients[,1:2])
  # Name your models
  modelnames = c("elev","disthha", "distacc", "sheep", "goat", "elk", "moose", "deer")
  # Now put all of your estimates in a pretty table with names that you'll remember!
  estimates.all = matrix(models, nrow=2*length(modelnames), ncol=2, dimnames = list(paste(rep(modelnames, each=2),c("intercept", "coefficient")), c("B", "SE")))
  # Gives you a table with b and standard error
  xtable(estimates.all)
  # export it as a latec - Jordan is presenting on flextable in the R group this semester 
  # xtable and flextable allows you to export tables to word
  return(estimates.all)
}

# doesn't plot things within the function