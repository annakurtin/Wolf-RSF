###### MODEL BUILDING PART TWO: Eliminate Elevation from Bow Valley #####
# justification: elevation was colinear with the prey variables so I eliminated it from the model


#########Bow Valley Model Building ################
# Bow valley
bv_null <- glm(used~1, data = bvkde, family=binomial(logit))
## 1. With Deer, High Human Access, Goat, Sheep, Land Cover EXCEPT rockIce
bv_full1 <- glm(used~deer_w2+
                  DistFromHighHumanAccess2+
                  goat_w2+
                  sheep_w2+
                  openConif+
                  modConif+
                  closedConif+
                  mixed+
                  herb+
                  shrub+
                  burn+
                  alpine,
                data = bvkde,
                family = binomial(logit),
                na.action = "na.fail")

# stepwise model selection backwards
stepAIC(bv_full1, direction = "backward")
bv1Top_backward <- glm(formula = used ~ deer_w2 + DistFromHighHumanAccess2 + goat_w2 + 
                         sheep_w2 + modConif + closedConif + burn, family = binomial(logit), 
                       data = bvkde, na.action = "na.fail")
# AIC 893.2
vif(bv1Top_backward) # everything looks good here, all values under 3 and nothing approaching 10

# stepwise model selection forwards
stepAIC(bv_null, scope=list(upper=bv_full1, lower=bv_null), direction="forward")
bv1Top_forward <- glm(formula = used ~ deer_w2 + DistFromHighHumanAccess2 + goat_w2 + 
                        sheep_w2 + closedConif + modConif + burn, family = binomial(logit), 
                      data = bvkde)
# AIC 893.2
vif(bv1Top_forward) # this looks good 

# dredging
dredge_bv1 <- dredge(bv_full1, rank=BIC)
head(dredge_bv1, n = 10)
bv1Top_dredge <- glm(used~deer_w2+DistFromHighHumanAccess2 + closedConif + modConif + sheep_w2 + goat_w2, data = bvkde, family=binomial(logit))
# BIC 931.3
# took out burn 
plot(dredge_bv1)
vif(bv1Top_dredge)
# This looks good 

# top is bv1Top_dredge
# justification: chooseing the better of the stepAIC models and the dredge model 

##2. With Deer, Elevation, Sheep, Landcover EXCEPT rockIce
bv_full2 <- glm(used~deer_w2+
                  sheep_w2+
                  openConif+
                  modConif+
                  closedConif+
                  mixed+
                  herb+
                  shrub+
                  burn+
                  alpine,
                data = bvkde,
                family = binomial(logit),
                na.action = "na.fail")

# stepwise model selection backwards
stepAIC(bv_full2, direction = "backward")
bv2Top_backward <- glm(formula = used ~ deer_w2 + sheep_w2 + closedConif + mixed + 
                         herb + shrub + burn + alpine, family = binomial(logit), data = bvkde, 
                       na.action = "na.fail")
# AIC: 981.5
vif(bv2Top_backward) # looks good
# vif/variance inflation factor  is a measure of colinearity 

# stepwise model selection forwards
stepAIC(bv_null, scope=list(upper=bv_full2, lower=bv_null), direction="forward")
bv2Top_forward <- glm(formula = used ~ deer_w2 + sheep_w2 + closedConif + burn + 
                        modConif + alpine, family = binomial(logit), data = bvkde)
# AIC 980.2
vif(bv2Top_forward) # all looks good

# dredging
dredge_bv2 <- dredge(bv_full2, rank = BIC)
head(dredge_bv2, n = 10)
bv2Top_dredge <- glm(used~deer_w2+sheep_w2+closedConif, data=bvkde, family=binomial(logit))
# BIC 1006
vif(bv2Top_dredge) # good


bv_full3 <- glm(used~Elevation2+
                  sheep_w2+
                  openConif+
                  modConif+
                  closedConif+
                  mixed+
                  herb+
                  shrub+
                  burn+
                  alpine,
                data = bvkde,
                family = binomial(logit),
                na.action = "na.fail")


# stepwise model selection backwards
stepAIC(bv_full3, direction = "backward")
bv3Top_backward <- glm(formula = used ~ Elevation2 + sheep_w2 + modConif + burn, 
                       family = binomial(logit), data = bvkde, na.action = "na.fail")
# AIC: 831.3
vif(bv3Top_backward) # looks good

# stepwise model selection forwards
stepAIC(bv_null, scope=list(upper=bv_full3, lower=bv_null), direction="forward")
bv3Top_forward <- glm(formula = used ~ Elevation2 + burn + sheep_w2 + modConif, 
                      family = binomial(logit), data = bvkde)
# AIC 831.3
vif(bv2Top_forward) # all looks good

# dredging
dredge_bv3 <- dredge(bv_full3, rank = BIC)
head(dredge_bv3, n = 10)
bv3Top_dredge <- glm(used~Elevation2+sheep_w2+modConif+burn, data=bvkde, family=binomial(logit))
# BIC 857.1
bv3Top_dredge2 <- glm(used~Elevation2+sheep_w2+burn, data=bvkde, family=binomial(logit))
# BIC 857.3

vif(bv3Top_dredge) # good
vif(bv3Top_dredge2) # good
# they are all the same

# Second model looks better 
## justification: since we can't include Elevation and Distance to high human access in the same model, we screen out one in each model, then exclude any variables that are colinear 
# vif/variance inflation factor  is a measure of colinearity 
# top models for bow valley:
bv1Top_dredge
bv3Top_dredge
bv3Top_forward # this one also is the second top scoring BIC

# Evaluate these with ROC curves 


#########Red Deer Model Building##############
# Red deer
##1: elk, sheep, goat, distance to human access, land cover EXCEPT rockIce
rd_full1 <- glm(used~elk_w2+
                  DistFromHumanAccess2+
                  goat_w2+
                  sheep_w2+
                  openConif+
                  modConif+
                  closedConif+
                  mixed+
                  herb+
                  shrub+
                  burn+
                  alpine,
                data = rdkde,
                family = binomial(logit),
                na.action = "na.fail")

# stepwise model selection backwards
stepAIC(rd_full1, direction = "backward")
rd1Top_backward <- glm(formula = used ~ elk_w2 + DistFromHumanAccess2 + goat_w2 + 
                         sheep_w2 + openConif + modConif + closedConif + mixed + herb + 
                         shrub + burn + alpine, family = binomial(logit), data = rdkde)
# AIC 397.2
vif(rd1Top_backward) # everything looks good here, all values under 2 and nothing approaching 10
# VARIABLES WITH ISSUES: openConif, modConif, closedConif, shrub, burn

# stepwise model selection forwards
rd_null <- glm(used~1, data = rdkde, family=binomial(logit))
stepAIC(rd_null, scope=list(upper=rd_full1, lower=rd_null), direction="forward")
rd1Top_forward <- glm(formula = used ~ DistFromHumanAccess2 + elk_w2 + goat_w2 + 
                        burn + openConif + mixed + sheep_w2 + herb + shrub + closedConif + 
                        modConif + alpine, family = binomial(logit), data = rdkde)
# AIC 397.2
vif(rd1Top_forward) # issues with vif for this one too


# dredging
dredge_rd1 <- dredge(rd_full1, rank = BIC)
head(dredge_rd1, n = 5)
rd1Top_dredge1 <- glm(used~elk_w2 + DistFromHumanAccess2+burn+goat_w2+openConif,data=rdkde,family=binomial(logit))
vif(rd1Top_dredge1)
# vif is good now
# BIC is 439
#rd1old<- glm(formula = used ~ DistFromHumanAccess2 + goat_w2 + 
#                        sheep_w2 + openConif + modConif + closedConif + mixed + herb + 
#                        shrub + burn + alpine, family = binomial(logit), data = rdkde)
# only took out elk
# AIC 397.6
#rd1Top_dredge2 <- glm(formula = used ~ elk_w2 + DistFromHumanAccess2 + goat_w2 + 
#                        sheep_w2 + openConif + modConif + closedConif + mixed + herb + 
#                        shrub + burn + alpine, family = binomial(logit), data = rdkde)
#vif(rd1Top_dredge2)
# VARIABLES WITH ISSUES: openConif, modConif, closedConif, shrub, burn
# subset the top models with dAIC < 2
get.models(dredge_rd1, subset = delta<2)
# what does the +1 in the formula for the models mean?
# look for interactions of these models?
# try grouping openConif, modConif, and closedConif together


##2: sheep, goat, elevation, land cover EXCEPT rockIce
rd_full2 <- glm(used~Elevation2+
                  goat_w2+
                  sheep_w2+
                  openConif+
                  modConif+
                  closedConif+
                  mixed+
                  herb+
                  shrub+
                  burn+
                  alpine,
                data = rdkde,
                family = binomial(logit),
                na.action = "na.fail")

# stepwise model selection backwards
stepAIC(rd_full2, direction = "backward")
rd2Top_backward <- glm(formula = used ~ Elevation2 + goat_w2 + sheep_w2 + openConif + 
                         modConif + closedConif + mixed + herb + shrub + burn + alpine, 
                       family = binomial(logit), data = rdkde)
# AIC 397.2
vif(rd1Top_backward)
# PROBLEM VARIABLES: openConif, modConif, closedConif, shrub, burn

# stepwise model selection forwards
stepAIC(rd_null, scope=list(upper=rd_full2, lower=rd_null), direction="forward")
rd2Top_forward <-glm(formula = used ~ Elevation2 + burn + sheep_w2 + openConif + 
                       mixed + shrub + goat_w2 + herb, family = binomial(logit), 
                     data = rdkde)
# AIC 397.2
vif(rd2Top_forward) # no issues with these variables



#head(dredge_rdtest, n = 5)
dredge_rd2 <- dredge(rd_full2, rank = BIC)
head(dredge_rd2, n = 5)
rd2Top_dredge <- glm(used ~ Elevation2 + burn +  sheep_w2 + mixed + openConif, data=rdkde, family=binomial(logit))
# BIC: 416.1
vif(rd2Top_dredge)
# no issues - use BIC instead
#rank=BIC 
## justification:BIC does not consider K the 'penalty' function, but instead, considers K*log(n) where n is the number of rows of data, as the penalty function. Thus, it calculates a bigger penalty for larger datasets, which guards against overfitting
#
# dredging
#dredge_rdtest <- dredge(rd_full2)
# AICc: 
#test <- glm(used ~ alpine + burn+ closedConif + elk_w2 + goat_w2+ herb+ mixed + modConif + openConif + sheep_w2 + shrub, data=rdkde, family =binomial(logit))
#vif(test) # problem variables
#AIC 384
# kfold to validate
head(dredge_rd2, n = 10)
rd1Top_dredge1 <- glm(formula = used ~ DistFromHumanAccess2 + goat_w2 + 
                        sheep_w2 + openConif + modConif + closedConif + mixed + herb + 
                        shrub + burn + alpine, family = binomial(logit), data = rdkde)
# only took out elk
# AIC 397.6

# stepwise model selection backwards

## What is going on with Red Deer VIF variables?
# what is going on with these variables?
rdkde %>% ggplot() + geom_histogram(aes(x=openConif))
hist(rdkde$openConif)
hist(rdkde$modConif)
hist(rdkde$closedConif)
hist(rdkde$burn)
hist(rdkde$shrub)