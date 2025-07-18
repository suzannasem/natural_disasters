# load packages
library(tidyverse)
library(car)
library(psych)
library(emmeans)
library(rties)
library(brms)
library(bayestestR)
library(BayesFactor)
library(flextable)

# load in data
clim <- read.csv("naturalDisaster.csv")

# scientific notation
options(scipen = 9) 

## Preliminary Analyses

str(clim)
# women, therapy, race are factors, so we need to recode them

# recoding women as factor
clim$womenF <- factor(clim$women, levels = c(0,1),labels = c("Men", "Women"))
# recoding therapy
clim$therapyF <- factor(clim$therapy, labels = c("No", "Yes"), levels = c(0,1))
# recoding race
clim$raceF <- factor(clim$race, labels = c("White", "Black or African American", "Asian or Asian American"),levels = c(0,1,2))

summary(clim)
# all quantitative variables seem to be in range based on data collection document

# subsetting to make fresh data set
clim2 <- subset(clim, select = c(pts,sleep,hurte,dassm,pssm,aaq,mindful,age,womenF,therapyF,raceF))

# Descriptive Statistics (for a table later)
## categorical variables
desc_women <- describeBy(clim2$pts, group = clim2$womenF, quant = c(.25, .75), mat = T)
desc_women <- desc_women[,c(2,4,5,6,7,16,17,10,11)]
desc_therapy <- describeBy(clim2$pts, group = clim2$therapyF, quant = c(.25, .75), mat = T)
desc_therapy <- desc_therapy[,c(2,4,5,6,7,16,17,10,11)]
desc_race <- describeBy(clim2$pts, group = clim2$raceF, quant = c(.25, .75), mat = T)
desc_race <- desc_race[,c(2,4,5,6,7,16,17,10,11)]

desc_women$Variable <- "Sex"
desc_therapy$Variable <- "Involvement in Therapy"
desc_race$Variable <- "Race"

desc_women <- desc_women[,c(10,1:9)]
desc_therapy <- desc_therapy[,c(10,1:9)]
desc_race <- desc_race[,c(10,1:9)]

combined_desc <- bind_rows(desc_women, desc_therapy, desc_race)
combined_desc <- flextable(combined_desc)
combined_desc
combined_desc %>%
  save_as_docx(path = "IHA4_table_f.docx")
# APA style table for quant vars
quant <- subset(clim2, select = c(pts,sleep,hurte,dassm,pssm,aaq,mindful,age))
pairs.panels(quant)
apaTables::apa.cor.table(quant, filename = "IHA4corTable.doc")

# checking distribution of outcome
histAll(clim2)
# PTS seems to be near-normal with more outliers than usual. 
# A student's t-distribution would be a good fit here.
# needed this to choose a good likelihood function for Bayesian
# Also: Predictor variables have outliers (pssm, aaq, mindful, age, dassm)
# but distributions are nearly normal and the outliers aren't too crazy

# to include in the write-up
hist(clim2$pts,main = 'Distribution of PTS Scores',xlab = 'PTS values')

# Correlations for continuous variables (checking that relationships are linear for GLM assumptions)

plot(pts ~ sleep, data = clim2) # positive linear
plot(pts ~ hurte, data = clim2) # negative linear
plot(pts ~ dassm, data = clim2) # strong positive linear
plot(pts ~ pssm, data = clim2) # strong positive linear
plot(pts ~ aaq, data = clim2) # strong positive linear
plot(pts ~ mindful, data = clim2) # negative linear
plot(pts ~ age, data = clim2) # curved (negative parabola)

# correlations for categorical variables

boxplot(pts ~ womenF, data = clim2) # pretty similar. men has outliers above & below, women has outliers below
boxplot(pts ~ therapyF, data = clim2) # "yes" group has lower pts values, but no has plenty of outliers above & below
boxplot(pts ~ raceF, data = clim2) # all seem similar, but white and asian have outliers on both sides, black has outliers above

# centering quantitative variables

summary(clim2)
# sleep, hurte, dassm, pssm, aaq, mindful, and age (basically all quant vars)
# would be better if centered (since no one in the sample would really sleep 0 hours a night)
# and in this sample, most individuals don't have 0 stress, hurricane trauma, inflexibility, etc
# centering the mean will make the intercept more informative of a typical participant

clim2$sleep_c <- clim2$sleep - mean(clim2$sleep, na.rm = TRUE)
clim2$hurte_c <- clim2$hurte - mean(clim2$hurte, na.rm = TRUE)
clim2$dassm_c <- clim2$dassm - mean(clim2$dassm, na.rm = TRUE)
clim2$pssm_c <- clim2$pssm - mean(clim2$pssm, na.rm = TRUE)
clim2$aaq_c <- clim2$aaq - mean(clim2$aaq, na.rm = TRUE)
clim2$mindful_c <- clim2$mindful - mean(clim2$mindful, na.rm = TRUE)
clim2$age_c <- clim2$age - mean(clim2$age, na.rm = TRUE)

## NHST Analysis ##

# Model 1: psychological inflexibility controlling for age, therapy, women, race
lm1 <- lm(pts ~ aaq_c + womenF + age_c + therapyF + raceF, data = clim2, na.action = na.exclude)

# checking assumptions
# from preliminary, we saw linear and additive relationship

# residuals normally distributed around zero: YES!
hist(lm1$residuals)

# check by group
# women looks good
qqPlot(lm1$residuals, groups = clim2$womenF)
# therapy looks good, though overpredicting at lower tail
qqPlot(lm1$residuals, groups = clim2$therapyF)
# race has normal residuals
qqPlot(lm1$residuals, groups = clim2$raceF)
# overall, looks good
qqPlot(lm1$residuals)

# checking constant variance of residuals
car::residualPlots(lm1,
                   pch=20, col="pink",
                   fitted = T,
                   ask = F, layout = c(1,2),
                   tests = F, quadratic = F)

# aaq doesn't show any pattern, but has some outlier residuals near the mean (aaq_c =  0)
# womenF has both men & women average residuals at 0 (good)
# constant variance roughly satisfied, though 139 and 137 are outliers for women
# age doesn't show any pattern - constant variance roughly satisfied (some outliers)
# therapyF roughly satisfies constant variance, except for 139 and 7 (outliers)
# although yes group has lower residuals on average
# race does not have roughly constant variance (NOT SATISFIED)

# i.e. constant variance roughly satisfied for most predictors, not race

# Interpret the model
summary(lm1)
# make a cute flextable for my report
lm1_flex <- as_flextable(lm1)
 lm1_flex %>%
  save_as_docx(path = "IHA4_lm1_table.docx")
# F(6,232) = 68.9, p < 0.01 -> Model is significant
# aaq_c is significant; gender, age, race, therapy are not (i.e. controlling for race/gender/therapy/age), psych inflexibility iy is a significant predictor of ptsd symptoms

 # interpretation redacted for data privacy

# omnibus test for categorical vars
Anova(lm1, type = 'III')
# sex, therapy involvement, and race are not significant

# effect size estimates
PartialEffectSizes(lm1)
# semi partial eta squared is 0.42135 EXPLAINS UNIQUE VARIANCE BY PREDICTOR
# adjusted R^2 is 0.6312 = 63% of variance

# Model 2: including mindfulness and sleep
lm2 <- lm(pts ~  aaq_c + womenF + age_c + therapyF + raceF + mindful_c + sleep_c, data = clim2, na.action = na.exclude)

# checking assumptions

# residuals normally distributed around 0? YES
hist(lm2$residuals)
# by group?
qqPlot(lm2$residuals, groups = clim2$womenF) # good
qqPlot(lm2$residuals, groups = clim2$raceF)# good, few outliers at tails 7,139 again
qqPlot(lm2$residuals, groups = clim2$therapyF) # good, but 7,139, 107, 162 outliers
# residuals are normally distributed around 0 - check!

# constant variance?
car::residualPlots(lm2,
                   pch=20, col="pink",
                   fitted = T,
                   ask = F, layout = c(1,2),
                   tests = F, quadratic = F)
# aaq_c is good
# roughly satisfied for womenF, though women have some more outliers
# satisfied for age
# roughly satisfied for therapy, though obs 7 and 139 again are outliers
# NOT satisfied for race (black/asian much less variable than white)
# satisfied for mindfulness
# satisfied for sleep
# OVERALL: roughly satisfied for most predictors, not race

# Interpreting (yay!)
summary(lm2)

lm2_flex <- as_flextable(lm2)
lm2_flex %>%
  save_as_docx(path = "IHA4_lm2_table.docx")

# F(8,230) = 51.55, p < 0.01 -> significant model
# only aaq_c is significant 

# categorical variables - omnibus test
Anova(lm2, type = 'III')
# none of our categorical variables are signficant

# effect size!
PartialEffectSizes(lm2)
# semi partial eta squared is 0.347
# adj rsq is 0.6295, still abt 63% variance

# comparing functions with anova
anova(lm1,lm2)
# F statistic is small (0.4579) and p = 0.6332, so model 2 is NOT better than model 1

### Bayesian Analysis

# default priors & student's t likelihood to CCOUNT FOR OUTLIERS

b1 <- brm(pts ~ 0 + Intercept + aaq_c + womenF + age_c + therapyF + raceF, data = clim2, family = "student",
          chains = 4, iter = 2000, seed = 123, sample_prior = T)
b2 <- brm(pts ~ 0 + Intercept + aaq_c + womenF + age_c + therapyF + raceF + sleep_c + mindful_c, data = clim2, family = "student",
          chains = 4, iter = 2000, seed = 123, sample_prior = T)
b_normal <- brm(pts ~ 0 + Intercept + aaq_c + womenF + age_c + therapyF + raceF, data = clim2, family = "gaussian",
                chains = 4, iter = 2000, seed = 123, sample_prior = T)

# Baye's Factor - does an effect exist?
bf1 <- lmBF(pts ~ aaq_c + womenF + age_c + therapyF + raceF, data = clim2)
bf2 <- lmBF(pts ~ aaq_c + womenF + age_c + therapyF + raceF + sleep_c + mindful_c, data = clim2)
bf1
bf2
# both models are really likely over the null, so we continue
# checking for convergence
summary(b1) # R-hats and ESS show convergence
summary(b2) # r hats & ESS good

plot(b1) # convergence met visually
plot(b2)

# next check: predictive accuracy
pp_check(b1, ndraws = 30) # model underpredicts at mean
pp_check(b2, ndraws = 30) # same
pp_check(b_normal, ndraws = 30) # normal distribution doesn't super change predictive accuracy (sensitivity test)
pp_check(b1, type = "hist", ndraws = 15, set.seed(293)) # distribution of pts has higher peak at mean
pp_check(b2, type = "hist", ndraws = 15, set.seed(293)) # same
# roughly satisfied, though models underpredict  pts at the mean

# normality of residuals

pp_check(b1, type = "error_hist", ndraws = 15, set.seed(293)) # roughly normal
pp_check(b1, type="error_hist_grouped", ndraws=5, group = "womenF", freq = T, set.seed(293))  # no variations by sex
pp_check(b1, type="error_hist_grouped", ndraws=5, group = "raceF", freq = T, set.seed(293))  # normal by race, though lower numbers of black/asian impact results
pp_check(b1, type="error_hist_grouped", ndraws=5, group = "therapyF", freq = T, set.seed(293))  # no variations by therapy

pp_check(b2, type = "error_hist", ndraws = 15, set.seed(293)) # roughly normal
pp_check(b2, type="error_hist_grouped", ndraws=5, group = "womenF", freq = T, set.seed(293))  # no variations by sex
pp_check(b2, type="error_hist_grouped", ndraws=5, group = "raceF", freq = T, set.seed(293))  # normal by race, though lower numbers of black/asian impact results
pp_check(b2, type="error_hist_grouped", ndraws=5, group = "therapyF", freq = T, set.seed(293))  # no variations by therapy

# constant variance - by group
# b1
# women - roughly satisfied, though women has a few outliers
ggplot(clim2, aes(x = womenF, y = residuals(b1)[,1]))+
  geom_point(size=2)+geom_jitter(width=0.1)
# therapy - check
ggplot(clim2, aes(x = therapyF, y = residuals(b1)[,1]))+
  geom_point(size=2)+geom_jitter(width = 0.1)
# race - NOT satisfied - more variance in white than black/asian, and a few outliers for asian
ggplot(clim2, aes(x = raceF, y = residuals(b1)[,1]))+
  geom_point(size=2)+geom_jitter(width=0.1)

plot(residuals(b1)) # overall, looks good
# b2
# women - roughly satisfied, though women has a few outliers
ggplot(clim2, aes(x = womenF, y = residuals(b2)[,1]))+
  geom_point(size=2)+geom_jitter(width=0.1)
# therapy - check (no group has outliers)
ggplot(clim2, aes(x = therapyF, y = residuals(b2)[,1]))+
  geom_point(size=2)+geom_jitter(width = 0.1)
# race - NOT satisfied - more variance in white than black/asian, and a few outliers for asian
ggplot(clim2, aes(x = raceF, y = residuals(b2)[,1]))+
  geom_point(size=2)+geom_jitter(width=0.1)

plot(residuals(b2)) # overall, looks good

# Interpreting Results - redacted for data privacy

summary(b1)
summary(b2)

# those HDIs look very close to zero on the upper end . . . let's see if it's practically significant
r <- rope(b1)
r # 9% in rope range .. not practically significant
plot(r)

r1 <- rope(b2)
r1 # 10% in rope!! not practically significant
plot(r1)
# EFFECT SIZE ESTIMATES

# Cohen's d for therapy (categorical)
# b1
summary(b1)$fixed[5,1]/summary(b1)$spec_pars[1,1] # -0.4501 = -0.45
# b2
summary(b2)$fixed[5,1]/summary(b2)$spec_pars[1,1] # -0.4499 = -0.45
# for both models,  ** involvement has a moderate effect, with those in ***
# experiencing moderately lower pts symptoms than those not

# COMPARING MODELS

# R^2 - total explained variance
bayes_R2(b1) # 64%
bayes_R2(b2) # 64.1%

# comparison with bayes factors
bf1/bf2 # model 1 is 79.75 times more likely with this data than model 2 (SE = 6.33%)
bf2/bf1 # sanity check that this is < 1, and it is!

# leave one out cross validation
b1 <- add_criterion(b1, criterion = "waic") 
b2 <- add_criterion(b2, criterion = "waic")
loo_compare(b1,b2, criterion = "waic")
# expected log predictive difference = -1.7 < 2, meaning models are statistically indistinguishable
