# Omid 2016-05-06

# use >  dat  from ddcq-and-sole.R
library(nortest)

# Question 1: Is FPUE stable in time? -------------------------------------

  # 1.1: Draw a linear trnedline. Is the solpe sigbnificant?
model <- lm(data = dat, fpue ~ year)
plot(x = dat$year, y = dat$fpue)
abline(model)
model <- lm(data = dat, fpue_bel ~ year)
plot(x = dat$year, y = dat$fpue_bel)
abline(model)
  # --> Both trends significant --> FPUE very likely not constant.

  # 1.2:  Group the data in decades or other group, e.g. half of the data.
  #       When FPUE is stable, variance in all groups would be the same.
  #       Thus compare variance of the different groups in ANOVA. Or use
  #       Levene - Test.

  # [!!! Do later !!!]


# Question 2: Does FPUE depend on SSB and time? ----------------------

  # 2.1: As a first try, use simple linear model  ----------
model <- lm(data = dat, fpue ~ ssb + year)
  # we do not see heteroscedasticity.
  # Check residuals for normality. Note: If p>0.1, your residuals are normal.
  # Else you cannot use this model.
ad.test(resid(model))
cvm.test(resid(model))
  # You cannot use this model, because residuals are not normal.

  # 2.2: Use SSB only --------
model <- lm(data = dat, fpue ~ ssb)
summary(model)
x11()
plot(model)
ad.test(resid(model))
cvm.test(resid(model))
  # Residuals still not normal, model cannot be accepted.

  
  # Omid suggests: Stick to linear models.

  # 2.3: Threshold effect of SSB ---------------
  #       also called: segmented regression model with multiple predictors
  # Group data: From looking at FPUE ~ SSB, 
  #             it looks like the hockey stick breaks at around 55 000

model <- lm(data = dat, fpue ~ ssb)
model_seg <- segmented(obj = model, seg.Z = ~ ssb, psi = 55000)

summary(model)
plot(model)
  # --> Looks homoscedatic
  # Check normality of residuals
ad.test(resid(model))
cvm.test(resid(model))
  # --> Problematic residuals, not normal.
  # --> Threshold model is not the golden bullet.


  # 2.4 -----------------------
dat$ssb_difference <- NA
dat$ssb_difference <- c(NA, dat$ssb[1:(length(dat$ssb) - 1)])
dat$ssb_difference <- dat$ssb_difference - dat$ssb
dat$ssb_difference <- abs(dat$ssb_difference)

plot(dat$fpue ~ dat$ssb_difference)
model <- lm(data = dat, fpue ~ ssb_difference)
summary(model)
plot(model)
  # looks like residuals show heteroscedasticity.
  # --> Show it to some OTHER expert.
ad.test(resid(model))
cvm.test(resid(model))
  # residuals are normal distributed.
  # --> SSB changes predict FPUE better than pure SSB.

  # --> Significant and looking good (but for maybe heteroscedasticity)!
  # --> It may show that, at 11% (which is the R²), big changes in SSB
  #     cause negative FPUE.
  # --> That may mean that, at extreme values, the relationship is different
  #     from mid range values.
  # --> Always consider that the model only covers 13% of the variability.