# check relationship  FPUEsol ~ Fple
# and Fsol ~ Fple
library(mgcv)
library(nortest)

working_directory <- 'D:\\OfflineOrdner\\Promotion III -- Technological Creep\\10--Sole\\ddcq and sole'
dat_with_ple <- read.csv(file = paste0(working_directory, '\\output\\dat_with_ple.csv'), stringsAsFactors = FALSE)
str(dat_with_ple)
dat_with_ple$ssb <- as.numeric(as.character(dat_with_ple$ssb))
dat_with_ple$ssb_ple <- as.numeric(as.character(dat_with_ple$ssb_ple))


# plot FPUEsol ~ Fple  and  Fsol ~ Fple ----
x11()
par(mfrow=c(2,1))
plot(dat_with_ple$fpue ~ dat_with_ple$mean.f_ple)
plot(dat_with_ple$mean.f ~ dat_with_ple$mean.f_ple)
dev.off()

# check shape Fsol ~ Fple with GAM ----
dat1 <- dat_with_ple[!is.na(dat_with_ple$fpue),]
model <- gam(data = dat1,
             mean.f ~ s(mean.f_ple), 
             family = gaussian(link = 'identity'))
x11()
gam.check(model)  # --> Doesn't look ok.
plot(dat1$mean.f ~ dat1$year)  # obs and predicted time series
points(predict(model) ~ dat1$year, col = 'red')  # -->flat til 2005
ad.test(model$residuals)
cvm.test(model$residuals)  # --> resids not normally distributed
model_gaussian <- model
rm(model)   # --> reject model
dev.off()

  # maybe Gamma family works better?
model <- gam(data = dat1,
              mean.f ~ s(mean.f_ple), 
              family = Gamma(link = 'identity'))
x11()
gam.check(model)  # --> Still doesn't look ok.
plot(dat1$mean.f ~ dat1$year)  # obs and predicted time series
points(predict(model) ~ dat1$year, col = 'red')  # --> still flat til 2005
acf(resid(model))  # --> residuals autocorrelated
AIC(model, model_gaussian) # --> still non-normal resids (tests not shown), not really better AIC-wise
summary(model)
plot(model)   # --> model suggests increasing Fsol with Fple,
              #     which is plausible, as both stem from higher BT effort
  # --> We need effort in the GAM.
rm(model)
dev.off()


# include effort in GAM ----
model <- gam(data = dat1,
             mean.f ~ s(mean.f_ple) + s(effort_rel_2003), 
             family = gaussian(link = 'identity'))
x11()
gam.check(model)  # --> doesn't look ok
ad.test(resid(model))  # --> residuals not normally distributed
model_gaussian <- model
  # try Gamma 
model <- gam(data = dat1,
             mean.f ~ s(mean.f_ple) + s(effort_rel_2003), 
             family = Gamma(link = 'identity'))
gam.check(model)  # --> still not ok
ad.test(resid(model))  # --> residuals still not normally distributed
AIC(model, model_gaussian)  # --> ...but Gamma is the better of the bad
acf(resid(model))  # --> residuals autocorrelated
summary(model)
par(mfrow=c(1,2))
plot(model)  # --> model predicts linear, though not significant effect of Fple
model_with_autocor <- model
dev.off()

# remove autocorrelation from residuals ----
model <- gamm(data = dat1, formula = mean.f ~ s(mean.f_ple) + s(effort_rel_2003),
               family = Gamma(link = 'identity'),
               correlation = corARMA(form = ~ year, p = 1))
x11()
gam.check(model$gam)  # --> looks awful
ad.test(resid(model$lme))
ad.test(resid(model$gam))  # --> residuals superabnormal
acf(resid(model$lme))
acf(resid(model$gam))  # --> ...and substantially autocorrealted
plot(model$gam)
summary(model$gam)  # --> Fsol is increasing at lower Fple, than flattens off.

model_corARMA <- model


  # maybe corAR1 works better?
model <- gamm(data = dat1, formula = mean.f ~ s(mean.f_ple) + s(effort_rel_2003),
              family = Gamma(link = 'identity'),
              correlation = corAR1(form = ~ year))
gam.check(model$gam)  # --> still terrible
AIC(model$lme, model_corARMA$lme) # --> no difference
