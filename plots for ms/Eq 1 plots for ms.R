# a script to demonstrate QR0 functioning for the ddcq manuscript

model <- function(year, effort, ssb0, creep, qr0, ssb, wantedness, f_other, f_focus) {
F_modelled = (1 + creep * year) * effort *
               qr0 / (1 + (qr0 - 1) * ssb / ssb0) +
               wantedness * f_focus / (f_other + f_focus)
return(F_modelled)
}


model_2 <- function(year, effort, ssb0, creep, qr0, ssb, wantedness, f_other, f_focus, f_focus_0, f_other_0) {
  F_modelled = (1 + creep * year) * effort *
    qr0 / (1 + (qr0 - 1) * ssb / ssb0) +
    wantedness * ((f_focus / (f_other + f_focus)) - (f_focus_0 / (f_other_0 + f_focus_0)))
  return(F_modelled)
}

# generate data ----

n <- 40
year <- seq(from = 1, to = n, by = 1)
effort <- seq(from = 1, to = 1, length.out = n)
ssb <- seq(from = 0.5, to =3, length.out = n)
set.seed(1234)
f_focus <- 0.2 + sin((year - year[1])/15) / 2
f_focus <- rnorm(n = length(f_focus), mean = f_focus, sd = 0.1)
f_other <- 0.3 + year * (0.4 / 30) + 
          + 2E-4 * (year / 40) ^ 2  - 5E-2 * (year / 20) ^ 3  # break term to collapse F in last years
f_other <- rnorm(n = length(f_other), mean = f_other, sd = 0.1)

f_focus <- rev(f_focus)

dat <- as.data.frame(cbind(year, effort, ssb, f_focus, f_other))
dat_backup <- dat

creep <- 0
ssb0 <- 1
wantedness <- 0
qr0 <- 1
f_focus_0 <- dat$f_focus[1]
f_other_0 <- dat$f_other[1]


# Effect of QR0 ----
# Model F

dat$F_at_qr0_1 <- model(year = dat$year,
                             effort = dat$effort,
                             ssb0 = ssb0,
                             creep = creep,
                             qr0 = qr0,
                             ssb = dat$ssb,
                             wantedness = wantedness,
                             f_other = dat$f_other,
                             f_focus = dat$f_focus)

dat$F_at_qr0_1.5 <- model(year = dat$year,
                        effort = dat$effort,
                        ssb0 = ssb0,
                        creep = creep,
                        qr0 = 1.5,
                        ssb = dat$ssb,
                        wantedness = wantedness,
                        f_other = dat$f_other,
                        f_focus = dat$f_focus)

dat$F_at_qr0_2 <- model(year = dat$year,
                          effort = dat$effort,
                          ssb0 = ssb0,
                          creep = creep,
                          qr0 = 2,
                          ssb = dat$ssb,
                          wantedness = wantedness,
                          f_other = dat$f_other,
                          f_focus = dat$f_focus)

# plot effect of QR0

ylimits <- range(dat$F_at_qr0_2)
x11()
plot(dat$F_at_qr0_2 ~ dat$ssb, ylim = ylimits, type = 'l', col = 2,
     xlab = 'SSB', ylab = 'relative F')
points(dat$F_at_qr0_1.5 ~ dat$ssb, ylim = ylimits, type = 'l', col = 3)
points(dat$F_at_qr0_1 ~ dat$ssb, ylim = ylimits, type = 'l', col = 1)

# effect of TC ----
# Model F

dat <- dat_backup
dat$F_at_TC_0 <- model(year = dat$year,
                        effort = dat$effort,
                        ssb0 = ssb0,
                        creep = creep,
                        qr0 = qr0,
                        ssb = dat$ssb,
                        wantedness = wantedness,
                        f_other = dat$f_other,
                        f_focus = dat$f_focus)

dat$F_at_TC_.03 <- model(year = dat$year,
                          effort = dat$effort,
                          ssb0 = ssb0,
                          creep = 0.03,
                          qr0 = qr0,
                          ssb = dat$ssb,
                          wantedness = wantedness,
                          f_other = dat$f_other,
                          f_focus = dat$f_focus)

# plot effect of TC

ylimits <- c(min(dat[,grep('F_at', names(dat))]),
             max(dat[,grep('F_at', names(dat))]))
x11()
plot(dat$F_at_TC_.03 ~ dat$year, ylim = ylimits, type = 'l', col = 2,
     xlab = 'time', ylab = 'relative F')
points(dat$F_at_TC_0 ~ dat$year, ylim = ylimits, type = 'l', col = 1)

# effect of market incentives
# Model F

dat <- dat_backup

      ## Try something
      multiplier <- function(f_other, f_focus, f_focus_0 = f_focus_0, f_other_0 = f_other_0) {
        multiplier <- (f_focus / (f_other + f_focus)) - (f_focus_0 / (f_other_0 + f_focus_0))}
      
      dat$multiplier <- multiplier(f_other = dat$f_other, f_focus = dat$f_focus,
                                   f_focus_0 = f_focus_0, f_other_0 = f_other_0)
      x11()
      par(mfrow = c(2,2))
      plot(dat$f_focus ~ dat$year, ylim = c(0,1), col = 2, ylab = 'F')
      points(dat$f_other ~ dat$year, ylim = c(0,1), col = 4)
      plot(y = (dat$f_focus / (dat$f_focus + dat$f_other))  - (f_focus_0 / (f_other_0 + f_focus_0)),
           x = dat$year, type = 'b')
      plot(y = (dat$f_focus / (dat$f_focus + dat$f_other)),
           x = dat$year, type = 'b', col = '6')
      ##
      
      
dat$F_at_SP_0 <- model_2(year = dat$year,
                       effort = dat$effort,
                       ssb0 = ssb0,
                       creep = creep,
                       qr0 = qr0,
                       ssb = dat$ssb,
                       wantedness = wantedness,
                       f_other = dat$f_other,
                       f_focus = dat$f_focus,
                       f_focus_0 = f_focus_0,
                       f_other_0 = f_other_0)

dat$F_at_SP_.1 <- model_2(year = dat$year,
                         effort = dat$effort,
                         ssb0 = ssb0,
                         creep = creep,
                         qr0 = qr0,
                         ssb = dat$ssb,
                         wantedness = 0.1,
                         f_other = dat$f_other,
                         f_focus = dat$f_focus,
                         f_focus_0 = f_focus_0,
                         f_other_0 = f_other_0)

dat$F_at_SP_1 <- model_2(year = dat$year,
                        effort = dat$effort,
                        ssb0 = ssb0,
                        creep = creep,
                        qr0 = qr0,
                        ssb = dat$ssb,
                        wantedness = 1,
                        f_other = dat$f_other,
                        f_focus = dat$f_focus,
                        f_focus_0 = f_focus_0,
                        f_other_0 = f_other_0)

# plot effect of SP or wantedness or market incentives

ylimits <- c(min(dat[,grep('F_at', names(dat))]),
             max(dat[,grep('F_at', names(dat))]))
x11()
par(mfrow = c(1,2))
plot(y = dat$F_at_SP_1, x = dat$f_focus / (dat$f_focus + dat$f_other), ylim = ylimits, type = 'l', col = 2,
     xlab = 'F_focus / (F_focus/F_other)', ylab = 'relative F')
points(y = dat$F_at_SP_0, x = dat$f_focus / (dat$f_focus + dat$f_other), ylim = ylimits, type = 'l', col = 1)
points(y = dat$F_at_SP_.1, x = dat$f_focus / (dat$f_focus + dat$f_other), ylim = ylimits, type = 'l', col = 3)

plot(y = dat$f_focus,
     x = dat$f_focus / (dat$f_focus + dat$f_other),
     ylim = c(0,1), col = 'red')
points(y = dat$f_other,
     x = dat$f_focus / (dat$f_focus + dat$f_other),
     ylim = c(0,1), col = 'blue')