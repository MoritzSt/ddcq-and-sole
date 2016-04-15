totalArea <- 5 # [km^2]
M <- 0.2 # natural mortality
q <- 0.5 # catchability

# N = pop. size; fracOccSwept = fraction of occ area swept; fracOcc = fraction of
# total area occupied
df <- expand.grid(N=c(100,200,500), fracOccSwept=c(0.1,0.2,0.5), fracOcc=c(0.25,1))

# aggregation / density of pop
df$areaOcc <- df$fracOcc * totalArea # total area occupied [km^2]
df$NPUA <- df$N/df$areaOcc # density; N per unit area [N/km^2]

# fishing 
df$areaSwept <- df$areaOcc * df$fracOccSwept  # assumes fishing occurs in occupied
                                              # area only 
df$f <- df$areaSwept # effort equivalent to total area swept
df$F <- df$f * q
df$C <- df$N * (1 - exp(-(M+(df$F))))*(df$F/(M+df$F))
df$CPUE <- df$C / df$f
df$FPUE <- df$F / df$f
df$Z <- df$F + M # total mortality
df$E <- df$F / df$Z # Exploitation rate


# result
df


# explorations
plot(CPUE~NPUA, df)
plot(F~f, df)
plot(FPUE~f, df)

fit <- glm(CPUE~log(f), df, family = Gamma(link="log"))
newdat <- data.frame(f=seq(min(df$f), max(df$f), length.out = 100))
newdat$CPUE <- predict(fit, newdat, type="response")
plot(CPUE~f, df, log=""); lines(CPUE~f, newdat)
plot(CPUE~f, df, log="xy"); lines(CPUE~f, newdat)



png("visualize.png", width = 7, height=7, units="in", res=200, type="cairo")
op <- par(mfrow=c(6,3))
par(oma=c(3,3,0,0), mar=c(0.5,0.5,2,0.5), mgp=c(2,0.5,0), tcl=-0.25)
for(i in seq(nrow(df))){
  plot(1, t="n", xlim=c(0,sqrt(totalArea)), ylim=c(0,sqrt(totalArea)), axes=FALSE,
       xaxs="i", yaxs="i", xlab="", ylab="")
  box()
  COL <- c(NA,rgb(1,0,0,0.5))[rep(1:2, times=c(round((df$N-df$C)[i]), round(df$C[i])))]
  points(x=runif(df$N[i])*sqrt(df$areaOcc[i]), y=runif(df$N[i])*sqrt(df$areaOcc[i]),
         pch=21, cex=0.5, lwd=0.5, bg=COL, col=rgb(0,0,0,0.5))
  mtext(paste0("N=", df$N[i], "; C=", round(df$C[i]), "; f=", round(df$f[i],2), ";
               F=", df$F[i], "; CPUE=", round(df$CPUE[i])), line=0.25, side=3, cex=par()$cex)
}
mtext("Population size(N)--> ; CPUE-->", line=1, side=1, outer = TRUE)
mtext("<--Effort(f) ; Density(NPUA)--> ; CPUE-->", line=1, side=2, outer = TRUE)
par(op)
dev.off()