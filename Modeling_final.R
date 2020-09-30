library(nlme)

year.offset = min(welfare$year)

## Clean MODEL

plinMat = welfare

plinMat$b0 = welfare$handicap
plinMat$a1 = welfare$year - year.offset
plinMat$b1 = ifelse(welfare$handicap==1, welfare$year - year.offset - 7, 0)
plinMat$b2 = ifelse((welfare$year - year.offset -7 >0)  & (welfare$handicap==1), welfare$year - year.offset -7, 0)


model.add.gls = gls(lwage ~ b0 + a1 + b1 + b2 + sex + factor(health.new2) + eduyear + 
                      (status.new + scale.new + job.new)*(exp+I(exp^2)), data=plinMat)

# Correlation Matrix
resmat = data.frame(year = welfare$year, 
                    id = welfare$ID,
                    resid = model.add.gls.AR$residuals)
resmat.wide= dcast(resmat, id ~ year, value.var="resid")
resmat.reform = resmat.wide[,2:ncol(resmat.wide)]
res.cor = cor(resmat.reform, method="pearson", use="pairwise.complete.obs")
print(res.cor, digits=3)

model.add.gls.cs =  gls(lwage ~ b0 + a1 + b1 + b2 + sex + factor(health.new2) + eduyear + 
                          (status.new + scale.new + job.new)*(exp+I(exp^2)), data=plinMat,
                        cor=corCompSymm(form = ~ 1 | ID))
print(corMatrix(model.add.gls.cs$modelStruct$corStruct)$'77001', digits=3)

model.add.gls.cs2 =  gls(lwage ~ b0 + a1 + b1 + sex + factor(health.new2) + eduyear + 
                          (status.new + scale.new + job.new)*(exp+I(exp^2)), data=plinMat,
                        cor=corCompSymm(form = ~ 1 | ID))
setEPS()
postscript("FitvsActual_glsCS.eps")
plot(model.add.gls.cs, lwage~fitted(.) | handicap, abline = c(0,1))
dev.off()

model.add.gls.AR = gls(lwage ~ b0 + a1 + b1 + b2 + sex + factor(health.new2) + eduyear + 
                      (status.new + scale.new + job.new)*(exp+I(exp^2)), data=plinMat,
                    cor=corAR1(form = ~ 1 | ID))

print(corMatrix(model.add.gls.AR$modelStruct$corStruct)$'77001', digits=3)

model.add.gls.AR2 = gls(lwage ~ b0 + a1 + b1 + sex + factor(health.new2) + eduyear + 
                         (status.new + scale.new + job.new)*(exp+I(exp^2)), data=plinMat,
                       cor=corAR1(form = ~ 1 | ID))


model.add.gls.gauss = gls(lwage ~ b0 + a1 + b1 + b2 + sex + factor(health.new2) + eduyear + 
                         (status.new + scale.new + job.new)*(exp+I(exp^2)), data=plinMat,
                       cor=corGaus(form = ~ 1 | ID))
print(corMatrix(model.add.gls.gauss$modelStruct$corStruct)$'77001', digits=3)

model.add.gls.gauss2 = gls(lwage ~ b0 + a1 + b1 + sex + factor(health.new2) + eduyear + 
                            (status.new + scale.new + job.new)*(exp+I(exp^2)), data=plinMat,
                          cor=corGaus(form = ~ 1 | ID))



####### Random Effect
model.add.RE = lme(lwage ~ b0 + a1 + b1 + b2 + sex + factor(health.new2) + eduyear + 
                     (status.new + scale.new + job.new)*(exp+I(exp^2)), data=plinMat,
                   random = ~1 | ID)

model.add.RE2 = lme(lwage ~ b0 + a1 + b1  + sex + factor(health.new2) + eduyear + 
                     (status.new + scale.new + job.new)*(exp+I(exp^2)), data=plinMat,
                   random = ~1 | ID)
setEPS()
postscript("FitvsActual_lmeRI.eps")
plot(model.add.RE, lwage~fitted(.) | handicap, abline = c(0,1))
dev.off()

model.add.RE.AR =update(model.add.RE, correlation=corARMA(p=1,q=0))
setEPS()
postscript("FitvsActual_lmeRIAR.eps")
plot(model.add.RE.AR, lwage~fitted(.) | handicap, abline = c(0,1))
dev.off()


model.add.RE.AR2 =update(model.add.RE2, correlation=corARMA(p=1,q=0))

# Prediction of the random intercept model
library(lattice)
lmepred=model.add.RE$fitted
colnames(lmepred)=c("pm","p")
welfare1=cbind(welfare,lmepred)

pdf("Fitted_samples_lmeAR1.pdf")
set.seed(2345)
sid = unique(welfare1$ID)[sample(6)]

xyplot(lwage+pm+p~year|ID, data=welfare1[welfare1$ID %in% unique(welfare1$ID)[sid], ], type=c("p","l","l"), col.line=c("white","black", "red"),
       panel=panel.superpose,distribute.type = TRUE, xlab="Time(years)", ylab="Log Wage", as.table=TRUE)
dev.off()

year.grid2 = seq(0,10, length.out = 200)

setEPS()
postscript("Handifit.eps")
plot(year.grid, predict(loess.hc, newdata=year.grid) - predict(loess.nhc, newdata=year.grid),
     type="l", lwd=2, col="black",
     xlab = "Year", ylab="Log wage", main= "Mean loess curve of difference Handicap")
lines(x= year.grid2+2005, y=(cbind(1, year.grid2) %*% model.add.RE.AR2$coefficients$fixed[c(2,4)]),lwd=2 ,col="red")
dev.off()

lines(x= year.grid+2005, y=(cbind(1, year.grid) %*% model.add.RE.AR2$coefficients$fixed[c(2,4)]), xlab="year", ylab="wage", col="red")

paramean


residset1 = cbind(welfare, model.add.RE.AR2$fitted)
colnames(residset1)[25] = "ind"
residset1$total = residset1$fixed + residset1$ind

paramean = aggregate(ind ~ year + handicap, data=residset1, mean)
ggplot(paramean, aes(x=year, y=ind, col=factor(handicap))) + geom_line(size=2) + 
  geom_line(data=welfare.loess, aes(x=year.grid, y=pred.nhc), col="blue", size=1, inherit.aes = FALSE) +
  geom_line(data=welfare.loess, aes(x=year.grid, y=pred.hc), col="red", size=1, inherit.aes = FALSE) +
  labs(x="Years", y="Log wage", col="Handicap") +
  scale_x_continuous(breaks=sort(unique(welfare$year)),labels=xtick) + scale_colour_manual(labels = c("False", "True"), values = c("steelblue", "red")) + theme_bw() +
  xlab("Years") + ylab("Log wage")

ggsave("meanplot.pdf")
