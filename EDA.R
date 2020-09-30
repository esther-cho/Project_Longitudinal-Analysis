#=================================#
#### Explanatory Data Analysis ####
#=================================#

library(ggplot2)
library(scales)

# Mean Plot : By handicap
## By mean
paramean = aggregate(lwage ~ year + handicap, data=welfare, mean)
ggplot(paramean, aes(x=year, y=lwage, col=factor(handicap))) + geom_line()

## Loess
loess.hc  = loess(lwage ~ year , data=welfare, subset=(handicap==1))
loess.nhc = loess(lwage ~ year , data=welfare, subset=(handicap==0))

year.grid = seq(min(welfare$year), max(welfare$year), length.out = 200)
plot(year.grid, predict(loess.hc, newdata=year.grid), ylim=c(4.6, 5.4),
     type="l", lwd=2, col="red",
     xlab = "Year", ylab="Log wage", main= "Mean loess curve by Handicap")
lines(x=year.grid, y=predict(loess.nhc, newdata=year.grid), lwd=2, col="blue")

yhat = predict(loess.hc, newdata=year.grid) - predict(loess.nhc, newdata=year.grid)
qfit = lm(yhat ~ year.grid + I(year.grid^2))

plot(year.grid, predict(loess.hc, newdata=year.grid) - predict(loess.nhc, newdata=year.grid),
     type="l", lwd=2, col="red",
     xlab = "Year", ylab="Log wage", main= "Mean loess curve of difference Handicap")

lines(x=year.grid, y=predict(qfit), col="blue")

# Individual plot
welfare.loess = data.frame(year.grid = year.grid,
                           pred.nhc =  predict(loess.nhc, newdata=year.grid),
                           pred.hc =  predict(loess.hc, newdata=year.grid))
#xtick = sort(unique(welfare$year))
xtick = c("05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15")


ggplot(welfare, aes(x=year, y=lwage, group=ID, col=as.factor(handicap))) +
  geom_line(alpha=0.2) +
  geom_line(data=welfare.loess, aes(x=year.grid, y=pred.nhc), col="blue", size=2, inherit.aes = FALSE) +
  geom_line(data=welfare.loess, aes(x=year.grid, y=pred.hc), col="red", size=2, inherit.aes = FALSE) +
  labs(x="Years", y="Log wage", col="Handicap") +
  scale_x_continuous(breaks=sort(unique(welfare$year)),labels=xtick) + scale_colour_manual(labels = c("False", "True"), values = c("steelblue", "red")) + theme_bw() +
  xlab("Years") + ylab("Log wage")

ggsave(file="IndividualPlot.pdf")

# By sex


# By handicap and sex
paramean = aggregate(lwage ~ year + handicap * sex, data=welfare, mean)
paramean$hs = 10*as.integer(paramean$handicap) + as.integer(paramean$sex)
ggplot(paramean, aes(x=year, y=lwage, col=as.factor(hs))) + geom_line() +
  scale_x_continuous(breaks=sort(unique(welfare$year)),labels=xtick) + theme_bw()

plot(paramean[paramean$handicap==0 & paramean$sex==1,"lwage"] - paramean[paramean$handicap==0 & paramean$sex==2,"lwage"],
     ylim=c(0.50, 1),col="black")
points(paramean[paramean$handicap==1 & paramean$sex==1,"lwage"] - paramean[paramean$handicap==1 & paramean$sex==2,"lwage"], col="red")

plot(paramean[paramean$handicap==1 & paramean$sex==1,"lwage"] - paramean[paramean$handicap==0 & paramean$sex==1,"lwage"],
     ylim=c(-0.7,-0.2),col="black")
points(paramean[paramean$handicap==1 & paramean$sex==2,"lwage"] - paramean[paramean$handicap==0 & paramean$sex==2,"lwage"], col="red")


# By Edu
paramean = aggregate(lwage ~ year + eduyear.new, data=welfare, mean)
ggplot(paramean, aes(x=year, y=lwage, col=factor(eduyear.new))) + geom_line() +
  scale_x_continuous(breaks=sort(unique(welfare$year)),labels=xtick) + theme_bw() +
  labs(main="Log wage by education year")

# By Exp : Why Exp has to be quadratic
loess.exp  = loess(lwage ~ exp , data=welfare)
exp.grid = seq(min(welfare$exp), max(welfare$exp), length.out = 200)
plot(exp.grid, predict(loess.exp, newdata=exp.grid),
     type="l", lwd=2, col="red",
     xlab = "Exp", ylab="Log wage", main= "Mean loess curve by Exp")

setEPS()
postscript("Exp_Wage.eps")
plot(lwage ~ exp, data=welfare, ylab="Log Wage", xlab="Exp")
lines(x=exp.grid, y=predict(loess.exp, newdata=exp.grid), col='red', lwd=2)
dev.off()
# By job.new
paramean = aggregate(lwage ~ year + job.new, data=welfare, mean)
ggplot(paramean, aes(x=year, y=lwage, col=factor(job.new))) + geom_line() +
  scale_x_continuous(breaks=sort(unique(welfare$year)),labels=xtick) + theme_bw() +
  labs(main="Log wage by education year")

# By status.new
paramean = aggregate(lwage ~ year + status.new, data=welfare, mean)
ggplot(paramean, aes(x=year, y=lwage, col=factor(status.new))) + geom_line() +
  scale_x_continuous(breaks=sort(unique(welfare$year)),labels=xtick) + theme_bw() +
  labs(main="Log wage by education year")

# Correlation Matrix
resmat = data.frame(year = welfare$year, 
                    id = welfare$ID,
                    resid = model.sat.lm$residuals)
resmat.wide= dcast(resmat, id ~ year, value.var="resid")
resmat.reform = resmat.wide[,2:ncol(resmat.wide)]
res.cor = cor(resmat.reform, method="pearson", use="pairwise.complete.obs")
print(res.cor, digits=3)

colnames(resmat.reform) = paste0("Y",2005:2015)

library(GGally)
pdf("CorrMatrix.pdf", height = 7, width = 7)
gg = ggpairs(data=resmat.reform, method="pairwise.complete.obs")
print(gg, left=0.5, bottom=0.5)
dev.off()
