rm(list=ls())
par(mfrow=c(1, 1))
####################
#### Question 1 ####
####################
set.seed(5072)
x <- rnorm(100)
eps <- rnorm(100, 0, .25)
y <- -1 + 0.5*x + eps
length(y)
# B0 = -1, B1 = 0.5
plot(x, y, main = 'Moderate Error')
# Generally, there appears to be a strong, positively-linear relationship between X and Y. The variability seems quite manageable and not too large
lm.fit <- lm(y ~ x)
coef(lm.fit)
# B0hat = -1.001, B1hat = 0.468
# Both betas are very close to the population parameters , within 0.05
abline(lm.fit, lwd = 2, col = 'black')
abline(-1, .5, lwd = 2, col = 'red')
legend("bottomright", legend=c("Least Squares", "Population"), col=c(1,2),cex=0.75, lty=1, lwd=2)
#legend('topleft', legend = c("Predicted", "Population"), col=c(1, 2), cex=.75, pch=16)

#polynomial
x2 <- x**2
lm.fitp <- lm(y ~ x + x2)
anova(lm.fit, lm.fitp)
# adding the squared term barely affects the model and is insiginificantly differet, so it is more parsimonious to use the linear model

#variance of eps = 0.1
eps2 <- rnorm(100, 0, .1)
y2 <- -1 + 0.5*x + eps2
length(y2)
lm.fit2 <- lm(y2 ~ x)
coef(lm.fit2)
# B0hat = -0.9995, B1hat = 0.5069
# this model is closer to the population values than our first one, both within .005
plot(x, y2, main = 'Lesser Error')
abline(lm.fit2, lwd = 2, col = 'black')
abline(-1., .5, lwd = 2, col = 'red')
legend("bottomright", legend=c("Least Squares", "Population"), col=c(1,2),cex=0.75, lty=1, lwd=2)
#legend('topleft', legend = c("Predicted", "Population"), col=c(1, 2), cex=.75, pch=16)

#variance of eps = 0.5
eps3 <- rnorm(100, 0, .5)
y3 <- -1 + 0.5*x + eps3
length(y3)
lm.fit3 <- lm(y3 ~ x)
coef(lm.fit3)
# B0hat = -1.052, B1hat = 0.5588
# These predictors are the least accurate, as B0hat is > .05 away from the population model and B1hat is the lowest of all the models
# (and thus furthest from the population)
plot(x, y3, main = 'Greater Error')
abline(lm.fit3, lwd = 2, col = 'black')
abline(-1, .5, lwd = 2, col = 'red')
legend("bottomright", legend=c("Least Squares", "Population"), col=c(1,2),cex=0.75, lty=1, lwd=2)
#legend('topleft', legend = c("Predicted", "Population"), col=c(1, 2), cex=.75, pch=16)

# anova(lm.fit)
# anova(lm.fit2)
# anova(lm.fit3)

# The least variance error model fits the population model the closest, followed by the moderate error variance model, with the highest 
# variance error model fitting the least well.

confint(lm.fit, level=.95)
confint(lm.fit2, level=.95)
confint(lm.fit3, level=.95)

# The confidence interval widths differ because of the differing variances in the error terms.
# The model a lower error noise has a more acute CI, whereas the more noise model has a larger CI.
# The confidence intervals depend on the standard error, so the model with a larger variance in the
# error has a larger confidence interval because it is less accurate, etc.

####################
#### Question 2 ####
####################
set.seed(5072)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100) / 10
y = 2 + 2 * x1 + 0.3 * x2 + rnorm(100)
# B0 = 2, B1 = 2, B2 = 0.3
z <- cbind(y, x1, x2)

print(paste('Correlation of x1 and y =', cor(x1, y)))
print(paste('Correlation of x2 and y =', cor(x2, y)))

pairs(z)
# x1 and x2 are strongly positively correlated with one another. The correlations between y and the x's appear to have a weak positive 
# correlation. 

lm.fit.both <- lm(y ~ x1 + x2) 
coef(lm.fit.both)
summary(lm.fit.both)
# Both B0hat and B1hat are significant to the p = 0 level, while x2 is not statistically significant at all
# You can reject the null that B1 = 0, but not B2 = 0 due to the aforementioned significance levels

lm.fit.justx1 <- lm(y ~ x1)
summary(lm.fit.justx1)
# B0hat and B1hat remain significant to the same levels as before, so the null can still be rejected

lm.fit.justx2 <- lm(y~ x2)
summary(lm.fit.justx2)
# Surprisingly, in this model both B0hat and B2hat are significant at the highest level. In this isolated model,
# you can reject the null as B2hat significantly differs from 0.

#The latter results do seem to contradict with the former, as isolated B2hat seems to be significantly different
# from 0, but in the combined model it loses its significance. This is probably due to the correlation between 
# the x variables.

x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)

lm.fit.both2 <- lm(y ~ x1 + x2)
summary(lm.fit.both2)
# In this new model, B0hat remains significant at the same level, but the significance of B1hat decreases 
# to be significant at the p < 0.05, and B2hat is significant at the p < 0.1 level 
par(mfrow=c(2, 2))
plot(lm.fit.both2)
par(mfrow=c(1, 1))

# The new observation point 101 is an outlier and a high leverage point upon examination of the scale-location and residuals vs leverage plots
# Point 101 is spread much farther around the particular predictor range, and it is far beyond the Cook's Distance line. 


####################
#### Question 3 ####
####################
library(MASS)
set.seed(5072)
Boston.xs <- Boston[-1]
# lapply(names(Boston.xs), function(x) assign(x, Boston.xs[, x], envir = .GlobalEnv))
# xs <- c(list(age), list(black), list(chas), list(dis), list(indus), list(lstat), list(medv), list(nox), list(ptratio), list(rad), list(rm), list(tax), list(zn))
names <- colnames(Boston[-1])
f.stat <- rep(0, length(names))
p.val <- f.stat
b1.coef <- f.stat
intercept <- f.stat
models <- f.stat

for (i in 1:length(names)) {
  f.stat[i] <- summary(lm(crim~Boston[[names[i]]], data = Boston))$fstatistic[1]
  p.val[i] <- anova(lm(crim~Boston[[names[i]]], data = Boston))$'Pr(>F)'[1]
  b1.coef[i] <- coef(summary(lm(crim~Boston[[names[i]]], data = Boston)))["Boston[[names[i]]]","Estimate"]
  intercept[i] <- coef(summary(lm(crim~Boston[[names[i]]], data = Boston)))[1,4] 
}
results <- cbind(names,f.stat,p.val,b1.coef, intercept)
results
sig.names <- c()
print('The variables with significant associations are as follows: ')
for (i in 1:nrow(results)) {
  if (as.numeric(results[i,3]) < 0.05)
    # print(paste(results[i]))
    sig.names <- append(sig.names, results[i])
}
print(paste(sig.names))

par(mfrow = c(4, 3))
for (i in 1:12) {
  plot(Boston[[sig.names[i]]], Boston$crim, main = sig.names[i], xlab = 'x')
  abline(lm(crim~Boston[[sig.names[i]]], data = Boston), lwd = 2, col = 'red')
}
par(mfrow = c(1, 1))

lm.fitmulti <- lm(crim ~ zn + indus + nox + rm + age + dis + rad + tax + ptratio + black + lstat + medv, data = Boston)
coeff.mat <- summary(lm.fitmulti)$coefficients[-1,]
for (i in 1:nrow(coeff.mat)) {
  if (as.numeric(coeff.mat[i,4]) < 0.05)
    print(paste(colnames(Boston[-1])[i], coeff.mat[i,4]))
}

#length(b1.coef)
#length(coef(lm.fitmulti))
plot(b1.coef[-1], coef(lm.fitmulti)[-1], xlab = 'Simple', ylab = 'Multiple')
# The points are clustered generally around the same area (0 for both models, with one outlier). There does not appear to be
# any sort of relationoship between the coefficients of both models, as ignoring the one outier, the points area almost randommly
# dispersed. The fact that so many of the X variables are signicant seems to suggest that the multi-variable regression is
# the better model to use and more accurately reflects the population parameters.

poly.fs <- rep(0, length(sig.names))
poly.ps <- poly.fs
for (i in 1:12) {
  x.var <- Boston[[sig.names[i]]]
  x.var2 <- Boston[[sig.names[i]]]**2
  x.var3 <- Boston[[sig.names[i]]]**3
  poly.fs[i] <- anova(lm(crim ~ x.var, data = Boston) , lm(crim ~ x.var + x.var2 + x.var3, data = Boston))[2,5]
  poly.ps[i] <- anova(lm(crim ~ x.var, data = Boston) , lm(crim ~ x.var + x.var2 + x.var3, data = Boston))$'Pr(>F)'[2]
}

predictor <- sig.names
fstat <- poly.fs
pvalueofFstat <- poly.ps

a.table <- cbind(predictor, fstat, pvalueofFstat)
a.table.sorted <- a.table[order(as.numeric(a.table[ ,'fstat']), decreasing=T, na.last=NA), ]
a.table.sorted
# According to the table, the only predictor for which the null hypothesis cannot be rejected is black, meaning
# that the rest of the linear and polynomial models significantly differ. In conclusion, this provides evidence for
# a potential non-linear relationship.