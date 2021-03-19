sat <- read.csv("satisfaction.csv", header=TRUE)
#sat

#str(sat)
sat$Gender <- factor(sat$Gender)
sat$Race <- factor(sat$Race)
sat$Age <- factor(sat$Age)
sat$Region <- factor(sat$Region)

#summary(sat)

y <- cbind(sat$Satisfied, sat$Notsatisfied)

fm0 <- glm(y ~ 1, sat, family = binomial)
fmfull <- glm(y ~ Gender * Race * Age * Region, sat, family = binomial)

#as.numeric(round(fitted(fmfull),3))
#round(sat$Satisfied/(sat$Satisfied+ sat$Notsatisfied),3)


library(MASS)

############# from NULL model ###############
fm1 <- stepAIC(fm0, scope = list(lower=formula(fm0), upper = formula(fmfull)), trace=0)
# glm(formula = y ~ Age + Region + Gender + Age:Gender, family = binomial, data = sat) #AIC=478.6

summary(fm1)
add1(fm1, scope=fmfull, test="Chisq")
drop1(fm1, test="Chisq")

#library(lattice)
#xyplot(Satisfied/(Satisfied+Notsatisfied)~Gender:Age, groups=Age, sat)


############# from FULL model ###############
fm2 <- stepAIC(fmfull, scope = list(lower=formula(fm0), upper=formula(fmfull)), trace=0)
#glm(formula = y ~ Gender + Race + Age + Region + Gender:Race + Gender:Age + Race:Age + Gender:Race:Age, family = binomial, data = sat)
summary(fm2)
add1(fm2, scope=fmfull, test="Chisq")
drop1(fm2, test="Chisq")

(fm2 <- update(fm2, .~. -Gender:Race:Age))
add1(fm2, scope=fmfull, test="Chisq")
drop1(fm2, test="Chisq")

(fm2 <- update(fm2, .~. -Race:Age))
add1(fm2, scope=fmfull, test="Chisq")
drop1(fm2, test="Chisq")

(fm2 <- update(fm2, .~. -Gender:Age))
add1(fm2, scope=fmfull, test="Chisq")
drop1(fm2, test="Chisq")

#glm(formula = y ~ Gender + Race + Age + Region + Gender:Race, family = binomial, data = sat) #AIC: 476.95



############# from ALL-MAIN-TERM model ###############
fm3 <- glm(y ~  Gender + Race + Age + Region , sat, family = binomial)
fm3 <- stepAIC(fm3, scope=list(lower = formula(fm0), upper=formula(fmfull)), trace=0)
#glm(formula = y ~ Gender + Race + Age + Region + Gender:Race + Gender:Age, family = binomial, data = sat)

summary(fm3)
add1(fm3, scope=fmfull, test="Chisq")
drop1(fm3, test="Chisq")

(fm3 <- update(fm3, .~. -Gender:Age))
add1(fm3, scope=fmfull, test="Chisq")
drop1(fm3, test="Chisq")

#glm(formula = y ~ Gender + Race + Age + Region + Gender:Race, family = binomial, data = sat) #AIC = 477



'''
############# from 2ND ORDER model, SELECTION USING DEVIANCE###############

fm4 <- glm(y ~ (Gender + Race + Age + Region)^2, sat, family = binomial)

drop1(fm4, test = "Chisq")
fm4 <- update(fm4, .~. - Gender:Region)
drop1(fm4, test = "Chisq")
fm4 <- update(fm4, .~. - Age:Region)
drop1(fm4, test = "Chisq")
fm4 <- update(fm4, .~. - Race:Age)
drop1(fm4, test = "Chisq")
fm4 <- update(fm4, .~. - Race:Region)
drop1(fm4, test = "Chisq")
fm4 <- update(fm4, .~. - Gender:Age)

#glm(formula = y ~ Gender + Race + Age + Region + Gender:Race, family = binomial, data = sat) #AIC = 477
'''

#(b)

fmb <- glm(y ~ Region + Race + Gender*Age, sat, family = binomial)
summary(fmb)



#(c)

fm_c <- glm(y ~ Region + Gender*Race + Gender*Age, sat, family = binomial(link="probit"))
summary(fm_c)

sat[sat$Age=="35-44"&sat$Region=="P"&sat$Gender=="F"&sat$Race=="W",]
(X <- model.matrix(fm_c)[56,])

(y <- predict(fm_c,data.frame(Region="P",Age="35-44",Gender="F",Race="W"),type="response"))
(Xb <- predict(fm_c,data.frame(Region="P",Age="35-44",Gender="F",Race="W")))

#Xb <- 0; y <- pnorm(0)
#X <- c(1,0,0,0,1,0,0,0,1,0,1,0,0,0)
(V <- vcov(fm_c))

#direct method
pnorm(Xb+qnorm(0.975)*sqrt(t(X)%*%V%*%X))
pnorm(Xb-qnorm(0.975)*sqrt(t(X)%*%V%*%X))

'''
#delta method
y + qnorm(0.975) * sqrt(t(X*dnorm(Xb)) %*% V %*% (X*dnorm(Xb)))
y - qnorm(0.975) * sqrt(t(X*dnorm(Xb)) %*% V %*% (X*dnorm(Xb)))

#bootstrap method
b <- mnormt::rmnorm(1500000,coef(fm_c),V)
Xb <- b%*%X
y <- pnorm(Xb)
quantile(y,c(0.025,0.5,0.975))
'''

#(d)
pchisq(fm_c$deviance,fm_c$df.residual,lower.tail = FALSE)

m <- sat$Satisfied+sat$Notsatisfied
library(boot)
fmr <- glm.diag(fm_c)

par(mfrow=c(1,1),mar=c(4,4,4,1))
plot(m,fmr$rp,main = "Standardized Pearson Residual versus Sample Size m", cex.main=1.0)

(k <- fm_c$deviance/fm_c$df.residual)

pnorm(Xb+qnorm(0.975)*sqrt(k*t(X)%*%V%*%X))
pnorm(Xb-qnorm(0.975)*sqrt(k*t(X)%*%V%*%X))
