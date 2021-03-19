menarche <- read.table("menarche.txt", header=TRUE)
#menarche

str(menarche)
summary(menarche)  # note that some levels of Age are combined

#library(lattice)
menarche$p <- Menarche/Total
attach(menarche)

par(mfrow=c(1,3),mar=c(4,4,4,1))

plot(Age,p, main = "p vs Age")

logit <- function(p) log(p/(1-p))
plot(Age,logit(p),main = "logit(p) vs Age")

probit <- function(p) qnorm(p)
plot(Age,probit(p),main = "probit(p) vs Age")

# logit has higher variance, probit within +-3

y.Bin <- cbind(Menarche, Total - Menarche)
fm_logit <- glm(y.Bin ~ Age , data = menarche, family = binomial)  # logit link
fm_logit2 <- glm(y.Bin ~ Age + I(Age^2) , data = menarche, family = binomial)  
fm_logit3 <- glm(y.Bin ~ Age + I(Age^2) + I(Age^3) , data = menarche, family = binomial)  
anova(fm_logit,fm_logit2,fm_logit3, test = "Chisq")

fm_probit <- glm(y.Bin ~ Age , data = menarche, family = binomial(link="probit"))  # logit link
fm_probit2 <- glm(y.Bin ~ Age + I(Age^2), data = menarche, family = binomial(link="probit")) 
fm_probit3 <- glm(y.Bin ~ Age + I(Age^2) + I(Age^3), data = menarche, family = binomial(link="probit")) 
anova(fm_probit,fm_probit2,fm_probit3, test = "Chisq")

#L5-16
pchisq(fm_logit3$deviance,fm_logit3$df.residual,lower.tail = FALSE)
pchisq(fm_probit2$deviance,fm_probit2$df.residual,lower.tail = FALSE)

AIC(fm_logit,fm_logit2,fm_logit3,fm_probit,fm_probit2,fm_probit3)


#par(mfrow=c(1,1), mar = c(4,4,2,2))
#plot(Age,logit(p),main = "logit(p) vs Age")
#points(Age,predict(fm_logit3),col="red")

#plot(Age,probit(p),main = "probit(p) vs Age")
#points(Age,predict(fm_probit2),col="red")



menarche$Age9 <- log(Age-9)
menarche$Age18 <- log(18-Age)

fm_c <- glm(y.Bin ~ Age9 + Age18 , data = menarche, family = binomial) 

#plot(Age,logit(p),main = "logit(p) vs Age")
#points(Age,predict(fm_c),col="red")

#plot(Age,p,main = "p vs Age")
#points(Age,predict(fm_logit3,type = "response"),col="red")
#plot(Age,p,main = "p vs Age")
#points(Age,predict(fm_probit2,type = "response"),col="red")
#plot(Age,p,main = "p vs Age")
#points(Age,predict(fm_c,type = "response"),col="red")

summary(fm_c)
AIC(fm_logit,fm_logit2,fm_logit3,fm_probit,fm_probit2,fm_probit3,fm_c)

fm_cloglog <- glm(y.Bin ~ Age + I(Age^2) + I(Age^3), data = menarche, family = binomial(link="cloglog")) 
summary(fm_cloglog)

beta <- coef(fm_cloglog)
beta0 <- beta[1]  
beta1 <- beta[2] 
beta2 <- beta[3]  
beta3 <- beta[4]

clogclog <- function(p) log(-log(1-p))

(I <- range(Age))
f <- function(x, p) {
  predict(fm_cloglog, data.frame(Age=x))-clogclog(p)}

a <- rep(0, 9)
p <- seq(9)/10

for (ii in 1:9) {
  output <- uniroot(f, interval=I, p=p[ii])
  a[ii] <- output$root
}

'''
par(mar = c(4,4,1,1))
plot(p ~ Age, menarche, ylim = c(0,1))
lines(Age, predict(fm_cloglog, menarche, type = "response"), col ="red")
abline(h = 0.5, col = "blue")
abline(v = a[5], col = "blue")
abline(h = 0.9, col = "green")
abline(v = a[9], col = "green")
'''

# CI 
sd_a <- rep(0,9)
ul <- rep(0, 9)
ll <- rep(0, 9)

for (ii in 1:9) {
  x <- a[ii]
  dm <- beta1 + 2*beta2*x + 3*beta3*x^2 # denominator
  h <- c(-1/dm, -x/dm, -x^2/dm, -x^3/dm)
  sd_x <- sqrt(h %*% vcov(fm_cloglog) %*% h)
  sd_a[ii] <- as.numeric(sd_x)
  ll[ii] <- x + -1 * qnorm(0.975) * sd_a[ii]
  ul[ii] <- x + 1 * qnorm(0.975) * sd_a[ii]
}

round(rbind(p,ul,a,ll,sd_a),3)

par(mar = c(4,4,1,1))
plot(p ~ Age, menarche, ylim = c(0,1), main="p versus Age")
lines(Age, predict(fm_cloglog, menarche, type = "response"))
lines(ul,p,lty = 2,col ="red") 
lines(ll,p,lty = 2,col ="red")
for (ii in 1:9) {
abline(v = a[ii], col = "blue")
abline(h = p[ii], col = "green")
text(a[ii]-0.1, 0.02, round(a[ii],2), col = "blue",srt=90, cex=0.7) 
}

