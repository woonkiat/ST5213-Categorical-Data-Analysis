F <- c("BothParents", "MotherOnly")
R <- c("Black","White")
G <- c("Male","Female")
A <- c("1","2")
S <- c("None","Some")

dat<-expand.grid(S=S,A=A,G=G,R=R,F=F)
dat$Count <- c(27,2,12,2,23,4,7,1,394,32,142,19,421,38,94,11,18,1,13,1,24,0,4,3,48,6,25,4,55,15,13,4)
ftable(xtabs(Count~F+R+G+A+S,dat))

fm <- glm(Count ~ F*R*G*A*S, dat, family = poisson)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:A:G:R:F)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - A:G:R:F)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:G:R:F)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - G:R:F)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:A:G:R)
drop1(fm, test="Chisq") #note AGR cannot be dropped

fm<- update(fm, .~. - S:G:R)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:A:G:F)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:A:G)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - A:G:F)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:G:F)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - G:F)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:G)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:A:R:F)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:A:F)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - A:R:F)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:A:R)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - A:F)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:R:F)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:R)
drop1(fm, test="Chisq")

#Count ~ S + A + G + R + F + S:A + A:G + A:R + G:R + S:F + R:F + A:G:R

pchisq(fm$deviance, fm$df.residual, lower.tail=FALSE)

fmtab <- xtabs(fitted(fm) ~F+R+G+A+S, dat)
oddsratio_fn<- function(tab) {tab[1,1]*tab[2,2] / (tab[1,2]*tab[2,1])}

apply(fmtab, c("G","A","R"), oddsratio_fn) #conditional odds SF

cfmtab2 <- apply(fmtab, c("F","S"),sum) #collapse over AGR # == apply(xtabs(Count~F+R+G+A+S,dat), c("F","S"),sum)
oddsratio_fn(cfmtab2) #marginal odds SF #not the same

cfmtab3 <- apply(fmtab, c("F","S","A","R"),sum) #collapse over G
apply(cfmtab3, c("A","R"), oddsratio_fn) #SF


#odd SA collapse over GR and GRF not the same
cfmtab4 <- apply(fmtab, c("A","S","F"),sum) #collapse over GR
apply(cfmtab4, "F", oddsratio_fn) 
cfmtab5 <- apply(fmtab, c("A","S"),sum) #collapse over GRF
oddsratio_fn(cfmtab5)

apply(cfmtab3, c("F","R"), oddsratio_fn) 









fm <- glm(Count ~ F*R*G*A*S, dat, family = poisson)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:A:G:R:F)
drop1(fm, test="Chisq")

#fm<- update(fm, .~. - A:G:R:F)
#drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:G:R:F)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:A:G:R)
drop1(fm, test="Chisq") 

fm<- update(fm, .~. - S:G:R)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:A:G:F)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:A:G)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:G:F)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:G)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:A:R:F)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:A:F)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:A:R)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:R:F)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:R)
drop1(fm, test="Chisq")

fm<- update(fm, .~. - S:A)
drop1(fm, test="Chisq")

pchisq(fm$deviance, fm$df.residual, lower.tail=FALSE)


fmtab <- xtabs(fitted(fm) ~F+R+G+A+S, dat)
oddsratio_fn<- function(tab) {tab[1,1]*tab[2,2] / (tab[1,2]*tab[2,1])}

apply(fmtab, c("G","A","R"), oddsratio_fn) #conditional odds SF

apply(fmtab, c("F","S"), sum) #collapse over GAR
oddsratio_fn(apply(fmtab, c("F","S"), sum)) #collapse over GAR



(dat.logit <- cbind(expand.grid(A=A,G=G,R=R,F=F),
                    SN = dat$Count[dat$S=="Some"], SY = dat$Count[dat$S=="None"]))

fm.logit <- glm(cbind(SN, SY) ~ F, dat.logit, family =binomial)
summary(fm.logit)
