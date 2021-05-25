nij <- cbind(c(195,979),c(46,370))

(muhat <- outer(rowSums(nij), colSums(nij), "*")/sum(nij))

(X2 <- sum((nij - muhat)^2/muhat))

pchisq(X2, 1, lower.tail=FALSE)

(odd <- nij[1,1]*nij[2,2]/(nij[1,2]*nij[2,1]))



par(mfrow=c(1,2),mar=c(4,4,4,1))

dhyper(104,114,24,126) #probability of observed table for white
plist <- dhyper(102:114,114,24,126)
i <- dhyper(102:114,114,24,126) <= dhyper(104,114,24,126)
sum(plist[i]) #p-value

plot(102:114, plist, main="white", xlab = "x", ylab = "P(N_11=x)")
abline(v=104, col="red")
text(104-0.2, 0.02, "x=104  ", col = "red",srt=90, cex=0.7)


dhyper(91,127,1325,1048) #probability of observed table for black
plist <- dhyper(0:127,127,1325,1048)
i <- dhyper(0:127,127,1325,1048) <= dhyper(91,127,1325,1048)
sum(plist[i]) #p-value

plot(0:127, plist, main="black", xlab = "y", ylab = "P(N_11=y)")
abline(v=91, col="red")
text(91-2, 0.006, "y=91  ", col = "red",srt=90, cex=0.7)
