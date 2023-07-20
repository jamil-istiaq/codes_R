testd <- matrix( byrow=TRUE, nrow=4,data=c(3,3,3,4,1,3,1,6))
(covmat<-cov(testd))
(cormat<-cor(testd))
str(testd)

########### covariance matrix ####################
# Centre the data and calculate covariance matrix
(means <- colMeans(testd))
(means2 <- cbind(c(1,1,1,1)) %*% means) # Turn into a matrix of means
(ctestd <- testd - means2)  #subtract the mean from each value in data matrix

#calculate the covariance matrix from centred values
XtX <- t(ctestd) %*% ctestd
(C<- XtX/(length( testd[,1] )-1) ) #divide by n-1
#OR simply
(C1<-cov(ctestd))
#the cov matrix of the centred data is = covariance matrix of the original data because
#centring is part of the calculation of C matrix, Manly page 23.

# calculate eigen values and scores
(covmat<-cov(testd))
(es<- eigen(covmat))
(scores <- ctestd %*% es$vectors)

##compare to function
(testd.prcomp <- prcomp(testd[,1:2], center=TRUE, scale=FALSE ))
(evalues_sqrt<-sqrt(es$values))


########### correlation matrix ####################
# Centre and scale and calculate correlation matrix
(st.devs <- sqrt(diag(C))) #sqrt the variances
cstestd<-testd #need to create cstestd and fill it with something temporarily
cstestd[,1] <- ctestd[,1] / st.devs[1] #divide the centred values by their std dev.
cstestd[,2] <- ctestd[,2] / st.devs[2]
cstestd
(cor1 <- cor( cstestd ))
cormat<-cor(testd)
#the cor matrix of the centred and standardised data is = cor matrix of the original 
#data because centring and standardising the variance is part of  calculation of 
#the oorrelation matrix. Variances are=1 i.e unit variances


# calculate eigen values and scores
(es1 <- eigen(cormat))
(scores <- cstestd %*% es1$vectors)




