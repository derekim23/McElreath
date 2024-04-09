#Lecture 05
#Simulate a fork

n <- 1000
Z <- rbern(n, 0.5)
X <- rbern(n, (1-Z)*0.1 + Z * .9)
Y <- rbern(n, (1-Z)*0.1 + Z * .9)

cor(X,Y)
table(X,Y)
#X and Y are correlated and are not d-separated

table(X[Z == 1], Y[Z == 1])
table(X[Z == 0], Y[Z == 0])
cor(X[Z == 1], Y[Z == 1])
cor(X[Z == 0], Y[Z == 0])
#As expected, conditionally d-separated

#A continuous example
cols <- c(4,2)
N <- 500
Z <- rbern(N, 0.5)
X <- rnorm(N,2*Z - 1)
Y <- rnorm(N, 2*Z - 1)
plot(X,Y, col = cols[Z+1], lwd = 3)

abline(lm(Y[Z==1]~X[Z==1]),col=2,lwd=3)
abline(lm(Y[Z==0]~X[Z==0]),col=4,lwd=3)

abline(lm(Y~Z),lwd=3)
#Above is simply Simpson's Paradox

library(rethinking)
data(WaffleDivorce)         

#Prior predictive sim
n <- 20
a <- rnorm(n,0,0.2)
bM <- rnorm(n,0,0.5)
bA <- rnorm(n,0,0.5)
plot(NULL, xlim = c(-2, 2), ylim = c(-2,2),
     xlab = 'Median Age of Marraige (Standardized)',
     ylab = 'Divorce Rate (Standardized)')

Aseq <- seq(from=-3, to=3, len=30)
for(i in 1:n){
  mu <- a[i] +bA[i] * Aseq
  lines(Aseq, mu, lwd=2, col=2)
}

dat <- list(
  D = standardize(WaffleDivorce$Divorce),
  M = standardize(WaffleDivorce$Marriage),
  A = standardize(WaffleDivorce$MedianAgeMarriage)
)

m_DMA <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM * M + bA * A,
    a ~ dnorm(0,0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = dat
)

plot(precis(m_DMA))

#The above model is problematic, because, we believe
#there is a fork going from A to M and D.
#So, we have to control for that.
post <- extract.samples(m_DMA)
n <- 1e3
As <- sample(dat$A, size = n, replace = T)

#Simulate D for M = 0, the sample mean
DM0 <- with(post, rnorm(n, a + bM * 0 + bA * As, sigma))

#Simuate D for M = 1.
DM1 <- with(post, rnorm(n, a + bM * 1 + bA * As, sigma))

#contrast

M10_contrast <- DM1 - DM0
dens(M10_contrast, lwd = 4, col = 2, xlab = 'Effect of 1 stdev Incrase in M')
#It seems there's hardly any effect

#But what about A's causal effect?
#We can't just intervene on A, because 
#that'd effectively "erase" the causal arrow pointing to 
#either M or D.

#Consider the pipe confounder instead.
#E.G. X -> Z -> Y

#Example
n <- 1e3
X <- rbern(n, 0.5)
Z <- rbern(n, (1-X) * 0.1 + X * 0.9)
Y <- rbern(n, (1 - Z) * 0.1 + Z * 0.9)
cor(X,Y)
cor(X[Z == 1], Y[Z == 1])
cor(X[Z == 0], Y[Z == 0])

cols <- c(4,2)
N <- 300
X <- rnorm(N)
Z <- rbern(N, inv_logit(X))
Y <- rnorm(N, (2*Z-1))
plot(X, Y, col = cols[Z + 1], lwd = 3)
abline(lm(Y[Z==1]~X[Z==1]), col = 2, lwd =3)
abline(lm(Y[Z==0]~X[Z==0]), col = 4, lwd =3)
abline(lm(Y~X), col = 1, lwd =3)


#Collider example
n <- 1e3
X <- rbern(n, 0.5)
Y <- rbern(n, 0.5)
Z <- rbern(n, ifelse(X + Y > 0, 0.9, 0.2))

table(X,Y)
cor(X,Y)

cor(X[Z==1], Y[Z==1])
cor(X[Z==0], Y[Z==0])

#The caausal factors become correlated when you control for
#the common outcome.

#This may mislead scientists to think that
#something that impacts X might actually impact Y
#which is clearly not true.
#We have to be careful controlling for the V structure here.

cols <- c(4,2)
N <- 500
Y <- rnorm(N)
X <- rnorm(N)
Z <- rbern(N, inv_logit(2*X+2*Y-2))
plot(X,Y, col = cols[Z+1], lwd = 3)

abline(lm(Y[Z==1]~X[Z==1]),col=2,lwd=3)
abline(lm(Y[Z==0]~X[Z==0]),col=4,lwd=3)

abline(lm(Y~Z),lwd=3)
#Because of how Z is defined, large values of X
#are associated with small values of Y and vice versa.


#The descendent example
#X->Z->Y
#   ↓
#   A  
n <- 1e3
X <- rbern(n, 0.5)
Z <- rbern(n, (1-X)*0.1 + X*0.9)
Y <- rbern(n, (1-Z)*0.1 + Z*0.9)
A <- rbern(n, (1-Z)*0.1 + Z*0.9)

table(X,Y)
cor(X,Y)

cor(X[A==1], Y[A==1])
cor(X[A==0], Y[A==0])
#Not as good of an intervention variable as would be Z

 