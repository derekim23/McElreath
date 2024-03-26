#Simulate weight from height
sim_weight <- function(H, b, sd){
  U <- rnorm(length(H), 0, sd)
  W <- b * H + U
  W
}

H <- runif(200, min = 130, max = 170)
W <- sim_weight(H, b = 0.5, sd = 5)

plot(W~H, col = 2, lwd=3)

data(Howell1)

d <- Howell1[Howell1$age>=18,]



library(rethinking)

H <- runif(10, 130, 170)
W <- sim_weight(H, b = 0.5, sd = 5)

m3.1 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + b * H,
    a ~ dnorm(0, 10),
    b ~ dunif(0, 1),
    sigma ~ dunif(0, 10)),
    data = list(W = W, H = H)
)

precis(m3.1)

dat <- list(W = d$weight, H = d$height)

m3.2 <-  quap(
    alist(
      W ~ dnorm(mu, sigma),
      mu ~ b * H + a,
      a ~ dnorm(0, 10),
      b ~ dunif(0, 1),
      sigma ~ dunif(0, 10)
    ), data = list(W = W, H = H)
)


precis(m3.2)

dnorm(0,10)
?dunif
runif(0, 1)

post <- extract.samples(m3.2)

plot(d$height, d$weight, col = 2, lwd = 3, xlab = 'Height (cm)', ylab = 'Weight (kg)')

for (j in 1:20){
  abline(a = post$a[j], b = post$b[j], lwd = 1)
}

library(rethinking)

#The normal approximation for smaller growth works
#but the underlying distribution is actually lognormal
growth <- replicate( 10000 , prod( 1 + runif(12,0,0.1) ) )
dens( growth , norm.comp=TRUE )

big <- replicate( 10000 , prod( 1 + runif(12,0,0.5) ) )
small <- replicate( 10000 , prod( 1 + runif(12,0,0.01) ) )

dens(log(big), norm.comp = T)
dens(log(small), norm.comp = T)


#Chapter 4.3.1
library(rethinking)
data(Howell1)
d <- Howell1
str(d)
summary(d)

precis(d)

#height prior
curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )

#prior predictive distribution

sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )

install.packages('easystats')


#check your model
library(easystats)

y <- rnorm(100,10,1)
x <- rnorm(100)

mod <- lm(y~x)

check_model(mod)  
  
d2 <- d[d$age >= 18,]

mu.list <- seq( from=150, to=160 , length.out=100 )
sigma.list <- seq( from=7 , to=9 , length.out=100 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum(
  dnorm( d2$height , post$mu[i] , post$sigma[i] , log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )

#contour plot of the posterior
contour_xyz( post$mu , post$sigma , post$prob )

image_xyz( post$mu , post$sigma , post$prob )

sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE ,
                        prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]

#Transparent plot of the bivariate posterior predictive
plot( sample.mu , sample.sigma , cex=0.5 , pch=16 , col=col.alpha(rangi2, 0.1) )
dens(sample.mu)
dens(sample.sigma)

#Repeat the above on a smalelr subset of heights and see what happens
d3 <- sample(d2$height, size = 20)
mu.list <- seq(from = 150, to = 170, length.out = 200)
sigma.list <- seq(from = 4, to = 20, length.out = 200)
post2 <-  expand.grid(mu = mu.list, sigma = sigma.list)
post2$LL <-  sapply(1:nrow(post2), function(i) 
  sum(dnorm(d3, mean=post2$mu[i], sd = post2$sigma[i], log = T)))

post2$prod = post2$LL + dnorm(post2$mu, 178, 20, T)  + 
  dunif(post2$sigma, 0, 50, T)

post2$prob = exp(post2$prod - max(post2$prod))

sample2.rows <- sample(1:nrow(post2), size = 1e4, replace = T, prob = post2$prob) 

sample2.mu <- post2$mu[ sample2.rows ]
sample2.sigma <- post2$sigma[ sample2.rows ]
plot( sample2.mu , sample2.sigma , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab='mu' , ylab='sigma' , pch=16 )

dens(sample2.sigma, norm.comp = T)

#Quad approximation starts here
flsit <- alist(
  height ~ dnorm(mu, sigma),
  mu <- dnorm(178, 20),
  sigma <- dunif(0, 50)
)

m4.1 <- quap(flist, data = d2)

precis(m4.1)

#quap estimates by setting an initial condition
start <- list(mu = mean(d2$height),
              sigma = sd(d2$height))

m4.1 <- quap(flist, data = d2, start = start)
precis(m4.1)

#Try a narrower prior
m4.2 <- quap(
  alist(height ~ dnorm(mu, sigma),
        mu ~ dnorm(178, 0.1), #narrower
        sigma ~ dunif(0, 50)), data = d2
)

precis(m4.2) #prior overrules the data and makes the mu much higher
#but now, sigma is much higher as well to compensate

post <- extract.samples( m4.1 , n=1e4 )
head(post)

precis(post)

library(MASS)

post <- mvrnorm(n=1e4, mu=coef(m4.1), Sigma=vcov(m4.1))

#4.4 Regression
plot(d2$weight ~ d2$height)

#Try prior predictive
set.seed(2971)
N <- 100 # 100 lines
a <- rnorm( N , 178 , 20 )
b <- rnorm( N , 0 , 10 )

plot( NULL , xlim=range(d2$weight) , ylim=c(-100,400),
    xlab="weight" , ylab="height")
abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )
mtext( 'b ~ dnorm(0,10)' )
xbar <- mean(d2$weight)
for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar),
                        from=min(d2$weight) , to=max(d2$weight) , add=TRUE ,
                        col=col.alpha('black',0.2) )

#the prior predictive looks wacky
#let's try to limit the prior on b
b <- rlnorm(1e4, 0, 1)
dens(b,xlim=c(0,5), adj = 0.1)


set.seed(2971)

plot(NULL, xlim = range(d2$weight), ylim = c(-100, 400),
     xlab = 'weight', ylab = 'height')
abline(h = 0, lty = 2)
abline(h = 272, lty = 1, lwd = 0.5)
mtext('b ~ dnorm(0,10)')
xbar <- mean(d2$weight)

for(i in 1:N) curve(a[i] + b[i]*(x-xbar), from = min(d2$weight), 
                    to = max(d2$weight), add = T, col = col.alpha('black', 0.2))

#Much better

#Now, try a quap posterior
m4.3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <-  a + b * (weight - xbar),
    a ~dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), data = d2
)

precis(m4.3)

pairs(m4.3)

#Now, try plotting the relationship
plot(height ~ weight, data = d2, col = rangi2)
post <- extract.samples(m4.3)
a_map <- mean(post$a)
b_map <- mean(post$b)

curve(a_map + b_map*(x-xbar), add = T)

#To map credible intervals:
post[1:5,]

N <- 100
dN <- d2[1:N,]
mN <- quap(alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b * (weight - mean(weight)),
  a ~ dnorm(178,20),
  b~ dlnorm(0, 1),
  sigma ~ dunif(0, 50)
), data = dN)

post <- extract.samples(mN, 20)

plot(dN$weight, dN$height, xlim = range(dN$weight), ylim = range(dN$height),
     col = rangi2, xlab = 'weight', ylab = 'height')
mtext(concat("N = ", N))

for (i in 1:100){
  curve(post$a[i] + post$b[i] * (x - mean(dN$weight)),
        col = col.alpha('black', 0.3), add = T)
}


#Try plotting uncertainty around a point estimte
post <- extract.samples( m4.3 )
mu_at_50 <- post$a + post$b * ( 50 - xbar )

points(rep(50,1e4),mu_at_50)

#So given a weight of 50kg, you get the following probability distribution of
#heights
dens(mu_at_50,col=rangi2, lwd = 2, xlab = "mu|weight=50", norm.comp = T)

#posterior interval for weight at 50
PI(mu_at_50, .89)

#But we want to repeat the above for all values of weight and not just 50
mu <- link(m4.3) #part of the stat rethinking pkg
str(mu)

dim(mu) #each row is a sample from the posterior
#there are 352 rows, because there are 352 individuals in the data
#But I don't think this really solves our problem?!

weight.seq <- seq(from=25, to=70, by = 1)
mu <- link(m4.3, data = data.frame(weight = weight.seq))
dim(mu)
#now, we have a thousand samples for each of the columns
#where each column corresponds to the weight we've set up above

plot(height ~ weight, d2, type = 'n')

for (i in 1:1000){
  points(weight.seq, mu[i,], pch=16, col = col.alpha(rangi2,0.1))
}

mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = .89)

mu.PI

plot(height ~ weight, data = d2, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)

plot(height)


#How the link function works
post <- extract.samples(m4.3)
mu.link <- function(weight) post$a + post$b*( weight - xbar )
weight.seq <- seq( from=25 , to=70 , by=1 )
mu <- sapply( weight.seq , mu.link )
mu.mean <- apply( mu , 2 , mean )
mu.CI <- apply( mu , 2 , PI , prob=0.89 )

post <- extract.samples(m4.3)
mu.link <- function(weight) post$a + post$b * (weight - xbar)
weight.seq <- seq(from = 25, to =70, by = 1)
mu <- sapply(weight.seq, mu.link)
mu.mean <- apply(mu, 2, mean)
mu.CI <-  apply(mu, 2, PI, prob = .89)

shade(mu.CI, weight.seq)


#We've only simulated randomness of mu and not sigma itself despite 
#having used extract.samples
#so, we try the following

sim.height <- sim(m4.3, data = list(weight = weight.seq), n=5e4)
str(sim.height)
height.PI <- apply(sim.height, 2, PI, prob = .89)

plot(height ~ weight, data = d2, col = col.alpha(rangi2,0.5))

lines(weight.seq, mu.mean)

mu.HPDI <-  apply(sim.height, 2, HPDI, prob = .89)

shade(mu.HPDI, weight.seq)

shade(height.PI, weight.seq)

#You can see that the intervals are much wider to account for the 
#uncertainty in sigma

#The rough edges of the CrI can be smoothened out by increasing the sample size

#This is how sim works
post <- extract.samples(m4.3)
weight.seq <- 25:70
sim.height <- sapply( weight.seq , function(weight)
  rnorm(
    n=nrow(post) ,
    mean=post$a + post$b*( weight - xbar ) ,
    sd=post$sigma ) )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

plot(height~weight, d2, col = col.alpha(rangi2,.5))
lines(weight.seq, mu.mean)

shade(height.PI, weight.seq)