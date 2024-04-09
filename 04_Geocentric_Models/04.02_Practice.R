#4M1
mu <- rnorm(2e4, 0, 10)
sigma <- rexp(2e4, 1)
y <- rnorm(2e4, mean = mu, sigma)
dens(y)

#4M2
alist(
  y ~ dnorm(mu, sigma),
  mu ~ dnorm(0, 10),
  sigma ~ dexp(1)
)

#4M8
library(rethinking)
data(cherry_blossoms)
d <- cherry_blossoms
precis(d)
?cherry_blossoms

plot(d$doy, d$year)

d2 <- d[complete.cases(d$doy), ] # complete cases on doy
num_knots <- 20
knot_list <- quantile( d2$year , probs=seq(0,1,length.out=num_knots) )

library(splines)
B <- bs(d2$year, knots = knot_list[-c(1,num_knots)], degree = 3, intercept = T)

#plot basis functions
plot(NULL, xlim = range(d2$year), ylim = c(0,1), xlab = 'year', ylab = 'basis')
for (i in 1:ncol(B)){lines(d2$year, B[,i])}
length(seq(0,1,length.out=num_knots))

plot(NULL, xlim = range(d2$year), ylim = c(0,1), xlab = 'year', ylab = 'basis')
for (i in 1:ncol(B)){lines(d2$year, B[,i])}

#Regression
m4.7 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + B %*% w,
    a ~ dnorm(100, 10),
    w ~ dnorm(0, 1), #notice how weights can be negative w/ abs val > 1
    sigma ~ dexp(1)),
  data = list(D= d2$doy, B = B),
  start = list(w=rep(0,ncol(B))))

precis(m4.7) #we don't see the weights



#Try extracting the MAP for the weights and draw out the associated bases
post <-  extract.samples(m4.7)
w <- apply(post$w,2,mean)
plot(NULL, xlim = range(d2$year), ylim = c(-6,6), xlab = 'year', 
     ylab = 'basis * weight')
for (i in 1:ncol(B)){
  lines(d2$year, w[i] * B[,i])
}

#97% posterior 
mu <- link(m4.7)
mu_PI <- apply(mu, 2, PI, .97)
plot(d2$year, d2$doy, col = col.alpha(rangi2, 0.3),  pch = 16)
shade(mu_PI, d2$year, col = col.alpha('black',0.5))

#General idea is that you can overfit by having too many knots.
#The fit becomes filled with high peaks and deep troughs.
#If one lowers the w prior's stdev, one ends up with a
#much flatter fit with lower peaks and shallower valleys.


#####
#4H1#
#####

data("Howell1")
d2 <- Howell1[Howell1$age >= 18,]
xbar <- mean(d2$weight)

m4.3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <-  a + b * (weight-xbar),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), data = d2
)

post <- extract.samples(m4.3)

prediction <- post$a + post$b * (46.95 - xbar)

PI(prediction, .9)
HPDI(prediction, .9)
mean(prediction)
dens(prediction)

#Actually, for the interval calc, you need...

pred_sample <- rnorm(1e4, post$a + post$b * (46.95 - xbar), sd = post$sigma)

HPDI(pred_sample,.89)
#Why is it necessary to sample from rnorm as shown above?
#Because otherwise we'd be only accounting for the randomness in 
#a and b only, and not the randomness in the data itself,
#which is encapsulated by sigma


#4H2
d3 <- Howell1[Howell1$age<18,]
dim(d3)
xbar2 <- mean(d3$weight)

m4h2 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * (weight - xbar2),
    a ~ dnorm(40, 10),
    b ~ dlnorm(0, 5),
    sigma ~ dexp(.5)),
  data = d3
)

precis(m4h2)

post <- extract.samples(m4h2)

#what does a 10 kg increase in weight 
#mean for height according to this model?
height_inc <- post$b * 10
mean(height_inc)
dens(height_inc)

#try plotting the data
plot(d3$height~d3$weight)
abline(a = mean(post$a) - mean(post$b) * xbar2, b = mean(post$b))

post_h_m <- rnorm(1e4, post$a + post$b * (mean(d3$weight) - xbar2), post$sigma)
PI(post_h_m, .89)
mean(d3$weight)

weight_seq <- seq(min(d3$weight), max(d3$weight), length.out=1000)

#Consumes too much memory
mu <- sapply(weight_seq, function(z) rnorm(1e4, post$a + post$b * (z - xbar2), post$sigma))
mu <- NULL

#So, instead, just use HPDI directly 
mu.CI <- sapply(weight_seq, function(z) HPDI(rnorm(1e4, post$a + post$b * (z - xbar2), post$sigma), .89))

#And this should be okay by SLN
mu <- sapply(weight_seq, function(z) mean(rnorm(1e4, post$a + post$b * (z - xbar2), post$sigma), .89))
shade(mu.CI, weight_seq, col = col.alpha(rangi2,0.2))
lines(weight_seq, mu, lty = 2)


#####
#4H3#
#####
xbar3 <- mean(log(Howell1$weight))
m4h3 <- quap(
  alist(height ~ dnorm(mu, sigma),
  mu <- a + b * (log(weight) - xbar3),
  a ~ dnorm(120,20),
  b ~ dlnorm(0, 1),
  sigma ~ dunif(0,50)),
  data = Howell1
)

m4h3_precis <- precis(m4h3)

m4h3_precis
#What is the height at log(weight) = 0?
-xbar3 * m4h3_precis$mean[2] + m4h3_precis$mean[1]
#negative, apparently.
#But a weight of 1kg is improbable.
m4h3_precis$mean[2]

#Plotting
plot(Howell1$height~Howell1$weight)
post <- extract.samples(m4h3)

weight_seq <- seq(min(Howell1$weight),max(Howell1$weight),length.out=1e4)
mu <- sapply(weight_seq, function(x) mean(rnorm(1e4,post$a + post$b * (log(x)-xbar3),post$sigma)))
h.CI <- sapply(weight_seq, function(x) HPDI(rnorm(1e4,post$a + post$b * (log(x)-xbar3),post$sigma),.97))

shade(h.CI, weight_seq, col = col.alpha(rangi2, 0.3))
lines(weight_seq, mu, col='red', lty =2 )

#If you want to plot the interval for the mean
mu.CI <- sapply(weight_seq, function(x) HPDI(post$a + post$b * (log(x) - xbar3)))

dens(mu.CI)

#Notice how the variance increases with weight 
#in the height graph


#4H4
d <- Howell1
d$weight_s <- ( d$weight - mean(d$weight) )/sd(d$weight)
d$weight_s2 <- d$weight_s^2
m4.5 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*weight_s + b2*weight_s2 ,
    a ~ dnorm( 178 , 20 ) ,
    b1 ~ dlnorm( 0 , 1 ) ,
    b2 ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d )

prior_a <- rnorm(1e3, 178, 20)
prior_b1 <- rlnorm(1e3,0,1)
prior_b2 <- rnorm(1e3,0,1)
sigma <- runif(1e3,0,50)

weight_seq <- seq(min(d$weight), max(d$weight), length.out=1e3)
weight_seq_scaled <- scale(weight_seq)
weight_seq_scaled <- as.numeric(weight_seq_scaled)

prior_h_CI <- sapply(weight_seq_scaled, 
                    function(x) HPDI(rnorm(1e3, prior_a + prior_b1 * x + prior_b2 * x^2, sigma),.97))

prior_mu <- sapply(weight_seq_scaled, 
                    function(x) mean(rnorm(1e3, prior_a + prior_b1 * x + prior_b2 * x^2, sigma),.97))
plot(d$height ~ d$weight)

shade(prior_h_CI, weight_seq)
lines(weight_seq, prior_mu)


#Another way is to use extract prior
prior <- extract.prior(m4.5)

prior_h_CI <-sapply(weight_seq_scaled, 
                    function(x) HPDI(rnorm(1e3, prior$a + prior$b1 * x + prior$b2 * x^2, prior$sigma),.97))

shade(prior_h_CI, weight_seq)

#How would I adjust the priors to make the fit more reasonable...?
m4.5.2 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*weight_s + b2*weight_s2 ,
    a ~ dnorm(0 , 20 ) , #height when weight is 0 
    b1 ~ dlnorm( 4 , 1 ) ,
    b2 ~ dnorm( -0, 0.5 ) , #negative mean
    sigma ~ dunif( 0 , 50 )
  ) , data=d )

plot(d$height~d$weight)

prior <- extract.prior(m4.5.2)

prior_CI <- sapply(weight_seq_scaled, function(x) HPDI(rnorm(1e3, 
                                                             prior$a+ prior$b1 * x + prior$b2 * x^2, sd = prior$sigma),
                                           .95))

prior_mu <- sapply(weight_seq_scaled, function(x) mean(rnorm(1e3, 
                                                             prior$a+ prior$b1 * x + prior$b2 * x^2, sd = prior$sigma),
                                                       .95))
shade(prior_CI, weight_seq)
lines(weight_seq, prior_mu)

#4H5
data("cherry_blossoms")
cherry <- cherry_blossoms

cherry <- cherry[complete.cases(cherry$doy),]
cherry <- cherry[complete.cases(cherry$temp),]

m4h5 <- quap(
  alist(
    doy ~ dnorm(mu, sigma),
    mu <- a + b * temp,
    a ~ dnorm(100, 10),
    b ~ dnorm(-1, 2),
    sigma ~ dexp(0.5)
  ), data = cherry
)

plot(cherry$doy~cherry$temp)
post <- extract.samples(m4h5)

precis(post)

temp_seq <- seq(min(cherry$temp), max(cherry$temp), length.out = 1e4)

mu <- sapply(temp_seq, function(x)
  mean(rnorm(1e4, post$a + post$b * x, post$sigma)))

CI <- sapply(temp_seq, function(x)
  HPDI(rnorm(1e4, post$a + post$b * x, post$sigma),.97))

mu.CI <- sapply(temp_seq, function(x) HPDI(
                post$a + post$b * x, .97))

dim(mu.CI)

temp_seq[1]

shade(CI, temp_seq)
lines(temp_seq, mu, lty = 2)

shade(mu.CI, temp_seq, col = col.alpha(rangi2,0.25))

#There is clearly a strong linear relationship between the two

#4H6
d2 <- cherry_blossoms[complete.cases(cherry_blossoms$doy),]
d2 <- d2[complete.cases(d2$year),]

library(splines)
num_knots <- 15
knot_list <- quantile(d2$year, probs = seq(0,1, length.out = num_knots))
B <- bs(d2$year, knots = knot_list[-c(1,num_knots)], degree = 3, intercept = T)

m4.7 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + B %*% w,
    a ~ dnorm(100, 10),
    w ~ dnorm(0, 10), #notice how weights can be negative w/ abs val > 1
    sigma ~ dexp(1)),
  data = list(D= d2$doy, B = B),
  start = list(w=rep(0,ncol(B))))


plot(d2$doy~d2$year)
#prior
mu <- link(m4.7, post = prior)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, HPDI, .97)
shade(mu.PI, d2$year, col = col.alpha(rangi2, 0.35))
lines(d2$year, mu.mean, lty = 2, lwd = 2, col = rangi2)

for (i in 1:100){
  lines(d2$year, mu[i,], col = 'grey75')
}

library(rethinking)
#Try shifting w
m47b <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + B %*% w,
    a ~ dnorm(100, 10),
    w ~ dnorm(20, 10), #notice how weights can be negative w/ abs val > 1
    sigma ~ dexp(1)),
  data = list(D= d2$doy, B = B),
  start = list(w=rep(0,ncol(B))))

plot(d2$doy~d2$year)
#prior
prior <- extract.prior(m47b)
mu <- link(m47b, post = prior)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, HPDI, .89)
shade(mu.PI, d2$year, col = col.alpha(rangi2, 0.35))
lines(d2$year, mu.mean, lty = 2, lwd = 2, col = rangi2)

for (i in 1:100){
  lines(d2$year, mu[i,], col = 'grey75')
}

#You can see how the predicted doy line moves up

m47c <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + B %*% w,
    a ~ dnorm(100, 10),
    w ~ dnorm(0, 1), #notice how weights can be negative w/ abs val > 1
    sigma ~ dexp(1)),
  data = list(D= d2$doy, B = B),
  start = list(w=rep(0,ncol(B))))

plot(d2$doy~d2$year)
#prior
prior <- extract.prior(m47c)
mu <- link(m47b, post = prior)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, HPDI, .89)
shade(mu.PI, d2$year, col = col.alpha(rangi2, 0.35))
lines(d2$year, mu.mean, lty = 2, lwd = 2, col = rangi2)

for (i in 1:100){
  lines(d2$year, mu[i,], col = 'grey75')
}

#Now, the band of uncertainty has changed above 
#now that sigma for the prior of w has gotten much smaller
#further, you can see that the splines are far less wiggly.

#4H8
#Try the spline model without an intercept
knot_list <- quantile(d2$year, seq(0, 1, length.out = num_knots))
B <- bs(d2$year, knots = knot_list[-c(1,15)], degree = 3, intercept = T)

m4h8 <- quap(alist(
  D ~ dnorm(mu, sigma),
  mu <- B %*% w,
  w ~ dnorm(10, 10),
  sigma ~ dexp(1)), data = list(D = d2$doy, B=B),
  start = list(w=rep(0,ncol(B)))
  )

post <- extract.samples(m4h8)
linkf <- link(m4h8, post = post)  
#Without an intercept, sigma does not converge;
#it's no longer positive definite
#main issue is that, since w is mostly zero, you have no 
#way to get positive weights that will push the actual predictions
#to be positive
#the only way to achieve it without an inercept is to
#set the w priors to be positive.
mu <- apply(linkf,2,mean)
mu.CI <- apply(linkf,2,HPDI,.89)

plot(d2$doy~d2$year)

shade(mu.CI,d2$year, col = col.alpha(rangi2, 0.5))
lines(d2$year, mu, lty = 2 , col = rangi2)
#notice how the predictions at the boundaries are far more leaning
#toward zero on the y axis
