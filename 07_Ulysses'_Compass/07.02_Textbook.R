#Consider the brain size v. body mass relationship of various hominids
sppnames <- c( 'afarensis','africanus','habilis','boisei',
               'rudolfensis','ergaster','sapiens')
brainvolcc <- c( 438 , 452 , 612, 521, 752, 871, 1350 )
masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 61.0 , 53.5 )
d <- data.frame( species=sppnames , brain=brainvolcc , mass=masskg)
d$mass_std <- (d$mass - mean(d$mass))/sd(d$mass)
d$brain_std <- (d$brain)/max(d$brain)

m7.1 <- quap(alist(
  brain_std~dnorm(mu,exp(log_sigma)),
  mu <- a + b * mass_std,
  a~ dnorm(0.5, 1),
  b ~ dnorm(0, 10),
  log_sigma ~ dnorm(0,1)
), data = d)

m7.1_OLS <- lm(brain_std ~ mass_std, data = d)
post <- extract.samples(m7.1_OLS)
#While OLS is not strictly bayesian, the poster distribution you get form 
#it can be considered to be from a uniform prior.

#Notice that, in R, var is the debiased variance estimator.
#var2 is the MLE counterpart.
var2(c(1,2,3))
var(c(1,2,3))

#To calculate R^2...
set.seed(12)
s <- sim(m7.1)
r <- apply(s,2,mean) - d$brain_std
resid_var <- var2(r)
outcome_var <- var2(d$brain_std)
1-resid_var/outcome_var

#Functionalize
R2_is_bad <- function(quap_fit){
  s <- sim(quap_fit, refresh = 0)
  r <- apply(s,2,mean) - d$brain_std
  resid_var <- var2(r)
  outcome_var <- var2(d$brain_std)
  1-resid_var/outcome_var
}

#Try fitting a linear model with up-to quadratic features.
m7.2 <- quap(alist(
    brain_std ~ dnorm(mu, exp(log_sigma)),
    mu <- a + b[1]*mass_std + b[2] * mass_std^2,
    a ~ dnorm(0.5,1),
    b ~ dnorm(0, 10),
    log_sigma ~ dnorm(0,1)
  ), data = d, start = list(b= rep(0,2))
) #the start vector shows the function how many times we should draw
#from the same feature

m7.3 <- quap(alist(
    brain_std ~ dnorm(mu, exp(log_sigma)),
    mu <- a + b[1]*mass_std + b[2] * mass_std^2 + b[3] * mass_std^3,
    a ~ dnorm(0.5,1),
    b ~ dnorm(0, 10),
    log_sigma ~ dnorm(0,1)
  ), data = d, start = list(b= rep(0,3))
)

m7.4 <- quap(alist(
  brain_std ~ dnorm(mu, exp(log_sigma)),
  mu <- a + b[1]*mass_std + b[2] * mass_std^2 + b[3] * mass_std^3
    + b[4] * mass_std^4,
  a ~ dnorm(0.5,1),
  b ~ dnorm(0, 10),
  log_sigma ~ dnorm(0,1)
), data = d, start = list(b= rep(0,4))
)

m7.5 <- quap(alist(
  brain_std ~ dnorm(mu, exp(log_sigma)),
  mu <- a + b[1]*mass_std + b[2] * mass_std^2 + b[3] * mass_std^3
    + b[4]*mass_std^4 + b[5] * mass_std^5,
  a ~ dnorm(0.5,1),
  b ~ dnorm(0, 10),
  log_sigma ~ dnorm(0,1)
), data = d, start = list(b= rep(0,5))
)

m7.6 <- quap(alist(
  brain_std ~ dnorm(mu, 0.001),
  mu <- a + b[1]*mass_std + b[2] * mass_std^2 + b[3] * mass_std^3
  + b[4]*mass_std^4 + b[5] * mass_std^5 + b[6] * mass_std^6,
  a ~ dnorm(0.5,1),
  b ~ dnorm(0, 10)
), data = d, start = list(b= rep(0,6))
)
#For the final model, sigma is replaced with 0.001, as the model
#fit does not work with the standard lognormal prior.
#Why this is will be made clearer later.

post <- extract.samples(m7.1)
mass_seq <- seq(from = min(d$mass_std),to = max(d$mass_std), 
                length.out = 100)


set.seed(1)
lppd(m7.1, n = 1e4)
#This calculates the log of the average probability for
#each observation i, where the avg is taken over the posterior.
#The above is used for deviance score calculation, which is
#based on KL DIVERGENCE.
#If you sum the lppd scores, you will get the total log score
#for the model and data.
#Larger values are better as they imply higher accuracy.
#the deviance is the sum of these scores multiplied by -2.

set.seed(1)
logprob <- sim( m7.1 , ll=TRUE , n=1e4 )
n <- ncol(logprob)
ns <- nrow(logprob)
f <- function( i ) log_sum_exp( logprob[,i] ) - log(ns)
( lppd <- sapply( 1:n , f ) )
#The above produces the same values as before.
#You're literally averaging the probabilities.
#So, logprob generates log probabilities of the posterior.
#each row is a sample.
#log_sum_exp takes the log prob, exponentiates each, sums them
#and then takes a log again.

#I still don't get how logprob can contain positive numbers?

lppd

#I think what he's showing is deviance...no?
#But then why is the last column negative?

#THe issue with the above is that the number always
#improves as the model gets more complex as does R^2

set.seed(1)
sapply(list(m7.1, m7.2, m7.3, m7.4, m7.5, m7.6), function(m)
  sum(lppd(m)))


#How does WAIC work?
data(cars)
m <- quap(alist(
  dist ~ dnorm(mu, sigma),
  mu <- a + b * speed,
  a ~ dnorm(0, 100),
  b ~ dnorm(0, 10),
  sigma ~ dexp(1)
), data = cars)

set.seed(100)
post <- extract.samples(m, n = 1e3)
n_samples <- 1e3
logprob <- sapply(1:n_samples, function(s){
  mu <- post$a[s] + post$b[s]*cars$speed
  dnorm(cars$dist, mu, post$sigma[s], log = T)
})
#each col is a simulated outcome.

logprob[logprob > 0] #rightfully, this should return nothing.
#So, why is it that his previous example had positive values?

n_cases <- nrow(cars)
lppd <- sapply(1:n_cases, function(i) log_sum_exp(logprob[i,]) -
                 log(n_samples))
log_sum_exp(logprob[1,])
#the function is confusing, but the idea is that
#you are converting the log probabilities back to 
#probabilities, summing them up, and the taking the log again.
log(sum(exp(logprob[1,])))

sum(lppd)

pWAIC <- sapply(1:n_cases, function(i) var(logprob[i,]))

-2* (sum(lppd) + sum(pWAIC))
#Note that each individual observation has an associated
#penalty and lppd - you can study how different obs
#contribute to overfitting this way.

set.seed(77)
compare( m6.6 , m6.7 ,m6.8, func=WAIC )
#The above compares the WAIC of three models
compare( m6.6 , m6.7 ,m6.8, func=PSIS)


#To see whether the difference in WAIC
#between m6.7 and m6.8 are significant
#we can check the standard error of the difference
set.seed(91)
waic_m6.7 <- WAIC( m6.7 , pointwise=TRUE )$WAIC
waic_m6.8 <- WAIC( m6.8 , pointwise=TRUE )$WAIC
n <- length(waic_m6.7)
diff_m6.7_m6.8 <- waic_m6.7 - waic_m6.8
sqrt( n*var( diff_m6.7_m6.8 ) )
#Standard error is for the sum of differences
#and hence the multiplication by n
#diff/se ~ N(0,1)
compare( m6.6 , m6.7 ,m6.8, func=WAIC )
plot(compare(m6.6, m6.7, m6.8))

#the weight in the compare table is
#exp(-0.5 delta of WAIC between model i and the best model)
#divided by the same for other models
#weights are also used in model averaging.
#These weights can be thought of as the probability
#of being the best choice.

#WAIC is a measure of out of sample error
#lower WAIC should mean better predictive accuracy
#So, WAIC is for prediction, not for causal analysis.

set.seed(92)
waic_m6.6 <- WAIC( m6.6 , pointwise=TRUE )$WAIC
diff_m6.6_m6.8 <- waic_m6.6 - waic_m6.8
sqrt( n*var( diff_m6.6_m6.8 ) )

set.seed(93)
compare( m6.6 , m6.7 , m6.8 )@dSE


#Revisiting the divorce data
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
d$A <- standardize( d$MedianAgeMarriage )
d$D <- standardize( d$Divorce )
d$M <- standardize( d$Marriage )

m5.1 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bA * A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

m5.2 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM * M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

m5.3 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M + bA*A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

set.seed(24071847)
compare( m5.1 , m5.2 , m5.3 , func=PSIS )

#The warning means that the smoothing approximation that PSIS 
#uses is unreliable for some points. Recall from the section 
#on PSIS that when a point’s Pareto k value is above 0.5, 
#the importance weight can be unreliable. Furthermore, 
#these points tend to be outliers with unlikely values, 
#according to the model. As a result, they are highly 
#influential and make it difficult to estimate out-of-sample 
#predictive accuracy. Why? Because any new sample is unlikely 
#to contain these same outliers, and since these outliers were 
#highly influential, they could make out-of-sample predictions
#worse than expected. WAIC is vulnerable to outliers as well. 
#It doesn’t have an automatic warning. But it does have a way 
#to measure this risk, through the estimate of the overfitting 
#penalty.

#Compare PSIS to WAIC
set.seed(24071847)
PSIS_m5.3 <- PSIS(m5.3,pointwise=TRUE)
set.seed(24071847)
WAIC_m5.3 <- WAIC(m5.3,pointwise=TRUE)
plot( PSIS_m5.3$k , WAIC_m5.3$penalty , xlab='PSIS Pareto k',
      ylab='WAIC penalty' , col=rangi2 , lwd=2 )

#The heavy penalties are coming from Idaho and Maine
#where the former has a low median age of mariage but
#also a very low divorce rate.
#Maine is the opposite.
#The high penalties highlight an overfitting risk for each.

#For example, for Idaho, WAIC penalty is 2 for each free
#parameter; this is like having 8 additional parameters
#if we already have 4.
WAIC(m5.3)
#However, the above shows that the total penalty is actaully
#closer to 6.

#When there are only a couple of outliers like these
#you may have the option of fitting the model
#both with and without the outliers and then reporting each.
#However, what if there are quite a few outliers?

#One possibility is to use more robust priors like
#student t's.
#The generalized student t distribution
#has the sasme mu and sigma parameters as does the normal
#but it also has an additional nu parameter 
#that captures the shape; when nu is large, the tails are
#thinner; as nu approaches 1, the tails get very thick.

#Estimating nu takes a lot of data.
#And usually for robust regression, you won't have enough.
#So, instead, assume that nu is small in your priors.

#Let's re-estimate the marraige model with student t's.

m5.3t <- quap(
  alist(
    D ~ dstudent( 2 , mu , sigma ) ,
    mu <- a + bM*M + bA*A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

PSIS(m5.3t)
#The above no longer complains about Pareto k values.
#Which means Idaho is no longer as influential.

#How does this impact the posterior dist of the association
#between age at marriage and divorce?

precis(m5.3)
precis(m5.3t)

#bA has gotten more negative while bM has become zero.
#This is because Idaho is having less of an influence.

p <- c(1/3,1/3,1/3)

sum(-p * log(p))
  