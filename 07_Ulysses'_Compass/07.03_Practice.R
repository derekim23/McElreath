#7E1. State the three motivating criteria that define information entropy. Try to express each in your own words.
#A: Information entropy should...
#1. be measured on a continuous scale such that the spacing between adjacent values is consistent
#2. capture the probability space s.t. each space's value scales with the representive probability mass
#3. be additive for independent events

#7E2. Suppose a coin is weighted such that, when it is tossed and lands on a table, it comes up heads 70% of the time. What is the entropy of this coin?
p <- c(.7, .3)
(h <- -sum(p*log(p,2)))

#7E4. Suppose another four-sided die is loaded such that it never shows “4”. The other three sides show equally often. What is the entropy of this die?
#Skipping 7E3, because it's essentially the same as this question.
p <- rep(1/3,3)
-sum(p*log(p,2))

#7M1 - Write down and compare the definitions of AIC and WAIC. 
#Which of these criteria is most general? Which assumptions are 
#required to transform the more general criterion into a less 
#general one?

#AIC = -2lppd + 2p
#WAIC = -2lppd + 2sum_i var(log(p(y_i|theta)))
#WAIC makes no assumptions about the posterior shape and
#therefore is more general.
#WAIC becomes (?) AIC if the posterior is MVN
#and that the prior is either flat or completely overhwlemed
#by the data.

#7M2 Explain the difference between model selection and 
#model comparison. What information is lost under model selection?”

#Model comparison is just that, comparing models.
#A byproduct of it is that one may have the opportunity
#to test multiple causal structures underlying the data.

#Model selection, on the other hand, is purely about improving
#predictions.

#7M3 When comparing models with an information criterion, 
#why must all models be fit to exactly the same observations? 
#What would happen to the information criterion values, 
#if the models were fit to different numbers of observations? 
#Perform some experiments, if you are not sure.

#It wouldn't be a fair comparison.
#Certain observations have more entropy/surprise and therefore
#contribute more to information.
#For example, by including the highest entropy observation
#in one model and not the other, I would be effectively
#penalizing the one with it more.
data(WaffleDivorce)
d <- WaffleDivorce
d$D <- standardize(d$Divorce)
d$A <- standardize(d$MedianAgeMarriage)
d$M <- standardize(d$Marriage)

m_no_ID <- quap(alist(
  D ~ dnorm(mu, sigma),
  mu <- a + bA * A + bM * M,
  a ~ dnorm(0,0.2),
  c(bA, bM) ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
), data = d[d$Location != 'Idaho',])

m_no_CA <- quap(alist(
  D ~ dnorm(mu, sigma),
  mu <- a + bA * A + bM * M,
  a ~ dnorm(0,0.2),
  c(bA, bM) ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
), data = d[d$Location != 'California',])

compare(m_no_ID, m_no_CA, func = WAIC)

#Notice how, by including a big outlier, Idaho, which has
#low median age of marriage but low divorce rates, we
#effectively penalize the same exact model more.

#7M4 What happens to the effective number of parameters, 
#as measured by PSIS or WAIC, as a prior becomes more 
#concentrated? Why? Perform some experiments, 
#if you are not sure.

#They should decrease, as we're regularizing to prevent 
#overfitting.

m_narrow <- quap(alist(
  D ~ dnorm(mu, sigma),
  mu <- a + bA * A + bM * M,
  a ~ dnorm(0,0.1),
  c(bA, bM) ~ dnorm(0, 0.25),
  sigma ~ dexp(1)
), data = d)

m_wide <- quap(alist(
  D ~ dnorm(mu, sigma),
  mu <- a + bA * A + bM * M,
  a ~ dnorm(0,1),
  c(bA, bM) ~ dnorm(0, 1.5),
  sigma ~ dexp(1)
), data = d)

WAIC(m_narrow)
WAIC(m_wide)

#7M5 “Provide an informal explanation of why informative priors 
#reduce overfitting.”

#Because they are not going to be easily overwhelmed by the
#likelihood, retaining some or even most of its prior shape.
#By extension, this means narrow priors are far more robust
#to outliers as well.

#7M6 “Provide an informal explanation of why overly informative 
#priors result in underfitting.
#Consider the dirac delta function.
#Pretty much the prior will never be updated
#regardless of how much information you get from
#the likelihood, because the probability density is zero
#everywhere except at one point.

#Which means the model would never update.
#While this is an extreme case, it's not hard to imagine
#that narrower distributions would naturally update less,
#especially if the likelihood falls far outside of where
#the probability mass is concentrated.

data(Laffer)
dL <- Laffer
dL$t <- standardize(Laffer$tax_rate)
dL$r <- standardize(Laffer$tax_revenue)

m_linear <- quap(alist(
  r ~ dnorm(mu, sigma),
  mu <- a + b * t,
  a ~ dnorm(0, 0.5),
  b ~ dnorm(0, 1),
  sigma ~ dexp(1)
), data = dL)

m_quad <- quap(alist(
  r ~ dnorm(mu, sigma),
  mu <- a + b * t + b2  * t^2,
  a ~ dnorm(0, 0.5),
  c(b, b2) ~ dnorm(0, 1),
  sigma ~ dexp(1)
), data = dL)

plot(dL$t, dL$r)

num_knots <- 4
knot_list <- quantile( dL$t , probs=seq(0,1,length.out=num_knots) )

library(splines)
B <- bs(dL$t, knots = knot_list[-c(1,num_knots)], degree = 3, intercept = T)

#There is basis function for each row
dim(B)

#plot basis functions
plot(NULL, xlim = range(dL$t), ylim = c(0,1), xlab = 'rate', ylab = 'rev')
for (i in 1:ncol(B)){lines(dL$t, B[,i])}

#Regression
m_spline <- quap(
  alist(
    r ~ dnorm(mu, sigma),
    mu <- a + B %*% w,
    a ~ dnorm(0, 0.2),
    w ~ dnorm(0, 1), #notice how weights can be negative w/ abs val > 1
    sigma ~ dexp(1)),
  data = list(D= dL$t, B = B),
  start = list(w=rep(0,ncol(B))))

plot_df <- data.frame(t = seq(min(dL$t), max(dL$t), length.out = 1e2))
link_m_linear <- link(m_linear, plot_df)
link_m_quad <- link(m_quad, plot_df)
#link_m_spline <- link(m_spline, plot_df)

m_linear_mean <- apply(link_m_linear,2, mean)
m_quad_mean <- apply(link_m_quad,2, mean)

m_linear_PI <- apply(link_m_linear,2, PI, 0.95)
m_quad_PI <- apply(link_m_quad,2, PI, 0.95)

plot(dL$t,dL$r)
lines(plot_df$t, m_linear_mean)
lines(plot_df$t, m_quad_mean, col = col.alpha(rangi2, 0.8), lty = 2)

shade(m_linear_PI, plot_df$t)
shade(m_quad_PI, plot_df$t, col = col.alpha(rangi2, 0.25))

compare(m_linear, m_quad, func = WAIC)

#Either way, the difference in fit is not meaningfully large
#enough, given dSE > abs(delta(WAIC))

#The relationship between tax rate and revenue
#is perhaps does not exhibit as strong of a
#diminishing rate of returns as implied by WSJ's chart.

#WSJ is overfitting to the outliers.

#7H2
WAIC_linear = WAIC(m_linear, pointwise = T)
WAIC_quad = WAIC(m_quad, pointwise = T)

WAIC_linear[dL$r>3,'WAIC']/sum(WAIC_linear$WAIC)
WAIC_quad[dL$r>3,'WAIC']/sum(WAIC_quad$WAIC)
#A very large amount of either model's KLD attributed to this
#particular point.

m_linear2 <- quap(alist(
  r ~ dstudent(2, mu, sigma),
  mu <- a + b * t,
  a ~ dnorm(0, 0.5),
  b ~ dnorm(0, 1),
  sigma ~ dexp(1)
), data = dL)

m_quad2 <- quap(alist(
  r ~ dstudent(2, mu, sigma),
  mu <- a + b * t + b2  * t^2,
  a ~ dnorm(0, 0.5),
  c(b, b2) ~ dnorm(0, 1),
  sigma ~ dexp(1)
), data = dL)

compare(m_linear, m_quad, m_linear2, m_quad2, func = WAIC)
#Significant improvement with student priors.
birds <- data.frame(
  matrix(c(rep(0.2,5), 0.8,0.1,0.05, 0.025, 0.025, 
           0.05, 0.15, 0.7, 0.05, 0.05), nrow = 3))

rownames(birds) = c('i1','i2','i3')
colnames(birds) = c('a','b','c','d','e')

indiv_entropy <- apply(birds,1,function(x) -x*log(x,2))

apply(indiv_entropy,2,sum)
#We have decreasing order of entropy.
#Flatter the distribution, higher the entropy,
#because extreme values become more likely.

install.packages('LaplacesDemon')
library(LaplacesDemon)

for (i in 1:2){
  for (j in 2:3)
  {
    if (i == j) next
    kld <- KLD(as.numeric(birds[i,]),as.numeric(birds[j,]),2)
    print(paste("Between islands ",i," and ", j, ': ',
                kld$sum.KLD.px.py, sep = ""))
    print(paste("Between islands ",j," and ",i, ': ',
                kld$sum.KLD.py.px, sep = ""))
    
  }
}


#7H4
