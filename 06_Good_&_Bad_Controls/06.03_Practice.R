#6M1 --Skipping trivial problems
library(dagitty)
library(dplyr)
library(rethinking)
dag6m1<- dagitty("dag{
 U [unobserved]
 V [unobserved]
 X -> Y
 X <- U -> B <- C -> Y
 U <- A -> C
 C <- V -> Y
 }")
coordinates(dag6m1)<-list(
    x=c(X=0,Y=2,U=0,A=1,B=1,C=2,V=2.5),
    y=c(X=2,Y=2,U=1,A=0.2,B=1.5,C=1,V=1.5)
)

dag6m1 %>% drawdag
dag6m1 %>% adjustmentSets(exposure = 'X', outcome = 'Y')

#Three paths connect Y and X through backdoors.
#By conditioning on A, we can block non-collider paths.

#6M2
library(MASS)
set.seed(99)
N = 100
X <- rnorm(N)
Z <- rnorm(N, X, sd = 0.25)
cor(X,Z)
Y <- rnorm(N, Z)
d <- data.frame(X = X, Y = Y, Z = Z)

m6m2 <- quap(alist(
  Y ~ dnorm(mu, sigma),
  mu <- a + bXY * X + bZY* Z,
  a ~ dnorm(0, 0.2),
  bXY ~ dnorm(0,0.5),
  bZY ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
), data = d)

precis(m6m2)

#bZY is somewhat below 1, but bXY's credibilty interval
#includes zero, suggesting that it's not surely impactful.
#And this should be the case given that we should have
#conditional independence between X and Y.

#6M3
#First and second graphs have two backdoor paths.
#Just need to condition on Z in the first.
#For second, no conditioning needed, as Z is a collider for A,
#and we want the total causal effect anyway.
#Third, no need to condition on A, since Z is a collider.
#So, condition on nothing.
#Fourth, Condition on A, but not on Z, since Z does not
#have an arrow going into X; recall, you're lookin at the 
#total causal influence.

#6H1
#Back to the Waffle Divorce Example.
#Based on the following DAG, can regress D on W
#and S.
library(dagitty)
dag_6.2 <- dagitty( 'dag {
  A -> D
  A -> M -> D
  A <- S -> M
  S -> W -> D
}')

coordinates(dag_6.2) <- 
  list(
    x= c(S =0, A = 0, M = 0.5, D = 1, W =1),
    y = c(A = 0, D = 0, M = -1, S = -2, W = -2)
  )
drawdag(dag_6.2)
adjustmentSets( dag_6.2 , exposure='W', outcome='D' )
data("WaffleDivorce")

d <- NULL
d$S <- WaffleDivorce$South + 1
d$D <- standardize(WaffleDivorce$Divorce)
d$W <- standardize(WaffleDivorce$WaffleHouses)

m6h1 <- quap(alist(
  D ~ dnorm(mu,sigma),
  mu <- a[S] + bW * W,
  a[S] ~ dnorm(0, 0.5),
  bW ~ dnorm(0,1),
  sigma ~ dexp(1)
), data = d)

precis(m6h1)
#Controlling for S, it seems W has not too significant of an
#impact.
#Note that the "southern" variable has been indexed.

#6h2
d$A <- standardize(WaffleDivorce$MedianAgeMarriage)
d$M <- standardize(WaffleDivorce$Marriage)

impliedConditionalIndependencies(dag_6.2)

m6h2.1 <- quap(alist(
  W ~ dnorm(mu, sigma),
  mu <- a[S] + bA * A,
  bA ~ dnorm(0,1),
  a[S] ~ dnorm(0,0.5),
  sigma ~ dexp(1)
), data = d)
precis(m6h2.1, depth = 2)
#bA is close to zero given S

m6h2.2 <- quap(alist(
  D ~ dnorm(mu, sigma),
  mu <- a[S] + bA * A + bM * M + bW * W,
  c(bA, bM, bW) ~ dnorm(0,1),
  a[S] ~ dnorm(0,0.5),
  sigma ~ dexp(1)
), data = d)
precis(m6h2.2, depth = 2)
#a[S] close to zero.

m6h2.3 <- quap(alist(
  M ~ dnorm(mu, sigma),
  mu <- a[S] + bW * W,
  bW ~ dnorm(0,1),
  a[S] ~ dnorm(0,0.5),
  sigma ~ dexp(1)
), data = d)
precis(m6h2.3, depth = 2)
#bW almost zero



#6H3
data(foxes)
dag6h3 <- dagitty('dag{
    a->f
    f->g
    g->w
    f->w
        }')

coordinates(dag6h3) <- list(
  x= c(a = 0, g = 1, w = 0.5, a = 0.5),
  y = c(a = 0, g = 0, w = 1, a = -1)
)

drawdag(dag6h3)

d <- NULL
d$a <- standardize(foxes$area)
d$f <- standardize(foxes$avgfood)
d$w <- standardize(foxes$weight)
d$g <- standardize(foxes$groupsize) #i don't like it
#but is there another way?
#given all other variables are standardized, i don't think so.
#I can try scaling, but scaling effective bounds the values,
#which i don't think makes much sense.

dag6h3 %>% adjustmentSets(exposure = 'a', outcome = 'w')

m6h3 <- quap(alist(
  w~dnorm(mu, sigma),
  mu <- b + ba * a,
  b ~ dnorm(0,0.25),
  ba ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
), data = d)

dev.off()
prior_m6h3 <- extract.prior(m6h3)
aseq <- seq(-3,3,length.out = 100)
mu <- link(m6h3, post = prior_m6h3, data = list(a=aseq))
plot(NULL, xlim = c(-3,3), ylim = c(-3,3),
     xlab = 'weight standardized',
     ylab = 'area standardized',
     main = 'weight v. area prior predictive')

for (i in 1:length(aseq)){
  lines(aseq, mu[i,], lwd = 0.3, col = col.alpha(rangi2, 0.5))
}
#we expect b to be zero, since weight should be zero if 
#area is zero.
#as for ba, we expect it to be mostly positive, but
#we keep a fairly ignorant prior

precis(m6h3)
#area does not seem to. have a meaningful impact

#m6h4
dag6h3 %>% adjustmentSets(exposure = 'f', outcome = 'w')
m6h4 <- quap(alist(
  w ~ dnorm(mu, sigma),
  mu <- b + bf * f,
  b ~ dnorm(0, 0.2),
  bf ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
), data = d)

precis(m6h4)
#again, no meaningful impact from f on w.

#6h5
dag6h3 %>% adjustmentSets(exposure = 'g', outcome = 'w')
m6h5 <- quap(alist(
  w ~ dnorm(mu, sigma),
  mu <- b + bf * f + bg * g,
  b ~ dnorm(0, 0.2),
  c(bf, bg) ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
), data = d)

precis(m6h5)
plot(precis(m6h5))
#Group size has a negative impact controlled for food
#Perhaps they have to fight amongst each other 
#rather than collaborate for food?
#Or it's that they have to share morsels with many mouths?
#On the other hand, adjusted for group size, 
#food has a meaningful direct positive impact on weight. 
#I believe this is an example of Simpson's paradox.
#area affects weight through avg food, which has zero
#total impact, so it makes sense that area has zero impact.
#question becomes why the total impact is zero for food.
#I think it's because avg food and group size are correlated.
plot(d$f,  d$g)
cor(d$f, d$g)
#Because group size has a negative impact on weight,
#and the two variables are so highly correlated,
#the impact through group size was effectively cancelling
#out the direct impact on weight.
