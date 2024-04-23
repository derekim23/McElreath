#Simulation of Berkson's Paradox in academia

set.seed(1914)
N <- 200 # num grant proposals
p <- 0.1 # proportion to select
# uncorrelated newsworthiness and trustworthiness
nw <- rnorm(N)
tw <- rnorm(N)
# select top 10% of combined scores
nw <- rnorm(N)
tw <- rnorm(N)
s <- tw + nw
q <- quantile(s, 1-p)
selected <- ifelse(s >= q, T, F)
cor(tw[selected], nw[selected])

#Multi-collinearity - is it really a problem?
#Try simlulating how leg heights relate to overall height

N <- 100 # number of individuals
set.seed(909)
height <- rnorm(N,10,2) # sim total height of each
leg_prop <- runif(N,0.4,0.5) # leg as proportion of height
leg_left <- leg_prop*height + # sim left leg as proportion + error
  rnorm( N , 0 , 0.02 )
leg_right <- leg_prop*height + rnorm( N , 0 , 0.02 )
d <- data.frame(height, leg_left, leg_right)

#Since leg prop is .45 on average, we'd expect
#the beta coefficient on leg predicting a person's height
#to be around 1/.45 ~= 2.2.
#But will this be the case with both legs in the model?

m6.1 <- quap(alist(
  height~dnorm(mu,sigma),
  mu <- a + bl * leg_left + br * leg_right,
  a ~ dnorm(10, 100),
  bl ~ dnorm(2, 10),
  br ~ dnorm(2, 10),
  sigma ~ dexp(1)
), data = d)

plot(precis(m6.1))
#way off the mark.

#Recall that the multiple regression framework tries to answer
#for each variable, the impact each variable has
#on the output "knowing" the values of all other variables.

#The posterior distribution answers this question

post <- extract.samples(m6.1)
par(mfrow = c(1,2))
plot(bl~br, post, col = col.alpha(rangi2, 0.1), pch = 16)
dens(post$bl+post$br)
#Since both legs contain almost the same information
#and there are an infinite number of combinations of bl
#and br that result in the same output.

#Notice that the sum in of the two coefficients
#is centered around 2, implying that the true
#coefficient is around 2.1, which is darn close to
#2.2.

#One way to think of this phenomenon is that, the predictor
#variables are essentially identical.
#So, we're not fitting mu_i = a + b_1 * x_1i + b_2 * x_2i
#We're fitting mu_i = a + (b_1 + b_2) * x_i
#So, the sum of b_1 and b_2 is effected in concert in proportion
#to x_i; they cannot be pulled apart.
#So, the model just tries to generate a very wide range of
#answers for b_1 and b_2 that would net out to the overall
#effect of 2.2.

#This is a much more intuitive argument than saying that
#t(x)%*%x is nearly singular.

#Another example of multicollinearity
library(rethinking)
data(milk)
d <- milk
d$K <- standardize(d$kcal.per.g)
d$F <- standardize(d$perc.fat)
d$L <- standardize(d$perc.lactose)

m6.3 <- quap(alist(
  K ~ dnorm(mu, sigma),
  mu <- a + bF * F,
  a ~ dnorm(0, 0.2),
  bF ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
), data = d)

m6.4 <- quap(alist(
  K ~ dnorm(mu, sigma),
  mu <- a + bL * L,
  a ~ dnorm(0, 0.2),
  bL ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
), data = d)

precis(m6.3) #fat is positively correlated
precis(m6.4) #lactose is negatively correlated
#the posterior distributions are mirror images of each other,
#almost

m6.5 <- quap(alist(
  K ~ dnorm(mu, sigma),
  mu <- a + bL * L + bF * F,
  a ~ dnorm(0, 0.2),
  bL ~ dnorm(0, 0.5),
  bF ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
), data = d)

precis(m6.5) #when both variables are included
#then the coefficients become closer to zero
#this is a similar situation to the leg example
#The result is a set of coefficients whose sum like
#along a single axis, much like before.

pairs( ~ kcal.per.g + perc.fat + perc.lactose , data=d , col=rangi2 )
#The above shows how negatively correlated
#lactose is to perc fat

#Despite this shows that just dropping correlated variables
#may not be a good idea.

#In the milk example, the causal graph might look like this
# F -> K <- L
# F <- D -> L
#where D is some variable that determines how dense the milk is
#The causal mechanism is that primates that nurse frequently
#likely have watery milk high in lactose but low in calories
#and vice versa for those that nurse infrequently.

#This causal interpretation would be lost by just a simple
#variable selection.
#Some way of measuring D than just stumbling through 
#regression would be much preferable.

#Multi-collinearty is known as non-identifiability (of a parameter).


#Try simulating how imprecise the posterior becomes with
#the association between two variables.
#the function below generates correlated variables
#fits a model and returns the stdev
#of the posterior dist of of the slope
#relating perc fat to kcal

sim.coll <- function(r=.9){
  d$x <- rnorm(nrow(d), mean = r* d$perc.fat, sd = 
                 sqrt((1-r^2)*var(d$perc.fat)))
  m <- lm(kcal.per.g ~ perc.fat + x, data = d)
  sqrt(diag(vcov(m)))[2] #stdev of a parameter
}

rep.sim.coll <- function(r = 0.9, n = 100){
  stddev <- replicate(n, sim.coll(r))
  mean(stddev)
}

r.seq <- seq(from = 0, to = .99, by = 0.01)
stddev <- sapply(r.seq, function(z) rep.sim.coll(r = z,
                                                 n = 100))
plot(stddev ~ r.seq, type = 'l', col = rangi2, lwd = 2,
     xlab = 'correlation')


#So, for each r, the code generates 100 regressions
#and returns the average stdev from them.
#The model implicitly uses flat priors.
#With informative priors, the inflation in stdev
# can be much slower.

#####################
#Post-Treatment Bias#
#####################
#Omitted variables may cause omission bias.
#Included variable bias is also an issue.
#One such example is the post-treatment bias.

#Say that you want to consider the impact
#of treating plants with antifungal sprays
#We expect plants free from fungus to grow taller
#Should we control for whether plants have fungus 
#post treatment?
set.seed(71)
N <- 100
h0 <- rnorm(N,10,2)

#simulate initial heights
treatment <- rep(0:1, each = N/2)
fungus <- rbinom(N, size = 1, prob = 0.5 - treatment * 0.4)

#simulate final heights
h1 <- h0 + rnorm(N, 5 - 3* fungus)

d <- data.frame(h0 = h0, h1 = h1, treatment = treatment,
                fungus = fungus)
precis(d)

#When designing the model, pretend that the underlying
#data generation mechanism is unknown.

#So, try constructing a prior for initial height
# and p, which is the proportion between final and initial
#heights.

#it would make sense to let the initial height be normal
#the final height/initial height should be some non-negative #
#So, model it w/ lognormal.
#And while plants shrinking seems very unlikely,
#let's just assume ignorance.

sim_p <- rlnorm(1e4, 0, 0.25)
precis(data.frame(sim_p))

m6.6 <- quap(alist(
  h1~dnorm(mu, sigma),
  mu <-  h0 * p,
  p~ dlnorm(0, 0.25),
  sigma ~ dexp(1)
),data = d)

precis(m6.6, prob =.95)
#So, roughly a 40% growth

#Let's now model p as a linear model 
#in terms of both fungus and treatment
m6.7 <- quap(alist(
  h1~dnorm(mu, sigma),
  mu <- h0 * p,
  p <- a + bt * treatment + bf * fungus,
  a ~ dlnorm(0, 0.2),
  bt ~ dnorm(0, 0.5),
  bf ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
), data = d)
precis(m6.7)
#Treatment has a zero impact

#But the data was generated to make the treatment
#positively impact height.
#So, what's happening here?

#The problem is that fungal growth is a consequence of
#the treatment. By controlling for it,
#we're effectively asking, does the soil treatment matter
#once we know whether a plant developed fungus?
#Becaues the treatment prevents the growth of fungus,
#the fact that fungus is already present
#does not mean much.

m6.8 <- quap(alist(
  h1~dnorm(mu, sigma),
  mu <- h0 * p,
  p <- a + bt * treatment,
  a ~ dlnorm(0, 0.2),
  bt ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
), data = d)
precis(m6.8)
#Now the treatment impact is positive as it should be.

#The dag of this problem below

library(dagitty)

plant_dag <- dagitty("dag { H_0 -> H_1 
                     F -> H_1
                     T -> F}")
coordinates(plant_dag ) <- list(
  x=c(H_0 = 0, T = 2, F = 1.5, H_1 = 1),
  y = c(H_0 = 0, T = 0, F = 0, H_1 = 0)
)

drawdag(plant_dag)

#Controlling for F literally masks T which becomes d-separated.
#from H1.

impliedConditionalIndependencies(plant_dag)
#Such above post-treatment variables may be difficult to 
#tell in observational studies.

#Another scenario
plant_dag2 <- dagitty("dag { H_0 -> H_1 
                     M -> F
                     M -> H_1
                     T -> F}")
coordinates(plant_dag2) <- list(
  x=c(H_0 = 0, T = 2, F = 1.5, H_1 = 0.5, M = 1),
  y = c(H_0 = 0, T = 0, F = 0, H_1 = 0, M = 0.1)
)

drawdag(plant_dag2)

#M is a common cause, moisture, impacting both F and H1

impliedConditionalIndependencies(plant_dag2)

set.seed(71)
N <- 1000
h0 <- rnorm(N, 10, 2)
treatment <- rep(0:1, each = N/2)
M <- rbern(N)
fungus <- rbinom(N, size=1, prob=0.5 - treatment * 0.4
                 + 0.4 * M)
h1 <- h0 + rnorm(N, 5 + 3*M)

d2 <- data.frame(h0 = h0, h1 = h1, treatment = treatment,
                 fungus = fungus)

m6.7 <- quap(alist(
  h1~dnorm(mu, sigma),
  mu <- h0 * p,
  p <- a + bt * treatment + bf * fungus,
  a ~ dlnorm(0, 0.2),
  bt ~ dnorm(0, 0.5),
  bf ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
), data = d2)
precis(m6.7)

#The treatment effect seems positive, because
#conditioning on F activates the path through the collider
#to H1.

#What's interesting is that running model selection
#algos with almost always select fungus, because 
#it seems to provide better prediction.
#This shows causal inference and prediction are
#two different but not unnecessarily unrelated beasts.

#The age and happiness correlation.

ah_dag <- dagitty('dag{
  A -> M
  H -> M
}')
coordinates(ah_dag) <- list(x= c(A = 0, M = 1, H = 2),
                            y = c(A = 0, M = 0, H = 0))
drawdag(ah_dag)

d <- sim_happiness(seed=1977,N_years = 1000)
precis(d)

d2 <- d[d$age>17,]
d2$A <- (d2$age-18)/(65-18) #0 to 1 normalization

d2$mid <- d2$married+1

m6.9 <- quap(alist(
  happiness~dnorm(mu, sigma),
  mu <- a[mid] + bA * A,
  a[mid] ~ dnorm(0, 1),
  bA ~ dnorm(0,2),
  sigma ~ dexp(1)
), data = d2)

precis(m6.9) #age is negatively associated w/ happiness
#controlling for marriage

m6.10 <- quap(alist(
  happiness ~ dnorm(mu, sigma),
  mu <- a + bA * A,
  a ~ dnorm(0,1),
  bA ~ dnorm(0,2),
  sigma ~ dexp(1)
), data = d2)

precis(m6.10) #Age has no effect not controlled for marriage

#Another example of grandparents influencing
#educational achievements of children
#with some unmeasured factors influence both parents and
#children (e.g. location)
gc_dag <- dagitty('dag{
  G -> P
  P -> C
  G -> C
  U -> P
  U -> C
}')
coordinates(gc_dag) <- list(
  x= c(G = 0, P = 1, C = 1, U = 2),
  y = c(G = 0, P = 0, C = 2, U = 1)
)
drawdag(gc_dag)

N <- 200
b_GP <- 1 #direct effect ofG on P
b_GC <- 0
b_PC <- 1
b_U <- 2

set.seed(1)
U <- 2 * rbern(N, 0.5) - 1
G <- rnorm(N)
P <- rnorm(N, b_GP * G + b_U * U)
C <- rnorm(N, b_GC * G + b_U * U + b_PC * P)
d <- data.frame(U = U, G = G, P = P, C= C)

m6.11 <- quap(alist(
  C ~ dnorm(mu, sigma),
  mu <- a + b_PC * P + b_GC * G,
  a ~ dnorm(0, 1),
  c(b_PC, b_GC) ~ dnorm(0,1),
  sigma ~dexp(1)
), data = d)

precis(m6.11)
#The effect of b_GC is estimated to be negative 
#while it should be zero.
#And the effect of b_PC is exaggerated.

#This is a Simpson's paradox
#The overall effect of G on C should be zero.
#But by learning P, it suddenly becomes negative.
#Another famous example is the Berkeley admissions
#gender gap.
#Admission rates, A depend on
#department, D, as a simplification.
#Some departments are just more in demand by students.
#What kind of departments are chosen are dependent on S, or sex.
# S->D->A and S -> A
#To measure the direct effect of S on A,
#one has to control for department.
#The total effect seems to negatively affect females,
#but controlling for department shows otherwise.

#You can get backdoor adjustment sets from the following
dag_6.1 <- dagitty( "dag {
  U [unobserved]
  X -> Y
  X <- U <- A -> C -> Y
  U -> B <- C
}")
coordinates(dag_6.1) <- list(
  x = c(X = 0, U = 0, Y = 1, B = 0.5, A = 0.5, C = 1),
  y = -c(X = 0, Y = 0, B = 1, C = 2, U = 2, A = 3)
)
drawdag(dag_6.1)
adjustmentSets( dag_6.1 , exposure="X" , outcome="Y" )

#It's better to adjust on C than on A for better precision.
#This is because A does not directly affect Y.

#Let's return to the Waffle Divorce Example for
#more on backdoors
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
#Assumes that 'southerness' lowers the median age of marriage
#as well as marriage rate.
#it also impacts the presence of waffle houses.
#The dag is rather simple and does not
#contain any unobserved confounders
#but it can still be useful in telling us
#whether the d-sep assumptions are right
#and if not, the data will show us.
#As with theories, a DAG can be proven to be false
#but it's difficult to prove that it's true.

impliedConditionalIndependencies(dag_6.2)

#Dags are useful but not complete.
#If you have a complex, dynamic systems model, you 
#usually do not need a dag.
#It would be much more effective in representing 
#the impact of initial conditions.

#In fact, domain-specific structural causal models
#can possibly answer causal questions when dags cannot.

#Side note on confounding
#More formally, confounding occurs when P(Y|X) !=
#P(Y|do(X))

#We might say that X causes Y if P(Y|do(X)) != P(Y|do(!X))
#On the other hand, P(Y|X) != P(Y|!X) does not close
#the backdoor.

