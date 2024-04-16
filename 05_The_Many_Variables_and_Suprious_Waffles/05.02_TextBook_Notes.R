library(rethinking)
data("WaffleDivorce")

#As per the text book, try a simpler model
d <- WaffleDivorce
d$D <- standardize(d$Divorce)
d$M <- standardize(d$Marriage)
d$A <- standardize(d$MedianAgeMarriage)

m5.1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

#Try looking at the prior predictive
prior <- extract.prior(m5.1)
mu <- link(m5.1, post = prior, data = list(A = c(-2,2)))

plot(NULL, xlim=c(-1,1), ylim = c(-2,2), 
     ylab = 'divorce rate (std)',
     xlab = 'mediange age of marriage (std)')
for(i in 1:50){
  lines(c(-2,2), mu[i,], col = col.alpha('black', 0.4))
}

#Posterior predictive
A_seq <- seq(from = -3, to = 3.2, length.out = 30)
mu <- link(m5.1, data = list(A = A_seq))
#Fit the link function, the equation mu = a + bA * A over A_seq
#within each column.
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(D~A, data = d, col = rangi2)
lines(A_seq, mu.mean, lwd = 2)
shade(mu.PI, A_seq)
precis(m5.1)

#Try relating marraige rates to divorce rates
m5.2 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM * M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

M_seq <- seq(from = -3, to = 3.2, length.out = 30)
mu <- link(m5.2, data = list(M = M_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu,2,PI)

plot(D~M, data = d)
lines(M_seq, mu.mean, lwd = 2)
shade(mu.PI, M_seq)

precis(m5.2)
#Note that, from a causality standpoint, the association
#between M and D may arise only from A indirectly influencing
#D through M.

library(dagitty)
dag5.1 <- dagitty('dag{A->D; A->M; M->D}')
coordinates(dag5.1) <- list(x=c(A=0, D =1 , M=2), 
                            y=c(A=0,D=1,M=0))
drawdag(dag5.1)

DMA_dag2 <- dagitty('dag{D<-A->M}')
impliedConditionalIndependencies(DMA_dag2)

DMA_dag1 <- dagitty('dag{D<-A->M->D}')
impliedConditionalIndependencies(DMA_dag1)

#Now with both variables
m5.3 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M + bA*A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )
precis( m5.3 )

plot( coeftab(m5.1,m5.2,m5.3), par=c('bA','bM') )

#Notice how the coefficient for M goes to zero when
#you use both variables.

'
Once we know median age at marriage for a State, 
there is little or no additional predictive power in also 
knowing the rate of marriage in that State.
'
'
Note that this does not mean that there is no value in knowing 
marriage rate. Consistent with the earlier DAG, if you didn’t 
have access to age-at-marriage data, then you’d definitely 
find value in knowing the marriage rate. M is predictive but 
not causal. Assuming there are no other causal variables 
missing from the model (more on that in the next chapter), 
this implies there is no important direct causal path from 
marriage rate to divorce rate. The association between marriage 
rate and divorce rate is spurious, caused by the influence of 
age of marriage on both marriage rate and divorce rate.
'

#Interpreting parameter estimates will always depending on
#what you expect the causal model to be.

#Try plotting some model diagnostic on the following example
m5.4 <- quap(
  alist(
    M ~ dnorm( mu , sigma ) ,
    mu <- a + bAM * A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bAM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

#residuals
mu <- link(m5.4)
mu_mean <- apply( mu , 2 , mean )
mu_resid <- d$M - mu_mean

'
when you do multiple regression, realize that the coefficient 
essentially measures the association between the residual of 
the independent variable after it has been regressed against 
other independent variables and the target variable; 
in other words, the residuals capture the variance that are 
not explained by any of the other predictor variables, and by 
eing how those relate to the target variable, you can capture 
the "direct" causal effect, if any.
a) However, if the independent variables are related to each 
other in a non-linear way, then the residuals cease to be useful.
'

#Posterior Predictive
mu <- link(m5.3)
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu,2,PI)
D_sim <- sim(m5.3, n=1e4)
D_PI <- apply(D_sim,2, PI)

plot(mu_mean ~ d$D, col = rangi2,
     ylim = range(mu_PI),
     xlab = 'Observed divorce',
     ylab = 'Predicted divorce')
     
abline(a = 0, b= 1, lty = 2)
for (i in 1:nrow(d)) lines(rep(d$D[i],2), mu_PI[,i], col = rangi2)
#Over predicts for lower divorce rates
#under predicts otherwise

identify(x=d$D, y= mu_mean, labels = d$Loc)
#Above allows you to label points on the plot

#You'll see that Utah and Idaho have low divorce rates
#driven by a relatively high population of Christians of 
#the Church of Jesus-Christ of Latter-day Saints.


#Another useful type of diagnostic plot other than the 
#residual plot is the counterfactual plot.

#“So let’s see how to generate plots of model predictions 
#that take the causal structure into account. The basic recipe 
#is:
  
#(1)  Pick a variable to manipulate, the intervention variable.
#(2)  Define the range of values to set the intervention 
#variable to.
#(3)  For each value of the intervention variable, and for each 
#sample in posterior, use the causal model to simulate the 
#values of other variables, including the outcome.

#In the end, you end up with a posterior distribution of 
#counterfactual outcomes that you can plot”

#Let's revisit the WaffleHouse example wherein we apply
#structural causal functions to simulate counterfactulas

m5.3_A <- quap(alist(
  D~dnorm(mu, sigma),
  mu <- a + bM * M + bA * A,
  a ~ dnorm(0, 0.2),
  bM ~ dnorm(0, 0.5),
  bA ~ dnorm(0, 0.5),
  sigma ~ dexp(1),
  M ~ dnorm(mu_M, sigma_M),
  mu_M <- aM + bAM * A,
  aM ~ dnorm(0, 0.2),
  bAM ~ dnorm(0, 0.5),
  sigma_M ~ dexp(1)),
  data = d
)

precis(m5.3_A)
#A and M are negatively correlated.

#We want to simulate what would happen for different A vals.
A_seq = seq(from = -2, to = 2, length.out = 30)

sim_dat <- data.frame(A=A_seq)

s <- sim(m5.3_A, data = sim_dat, vars = c('M','D'))

plot(sim_dat$A, colMeans(s$D), ylim=c(-2,2), type = 'l',
     xlab = 'manipulated A', ylab = 'counterfactual D'
     )

shade(apply(s$D, 2, PI), sim_dat$A)
mtext('Total Counterfactual Effect of A on D')


plot(sim_dat$A, colMeans(s$M), ylim=c(-2,2), type = 'l',
     xlab = 'manipulated A', ylab = 'counterfactual M'
)

shade(apply(s$M, 2, PI), sim_dat$A)
mtext('Total Counterfactual Effect of A on M')

#Now, try predicting the expected causal effect of
#increasing the median age from 20 to 30;
#Data below standardized to mean 26.1 and stdev = 1.24
sim2_dat <- data.frame(A=(c(20,30)-26.1)/1.24)
s2 <- sim(m5.3_A, data = sim2_dat, vars = c('M', 'D'))
mean(s2$D[,2] - s2$D[,1])

#Recall that controlling for a variable effectively deletes
#the dag arrow from that variable to other nodes.
#Let's try simulating an average state with A = 0
#in which we try to change M.

sim_dat <- data.frame(M=seq(from=-2, to = 2, length.out = 30),
                      A = 0)
s <- sim(m5.3_A, data = sim_dat, vars = 'D')
plot(sim_dat$M, colMeans(s), ylim = c(-2,2), type = 'l',
     xlab = 'manipulated M', ylab = 'counterfactual D')
shade(apply(s,2,PI), sim_dat$M)
mtext('Total coutnerfactual effect of M on D')

#We've effectively eliminated the impact of A on D or M.
#And we now see that the effect of M on D is non-existent.

?sim

#This is how sim function works:
A_seq <- seq(from = -2, to = 2, length.out = 30)
post <- extract.samples(m5.3_A)
M_sim <- with(post, sapply(1:30, function(i)
  rnorm(1e3, aM + bAM * A_seq[i], sigma_M)))

#with post allows us to write out the function without
#having to type post$ before every parameter.
D_sim <- with(post, sapply(1:30, function(i)
  rnorm(1e3, a + bA*A_seq[i] + bM * M_sim[,i], sigma)))

#Masked relationships
data(milk)
d <- milk
str(d)
head(d)

#does milk nutritional content depend on brain mass
#as a percetange of the overall body mass?
d$K <- standardize(d$kcal.per.g)
d$N <- standardize(d$neocortex.perc)
d$M <- standardize(log(d$mass))

m5.5_draft <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bN*N ,
    a ~ dnorm( 0 , 1 ) ,
    bN ~ dnorm( 0 , 1 ) ,
    sigma ~ dexp( 1 )
  ),
  data  = d
)
#The above does not work becuase of NAs

dcc <- d[complete.cases(d$K, d$N, d$M),]

m5.5_draft <- quap(alist(
  K~dnorm(mu, sigma),
  mu <- a + bN * N,
  bN ~ dnorm(0, 1),
  a ~ dnorm(0,1),
  sigma ~ dexp(1)
), data = dcc 
)

#Are the priors reasonable?
prior <- extract.prior(m5.5_draft)
xseq <- c(-2,2)
mu <- link(m5.5_draft, post = prior, data = list(N=xseq))
plot(NULL, xlim = xseq, ylim = xseq,
     xlab = 'neocortex perc standardized',
     ylab = 'kcal/g standardized')
for (i in 1:50){
  lines(xseq, mu[i,], col = col.alpha('black', 0.3))
}
#the priors are too broad.
#in particular, a should be near zero, as
#we expect the outcome to be near zero when
#the input is zero.

m5.5 <- quap(alist(
  K~dnorm(mu, sigma),
  mu <- a + bN * N,
  bN ~ dnorm(0, 0.5),
  a ~ dnorm(0,0.2),
  sigma ~ dexp(1)
), data = dcc 
)

prior <- extract.prior(m5.5)
xseq <- c(-2,2)
mu <- link(m5.5, post = prior, data = list(N=xseq))
plot(NULL, xlim = xseq, ylim = xseq,
     xlab = 'neocortex perc standardized',
     ylab = 'kcal/g standardized')
for (i in 1:50){
  lines(xseq, mu[i,], lwd = 0.3)
}
#Narrower prior predictive band above

#posterior
xseq <- seq( from=min(dcc$N)-0.15 , to=max(dcc$N)+0.15 , length.out=30 )
mu <- link( m5.5 , data=list(N=xseq) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( K ~ N , data=dcc )
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )
#Given the data above, any mildly negative slope might just as
#well be plausible.

#Now w/ body mass
m5.6 <- quap(alist(
  K~dnorm(mu, sigma),
  mu <- a + bM * M,
  a~dnorm(0, 0.2),
  bM~dnorm(0, 0.5),
  sigma~dexp(1)),
  data = dcc
)

precis(m5.6)
#The association above seems stronger than 
#that of the neocortex mass %, but it is still quite uncertain
#And the credibility interval still includes zero

#bivariate model
m5.7 <- quap(alist(
  K~ dnorm(mu, sigma),
  mu <- a + bM * M + bN * N,
  a ~ dnorm(0, 0.2),
  bN~ dnorm(0, 0.5),
  bM~dnorm(0, 0.5),
  sigma ~ dexp(1)
), data = dcc)

precis(m5.7)
#with both variables, the association has become
#much stronger for both

plot(coeftab(m5.5, m5.6, m5.7), pars = c('bM', 'bN'))
pairs(~K + M + N, dcc)
# M & N are positively correlated

#The model is asking whether a higher neocortex percent for 
#some given body mass relates to higher milk energy.

#And also whether a higher body mass for a given neocortex 
#percent relates to lower milk energy.

#Some possible dags for this situation

#M->N
# \/ #pointing downward
# K

#M<-N
# \/ #pointing downward
# K

#M<-U->N
# \  / #pointing downward
#  K

#Where U is some unobserved confounder.

#In any of the above cases, either that not controlling for one 
#of the variables results in a backdoor
#or that one of the causal factors are not accounted for.

#Which of the above dags are right?
#It's not possible to tell, as all variables are conditionally
#dependent regardless of a control.

#Any set of dags that has the same conditional independence
#relationships is called a markov equivalence class.

#We can compute the equivalence set using dagitty pkg.

#Suppose that the third dag is correct.
#So, we'd want to control for both M and N to break the
#link to U.
#Such an experiment is impossible in reality
#because of how M and N are related through U.

#But, we can simulate a counterfactual plot.

xseq <- seq(from = min(dcc$M) - 0.15, to = max(dcc$M) + 0.15, length.out = 30)
mu <- link(m5.7, data = data.frame(M=xseq, N = 0))
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu, 2, PI)
plot(NULL, xlim = range(dcc$M), ylim=range(dcc$K))
lines(xseq, mu_mean, lwd=2)
shade(mu_PI, xseq)
#The counterfactual plot w/ other variable fixed at 0
#shows a clear negative relationship between
#milk energy and log body mass.

xseq2 <- seq(from = min(dcc$N) - 0.15, to = max(dcc$N) + 0.15, length.out = 30)
mu2 <- link(m5.7, data = data.frame(N=xseq, M = 0))
mu2_mean <- apply(mu2,2,mean)
mu2_PI <- apply(mu2, 2, PI)
plot(NULL, xlim = range(dcc$N), ylim=range(dcc$K))
lines(xseq2, mu2_mean, lwd=2)
shade(mu2_PI, xseq)
#The counterfactual plot w/ other variable fixed at 0
#shows a clear postive relationship between
#milk energy and brain cortex percentage.

#Overthinking
#Try to simulate data that matche the three dags
# M->K<-N
# M->N
n <- 100
M1 <- rnorm(n)
N1 <- rnorm(n, M1) #positive correlation between M and N
K1 <- rnorm(n, N1 - M1) #negative impact from body mass
dsim1 <- data.frame(K = K1, M = M1, N = N1)

# N->M
N2 <- rnorm(n)
M2 <- rnorm(n, N2)
K2 <- rnorm(n, N2 - M2)
dsim2 <- data.frame(K = K2, M = M2, N = N2)

# N <- U -> M
U <- rnorm(n)
N3 <- rnorm(n, U)
M3 <- rnorm(n, U)
K3 <- rnorm(n, N3 - M3)
dsim3 <- data.frame(K=K3, N = N3, M = M3)

#Why do larger animals seem to have larger brain cortex %?
#They tend to live long lives, and learning is a valuable
#asset that can be amortized over the lifetimes.

#If we believe this story,
#M->N seems the most plausible among the three.

#But we do not have a clear evidence of this.
#Let's create a markov equivalent set first.
dag5.7 <- dagitty("dag{M->K<-N
                  M->N}")
coordinates(dag5.7) <- list(x = c(M=0,K=1,N=2),
                            y = c(M=0.5,K=1,N=0.5))
MElist <- equivalentDAGs(dag5.7) #equivalent class

drawdag(MElist)
#We could probably eliminate dags here that have arrows going
#from K to the others
#some might argue chicken or egg, but
#let's say that physiology comes before milk change.

#######################
#Categorical Variables#
#######################

#The milk data includes both primates and new world monkey data.
#We may want to stratify the data based on this.

#But let's rewind a bit to the Howell1 data to see
#how categorical data can be used.

data("Howell1")
d <- Howell1
str(d)
#the male variable is shown as a categorical.
#consider a very simple regression model with 
#just male as the predictor.

#the issue with this is that, even when just looking at the
#prior, you will see that the height of males
#will have a greater variability
#just because the variability of the intercept
#and the predictor coefficient are being added up.
#This seems unreasonable.

#How else to approach this?
#Try index variables.

d$sex <- ifelse(d$male==1, 2, 1)
#In the above case, we end up with two intercepts, each
#corresponding to one index

#So, one natural question is what is the height diff
#between male and female
#we can quantify this by sampling form the posterior
m5.8 <- quap(alist(
  height~dnorm(mu, sigma),
  mu <- a[sex],
  a[sex]~dnorm(178,20),
  sigma~dunif(0,50)
),data = d)
precis(m5.8,depth=2)

post <- extract.samples(m5.8)
post$diff_fm <- post$a[,1] -post$a[,2]
precis(post, depth = 2)

#Such differences calculations are called contrasts.
#Now, we return to the milk data

data(milk)
d <- milk
levels(d$clade)
d$clade_id <- as.integer(d$clade)

#We only regress on clade

d$K <- standardize(d$kcal.per.g)
m5.9 <- quap(
  alist(
    K~ dnorm(mu, sigma),
    mu <- a[clade_id],
    a[clade_id]~dnorm(0,0.5),
    sigma~dexp(1)
  ), data = d
)
labels <- paste("a[", 1:4, "]:", levels(d$clade), sep = "")
plot(precis(m5.9, depth = 2, pars="a"), labels=labels,
     xlab = 'expected kcal (std)')

#Rethinking
#We have a tendency to look at credibility intervals
#and see whether they are far removed from 0 to check for
#significance.
#If you want to know the distribution of a differnce,
#you have to compute that difference.
#“It isn’t enough to just observe, for example, that 
#a slope among males overlaps a lot with zero while the 
#same slope among females is reliably above zero. ” 

#for example if one of the slopes were 0.15 with stdev of 0.02
#and the other were 0.02 with stdev of 0.1,
#the difference between the two will have, assuming independence,
#(0.15 - 0.02) = 0.13 with stdev of sqrt(0.02^2 + 0.1^2) = 0.1
#this distribution overflaps with zero... a lot.
#While one of the slopes is far from zero,
#you cannot say for sure say that the difference is far from 
#zero as well.

