#5M4
library(rethinking)
data("WaffleDivorce")

d <- WaffleDivorce

d$D <- d$Divorce
d$M <- standardize(d$Marriage)
d$L <- log(c(0.75, 4.53, 6.18, 1, 2.01, 2.82, 0.43, 0.55, 0.38,
                      0.75, 0.82, 5.18, 26.35, 0.44, 0.66, 0.87, 1.25, 0.77, 0.64, 0.81,
                      0.72, 0.39, 0.44, 0.58, 0.72, 1.14, 4.78, 1.29, 0.61, 0.37, 3.34,
                      0.41, 0.82, 1.48, 0.52, 1.2, 3.85, 0.4, 0.37, 0.83, 1.27, 0.75,
                      1.21, 67.97, 0.74, 1.13, 3.99, 0.92, 0.44, 11.5 )/100)
d$L <- standardize(d$L)
d$A <- standardize(d$MedianAgeMarriage)

p5m4 <- quap(alist(
  D~dnorm(mu, sigma),
  mu <- a + bM * M + bL * L + bA * A,
  a ~ dnorm(0, 1),
  bM ~ dnorm(0, 1),
  bL ~ dnorm(0, 1),
  bA ~ dnorm(0, 1),
  sigma ~dexp(1)
), data = d)

precis(p5m4)
#the LDS marker may be indicative of another that exhibits
#the cultural norms of a particular state, which may 
#also influence divorce rates in groups other than LDS.
#How would you control for this?

#Perhaps we can control for general religiousity.


#5M5
#obesity rate <- ? - gasoline price
#obseity rate <- caloric consumption per capita
#obesity rate <- caloric expenditure per capita
#gasoline price -> caloric consumption per capita
#gasoline price -> caloric expenditure per capita
#temperature -> caloric expenditure
#location -> caloric expenditure as well as consumption
#ethnic breakdown -> both
#age distribution -> both
#income distribution -> both
#anyhow,both which directly impact obesity rate
#depend on many confounding variables other than gas price
#that may also be backdoors into gas price
#would control for all such.

#5H1
#M would be independent of D conditioned on A.
mod1 <- lm(D ~ A,data = d)
summary(mod1)
mod2 <- lm(D ~ M,data = d)
summary(mod2)
mod3 <- lm(D ~ M + A,data = d)
summary(mod3)
#It does seem M is independent of D conditioned on A,
#at least based on the linear models.
#But is this true in the Bayesian sense?

m5h1 <- quap(alist(
  D~dnorm(mu, sigma),
  sigma~dexp(1),
  mu <- a + bM * M,
  a ~ dnorm(0,2),
  bM ~ dnorm(0,2)
), data = d)

precis(m5h1)
#Marriage has a positive impact

m5h2 <- quap(alist(
  D~dnorm(mu, sigma),
  sigma~dexp(1),
  mu <- a + bM * M + bA * A,
  a ~ dnorm(0,2),
  bM ~ dnorm(0,2),
  bA ~ dnorm(0,2)
), data = d)
precis(m5h2)
#M now has a negative effect but almost close to zero

#Wouldn't it be also convincing to check the model fit between
#the residuals after regressing D on A
#and M?

mod <- lm(D~A, data = d)
mod2 <- lm(mod$residuals~d$M)
summary(mod2)
#Confirmation.




#5H2
xseq <- seq(from = min(d$A) - 0.15, to = max(d$M) + 0.15, length.out = 30)
#Let's check CA
d$Marriage[d$Loc == 'CA']
#Say CA's rate is halved:
d$M2 <- d$Marriage
d$M2[d$Loc == 'CA'] = d$M2[d$Loc == 'CA']/2 
d$M2 <- standardize(d$M2)

mu <- link(m5h2, data = data.frame(A=xseq, M = d$M[d$Loc == 'CA']))
mu_c <- link(m5h2, data = data.frame(A=xseq, M = d$M2[d$Loc == 'CA']))
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu, 2, PI)
mu_c_mean <- apply(mu_c,2,mean)
mu_c_PI <- apply(mu_c, 2, PI)
plot(NULL, xlim = range(d$A), ylim=range(d$D))
lines(xseq, mu_mean, lwd=2)
lines(xseq, mu_c_mean, lwd=2, lty = 2, col = rangi2)
shade(mu_PI, xseq)
shade(mu_c_PI, xseq, col= col.alpha(rangi2,0.3))
#Interestingly, a lower marriage rate implies higher 
#divorce rates?

#Doesn't seem quite right, so I referred to a solution I found
#here: https://www.youtube.com/watch?v=s_k1UPJwVGo.
#The idea is to model M->A->D sequentially.

m5h2 <- quap(alist(
  A ~ dnorm(muA, sigmaA),
  muA <- aA + bMA * M,
  aA ~ dnorm(0, 0.2),
  bMA ~ dnorm(0, 0.5),
  sigmaA ~ dexp(1),
  
  D ~ dnorm(muD, sigmaD),
  muD <- aD + bAD * A,
  aD ~ dnorm(0, 0.2),
  bAD ~ dnorm(0, 0.5),
  sigmaD ~ dexp(1)
), data = d)

precis(m5h2)

#Since we expect marriage rates to 'positively' affect
#divorce rates, let's see if bMA * bAD is positive.
#And indeed, it is according to the above output.

mu_M <- mean(d$Marriage)
half_mu_M <- mu_M/2
half_mu_M_std <- (half_mu_M - mu_M)/sd(d$Marriage)

M_half <- c(0, half_mu_M_std) #We're contrasting between
#the mean marriage rate across all sates vs. half of that.

m5h2_sim_half <- sim(m5h2, data= data.frame(M=M_half), vars = c("A", "D"))
diff <- m5h2_sim_half$D[,2] - m5h2_sim_half$D[,1]
mu_diff <- mean(diff)


mu_diff * sd(d$Divorce) #so, roughly a -1.2% impact on avg
#by reducing the avg marriage rate by half.

M_seq <-seq(from = -3, to = 3, length.out = 100)
m5h2_sim <- sim(m5h2, data = data.frame(M = M_seq), 
                vars = c("A", "D"))

mu_D <- colMeans(m5h2_sim$D)

plot(mu_D ~ M_seq, type = 'l', main = "M->A->D Counterfactual",
     ylab = 'D', xlab = 'M')
HDPI_D <- apply(m5h2_sim$D,2,HPDI)
shade(HDPI_D, M_seq)
abline(h = mu_diff, col = rangi2)
abline(v = half_mu_M_std, col = rangi2)


#5H3