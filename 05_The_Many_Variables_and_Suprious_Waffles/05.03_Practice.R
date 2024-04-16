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
#Buy is this true in the Bayesian sense?

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

post <- extract.samples(m5h2)
cor(post$bM , post$bA)
link(m5h2, data = )
