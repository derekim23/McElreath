library(rethinking)
data(rugged)
d = rugged
d$log_gdp = log(d$rgdppc_2000)
dd = d[complete.cases(d$rgdppc_2000),]

dd$log_gdp_std = dd$log_gdp/mean(dd$log_gdp)
dd$rugged_std = dd$rugged/mean(dd$rugged)
# Why scale ruggedness this way? Because zero ruggedness
#is meaningful.

m8.1 = quap(alist(
  log_gdp_std <- dnorm(mu, sigma),
  mu <- a + b*(rugged_std-0.215), #mean of rugged_std
  a~dnorm(1,0.1), #a should be 1 when rugged_std is average
  b~dnorm(0,0.3), #no positive or negative bias
  sigma ~dexp(1)
), data = dd)

set.seed(7)
prior <- extract.prior(m8.1)
plot(NULL, xlim=c(0,1), ylim = c(0.5, 1.5), xlab = 'ruggedness_std',
     ylab = 'log GDP')
abline(h=min(dd$log_gdp_std), lty = 2)
abline(h=max(dd$log_gdp_std), lty = 2)

rugged_seq <- seq(0,1,length.out = 30)
mu <- link(m8.1,post=prior, data = data.frame(rugged_std = 
                                                rugged_seq))
for(i in 1:50){
  lines(rugged_seq, mu[i,], col = col.alpha('black', 0.3))
}
#Idea: set up the priors so that log GDP is roughly 1 
#when ruggedness_std is 0.
#And the slopes probably should not be too large
#as ruggedness is only a small part of what impacts GDP.
#The abs value of slope being 0.6 is roughly 2 stdevs out.

precis( m8.1 )
dd$cid <- ifelse(dd$cont_africa==1,1,2)
m8.2 <- quap(alist(
  log_gdp_std ~ dnorm(mu,sigma),
  mu <- a[cid] + b * rugged_std,
  a[cid] ~ dnorm(1,0.1),
  b ~ dnorm(0, 0.3),
  sigma ~ dexp(1)
), data = dd)

compare(m8.1, m8.2)
precis(m8.2,depth=2)
post <- extract.samples(m8.2)
diff_a1_a2 <- post$a[,1] - post$a[,2]
PI(diff_a1_a2)

rugged_seq <- seq(0,1,length.out=30)
mu_not_africa <- link(m8.2, data = data.frame(rugged_std = 
                                                rugged_seq,
                                              cid = 2))
mu_africa <- link(m8.2, data = data.frame(rugged_std = 
                                                rugged_seq,
                                              cid = 1))

mu_not_africa_mu <- apply(mu_not_africa,2, mean)
mu_africa_mu <- apply(mu_africa,2, mean)

mu_not_africa_HPDI <- apply(mu_not_africa,2, HPDI, pro = .95)
mu_africa_HPDI <- apply(mu_africa,2, HPDI, prob = .95)

#the regression slope is still very close to zero
#meaning that the model improvement comes from 
#having another intercept for africa.

#So, let's add an interaction between
#continent and ruggedness slope as well
#note that the convention is to 
#have some fixed value and then add
#another value for africa, for example.
#But this again runs into the issue
#of difficulty in calibrating priors
#So, let's instead just use index variables.

m8.3 <- quap(alist(
  log_gdp_std~dnorm(mu, sigma),
  mu <- a[cid] + b[cid]*(rugged_std - 0.215),
  a[cid] ~ dnorm(1,0.1),
  b[cid] ~ dnorm(0,0.3),
  sigma ~ dexp(1)
), data = dd)

precis(m8.3, depth = 2)
#stronger, more positive association for cid = 1.

compare(m8.1,m8.2,m8.3,func=PSIS)
#model 8.3 has more than 95% of the weight.

plot(PSIS(m8.3, pointwise = T)$k)

d.A1 <- dd[dd$cid == 1,]
plot(d.A1$rugged_std,d.A1$log_gdp_std,pch=16, col = rangi2,
     xlab = 'ruggedness (standardized)',
     ylab = 'log gdp (as a prop of the mean)',
     xlim = c(0,1))
mu <- link(m8.3, data = data.frame(rugged_std =
                                     rugged_seq, cid =1))
mu_mean <- apply(mu,2,mean)
mu_HPDI <- apply(mu,2,HPDI,prob = .95)
lines(rugged_seq, mu_mean, lwd = 2)
shade(mu_HPDI, rugged_seq)
mtext('African Nations')

d.A2 <- dd[dd$cid == 2,]
plot(d.A2$rugged_std,d.A2$log_gdp_std,pch=16, col = rangi2,
     xlab = 'ruggedness (standardized)',
     ylab = 'log gdp (as a prop of the mean)',
     xlim = c(0,1))
mu <- link(m8.3, data = data.frame(rugged_std =
                                     rugged_seq, cid =2))
mu_mean <- apply(mu,2,mean)
mu_HPDI <- apply(mu,2,HPDI,prob = .95)
lines(rugged_seq, mu_mean, lwd = 2)
shade(mu_HPDI, rugged_seq)
mtext('Non-African Nations')
