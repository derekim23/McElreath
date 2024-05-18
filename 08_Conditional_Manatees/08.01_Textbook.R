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

#In trying to interpret the interaction model,
#the following two interpretations would have the 
#same meaning to the golem while they do not for us.

#(1)  How much does the association between ruggedness and 
#log GDP depend upon whether the nation is in Africa?
  
#(2)  How much does the association of Africa with 
#log GDP depend upon ruggedness?


#first, plot the association of being in Africa
#with log GDP depending on ruggedness.

rugged_seq <- seq(-0.2, 1.2, length.out = 30)
muA <- link(m8.3, data = data.frame(cid=1, rugged_std = rugged_seq))
muN <- link(m8.3, data = data.frame(cid=2, rugged_std = rugged_seq))

delta <- muA - muN

mu_delta <- apply(delta,2,mean)
HPDI_delta <- apply(delta,2,HPDI, .95)

plot(rugged_seq, mu_delta, type = 'l')
shade(HPDI_delta, rugged_seq)
#At low ruggedness, going to Africa hurts log gdp 
#At high ruggedness, the reverse should be true

#Not sure why the chart shows as above, but the general idea 
#is that the contrast is negative for lower ruggedness
#and is positive for higher ruggedness in favor of Africa.

data(tulips)
d <- tulips
str(d)

d$blooms_std <- d$blooms/max(d$blooms)
d$water_cent <- d$water - mean(d$water)
d$shade_cent <- d$shade - mean(d$shade)
#The prior for blooms will be centered at 0.5.
#this means that, when water and shade amount are average,
#we expect blooms to be at 0.5.
#also, a has to be between 0 and 1.
#it would make sense to limit the sd
#so taht the normal prior does not
#cause too much of the probability mass to lie
#outside of the 0 and 1 bounds.
#For example, by using 0.25 with a mean of 0.5,
#only 5% lie outside of the bounds.
#because water and shade can range from -1 to 1,
#it makes sense to bound the slope so that 
#we do not exceed the max bloom value of 1.
#Since an entire range of 2 * the slope should not exceed 1,
# we should consider a slope of 0.5 to be a reasonable limit
#So, using n(0, 0.25) seems to make sense.

range(d$shade_cent)
range(d$water_cent)

m8.4 <- quap(
alist(blooms_std ~ dnorm(mu, sigma),
      mu <- a + bw * water_cent + bs * shade_cent,
      a ~ dnorm(0.5, 0.25),
      bw ~ dnorm(0, 0.25),
      bs ~ dnorm(0, 0.25),
      sigma ~ dexp(1)),
data = d) 

#Now, intuitively thinking, the impact of bw
#is likely limited by how much shade there is
#and the same for bs.
#how can we factor in this interaction?

#One possibility is to let the coefficient bw
#becaome a linear function of s:
#bw_old = bw + b_ws * s_i

#bs - we would like for this to depend on water too,
#but by symmetry, we get this for free.

#how do we even set up a prior for bws?

#we can imagine that the impact of water should be zero
#if the impact of shade is maximized.
#so, bw_old = 0 = bw + b_ws * 1

#So, perhaps it makes sense to set b_ws ~ n(-mean(bw), sd(bw))
#since mean(bw) is zero, we still get zero.

m8.5 <- quap(
  alist(blooms_std ~ dnorm(mu, sigma),
        mu <- a + bw * water_cent + bs * shade_cent + 
          bws * water_cent * shade_cent,
        a ~ dnorm(0.5, 0.25),
        bw ~ dnorm(0, 0.25),
        bs ~ dnorm(0, 0.25),
        bws ~ dnorm(0, 0.25),
        sigma ~ dexp(1)),
  data = d) 

#Without interaction
par(mfrow=c(1,3))
for (s in -1:1){
  idx <- which(d$shade_cent == s)
  plot(d$water_cent[idx], d$blooms_std[idx], xlim = c(-1,1),
       ylim = c(0,1), xlab = 'water', ylab = 'blooms',
       pch = 16, col = rangi2)
  mu <- link(m8.4, data = data.frame(shade_cent = s,
                                     water_cent = -1:1))
  for (i in 1:20) lines(-1:1, mu[i,], col = col.alpha('black',
                                                      .3))
}

#With interaction
for (s in -1:1){
  idx <- which(d$shade_cent == s)
  plot(d$water_cent[idx], d$blooms_std[idx], xlim = c(-1,1),
       ylim = c(0,1), xlab = 'water', ylab = 'blooms',
       pch = 16, col = rangi2)
  mu <- link(m8.5, data = data.frame(shade_cent = s,
                                     water_cent = -1:1))
  for (i in 1:20) lines(-1:1, mu[i,], col = col.alpha('black',
                                                      .3))
}

set.seed(7)
prior <- extract.prior(m8.5)
for (s in -1:1){
  idx <- which(d$shade_cent == s)
  plot(d$water_cent[idx], d$blooms_std[idx], xlim = c(-1,1),
       ylim = c(0,1), xlab = 'water', ylab = 'blooms',
       pch = 16, col = rangi2)
  mu <- link(m8.5, post = prior, data = data.frame(shade_cent = s,
                                                   water_cent = -1:1))  
  for (i in 1:20) lines(-1:1, mu[i,], col = col.alpha('grey', .3))
}


