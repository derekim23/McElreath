p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

#3E
sum(samples > 0.8)/length(samples)

quantile(samples,0.2)

quntile(samples)

library(rethinking)
HPDI(samples, .66)


#3M1
p_grid <- seq(from=0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- prior * likelihood
posterior <- posterior/sum(posterior)
length(p_grid)
plot(p_grid, posterior, type = 'l')

#3M3
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
HPDI(samples, 0.9)
w <- rbinom(10000,15,prob = samples)

sum(w==8)/length(w)


hist(w)

#3M4
w <- rbinom(10000,9,prob=samples)
sum(w==6)/length(w)

#3M5
p_grid <- seq(from = 0, to = 1, length.out = 10000)
prior <- ifelse(p_grid < 0.5, 0, 0.8)
likelihood <- dbinom(8, 15, prob = p_grid)
unscaled_posterior <- prior * likelihood
posterior = unscaled_posterior/sum(unscaled_posterior)
plot(p_grid, posterior, type = 'l')

samples <- sample(p_grid, prob = posterior, size = 1e4, replace = 1)
HPDI(samples)

w <-rbinom(1e4, 15, prob = samples)
sum(w==8)/1e4

hist(w)


#3H1
data(homeworkch3)
birth1
birth2

sum(birth1) + sum(birth2)

length(birth1) + length(birth2)

p_grid <- seq(from = 0, to = 1, length.out = 10000)
prior <-  rep(1, length(p_grid))

likelihood <- dbinom(111, 200, prob = p_grid)

posterior = prior * likelihood
posterior = posterior / sum(posterior)

plot(p_grid, posterior)

p_grid[which.max(posterior)]

#3H2
samples <-  sample(p_grid, 1e4, prob = posterior, replace = 1)
HPDI(samples, .5)
HPDI(samples, .89)
HPDI(samples, .97)

#3H3
boys_sim <- rbinom(1e4, 200, samples)

hist(boys_sim)
abline(v=111)

#3H4
b1 <- sum(birth1)
likelihood <- dbinom(b1, 100, p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)

plot(p_grid, posterior, type = 'l')

samples <- sample(p_grid, size = 1e4, replace = T, prob = posterior)


boys_sim2 <- rbinom(1e4, 100, samples);
hist(boys_sim2)
abline(v=51)


#3H5
g1b2 <- birth2[birth1 == 0]
l = length(g1b2)
s = sum(g1b2)

w <- rbinom(1e4, l, samples)

hist(w)

abline(v=s) #biased sample