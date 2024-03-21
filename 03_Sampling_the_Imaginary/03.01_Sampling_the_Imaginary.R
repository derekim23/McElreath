p_grid <- seq( from=0 , to=1 , length.out=1000 )
prob_p <- rep( 1 , 1000 )
prob_data <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

#Posterior sampling
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = T)

plot(samples)

library(rethinking)
dens(samples)

#For posterior point estimates, consider using loss functions like MAE
#to find "optimal" points
loss <- function(d, p_grid, posterior){
  sum(abs(d - p_grid) * posterior)
}

loss(0.5, p_grid, posterior)

losses <- sapply(p_grid, function(d) sum(abs(d - p_grid) * posterior))

#The following two should be very close
p_grid[which.min(losses)]
median(samples)

#MSE will lead to posterior mean

#posterior predictive sampling
pred_sample <- rbinom(1e4, size = 9, prob = samples)

#Since the sampled values appear in proportion to the posterior probabilities,
#you're effectively averaging.

pred_sample <-rbinom(1e4, size = 9, prob = p_grid)

plot(p_grid, pred_sample)
