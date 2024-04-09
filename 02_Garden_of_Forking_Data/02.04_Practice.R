#2M1
#Recall the globe tossing model from the chapter. 
#Compute and plot the grid approximate posterior distribution for each of the following sets of observations. 
#In each case, assume a uniform prior for p.

#1) W W W
len_grid = 100
p_grid <- seq(from = 0, to = 1, length.out = len_grid)
prior <- rep(1, len_grid)
likelihood <- dbinom(3, 3, p_grid)
unscaled_posterior = prior * likelihood
posterior = unscaled_posterior/sum(unscaled_posterior)

plot(p_grid, posterior, type = 'b')

#2) W W W L
likelihood <- dbinom(3, 4, p_grid)
unscaled_posterior = prior * likelihood
posterior = unscaled_posterior/sum(unscaled_posterior)

plot(p_grid, posterior, type = 'b')

#3) L W W L W W W
likelihood <- dbinom(5, 7, p_grid)
unscaled_posterior = prior * likelihood
posterior = unscaled_posterior/sum(unscaled_posterior)

plot(p_grid, posterior, type = 'b')


#2M2
#“Now assume a prior for p that is equal to zero when p < 0.5 and 
#is a positive constant when p ≥ 0.5. Again compute and plot the grid 
#approximate posterior distribution for each of the sets of observations 
#in the problem just above.”
prior <- ifelse(p_grid < 0.5, 0, 1)

#1) W W W
likelihood <- dbinom(3, 3, p_grid)
unscaled_posterior <-  likelihood * prior
scaled_posterior <- unscaled_posterior/sum(unscaled_posterior)

plot(p_grid, scaled_posterior, type = 'b')

#2) W W W L
likelihood <- dbinom(3, 4, p_grid)
unscaled_posterior <-  likelihood * prior
scaled_posterior <- unscaled_posterior/sum(unscaled_posterior)

plot(p_grid, scaled_posterior, type = 'b')

#3) L W W L W W W
likelihood <- dbinom(5, 7, p_grid)
unscaled_posterior <-  likelihood * prior
scaled_posterior <- unscaled_posterior/sum(unscaled_posterior)

plot(p_grid, scaled_posterior, type = 'b')


#2M3
#p(earth) = 0.5
#p(earth, land) = 0.5 * 0.3
#p(land) = p(land|earth) * p(earth) + p(land|mars) * p(mars) = 0.3 * 0.5 + 0.5

#p(earth|land) = p(earth, land) / p(land) = 0.5 * 0.3 / 0.5 / (0.3 + 1)
# = 0.3/1.3
0.3/1.3

#2M4
#Other writing exercises are too text heavy - just view them in the book.

sample(p_grid, posterior, size = 14, replace = T)


