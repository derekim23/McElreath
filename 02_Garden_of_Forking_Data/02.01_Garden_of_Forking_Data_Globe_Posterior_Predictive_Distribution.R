sim_globe <- function(p = 0.7, N = 9){
  sample(c('W', 'L'), size = N, prob = c(p, 1-p), replace = TRUE)
}

post_samples <- rbeta(10000, 7, 4)
#For each of the posterior probability, simulate 10 samples 
#and see the proportion of the generative samples that happen to be water
pred_post <- sapply(post_samples, function(p) sum(sim_globe(p, 10) == 'W'))
tab_post <- table(pred_post)
tab_post
plot(-10, xlim = c(0,10), ylim = c(0, 2250))
for(i in 0:10) lines(c(i,i), c(0,tab_post[i+1]), lwd = 4, col = 4)

#grid approximation of the posterior for the globe tossing problem

# define grid
(p_grid <- seq(from = 0, to = 1, length.out = 100))
# define prior
(prior <- rep( 1 , length(p_grid)))
(prior <- ifelse( p_grid < 0.5 , 0 , 1 ))

prior <- exp( -5*abs( p_grid - 0.5 ) )
plot(prior)

# compute likelihood at each value in grid
(likelihood <- dbinom( 6 , size=9 , prob=p_grid ))
  #Above, you have six tosses of water in a binomail trial size of 9
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
(posterior <- unstd.posterior / sum(unstd.posterior))
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "100 points" )


#quadratic approximation of the posterior
library(rethinking)
globe.qa <- quap(
  alist(
    W ~ dbinom( W+L ,p) , # binomial likelihood
    p ~ dunif(0,1) # uniform prior
  ) ,
  data=list(W=6,L=3) )
# display summary of quadratic approximation
precis( globe.qa )
globe.qa


# analytical calculation
W <- 6
L <- 3
curve( dbeta( x , W+1 , L+1 ) , from=0 , to=1 )
# quadratic approximation
curve( dnorm( x , 0.67 , 0.16 ) , lty=2 , add=TRUE )
