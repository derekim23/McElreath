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