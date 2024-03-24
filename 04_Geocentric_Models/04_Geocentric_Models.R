#Simulate weight from height
sim_weight <- function(H, b, sd){
  U <- rnorm(length(H), 0, sd)
  W <- b * H + U
  W
}

H <- runif(200, min = 130, max = 170)
W <- sim_weight(H, b = 0.5, sd = 5)

plot(W~H, col = 2, lwd=3)

data(Howell1)

d <- Howell1[Howell1$age>=18,]



library(rethinking)

H <- runif(10, 130, 170)
W <- sim_weight(H, b = 0.5, sd = 5)

m3.1 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + b * H,
    a ~ dnorm(0, 10),
    b ~ dunif(0, 1),
    sigma ~ dunif(0, 10)),
    data = list(W = W, H = H)
)

precis(m3.1)

dat <- list(W = d$weight, H = d$height)

m3.2 <-  quap(
    alist(
      W ~ dnorm(mu, sigma),
      mu ~ b * H + a,
      a ~ dnorm(0, 10),
      b ~ dunif(0, 1),
      sigma ~ dunif(0, 10)
    ), data = list(W = W, H = H)
)


precis(m3.2)

dnorm(0,10)
?dunif
runif(0, 1)

post <- extract.samples(m3.2)

plot(d$height, d$weight, col = 2, lwd = 3, xlab = 'Height (cm)', ylab = 'Weight (kg)')

for (j in 1:20){
  abline(a = post$a[j], b = post$b[j], lwd = 1)
}
