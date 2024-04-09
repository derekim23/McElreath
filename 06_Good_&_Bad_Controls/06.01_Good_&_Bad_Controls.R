#Simulate confounded Y

N <- 200
b_XY <- 0
b_UY <- -1
b_UZ <- -1
b_ZX <- 1

set.seed(10)
U <- rbern(N)
Z <- rnorm(N, b_UZ * U)
X <- rnorm(N, b_ZX * Z)
Y <- rnorm(N, b_UY * U + b_XY * X)
d <- list(Y=Y, X=X, Z= Z)
#U is the common cause
#Want to see how X affects Y
#There's a backdoor gate, Z, that goes from U in to X.

#Ignore U,Z
m_YX <- quap(
  alist(
    Y ~ dnorm(mu, sigma),
    mu <- a + b_XY * X,
    a ~ dnorm(0, 1),
    b_XY ~ dnorm(0,1),
    sigma ~ dexp(1)
  ), data = d
)

#Stratify by Z
m_YXZ <- quap(
  alist(
    Y ~ dnorm(mu, sigma),
    mu <- a + b_XY * X + b_Z * Z,
    a ~ dnorm(0,1),
    c(b_XY, b_Z) ~ dnorm(0,1), #What does this mean? it's a shorthand for each var
    sigma ~ dexp(1)
  ), data = d
)

post <- extract.samples(m_YX)
post2 <- extract.samples(m_YXZ)

dens(post$b_XY,lwd=3,col = 1, xlab = 'posterior b_XY', xlim = c(-0.3, .3))
dens(post2$b_XY, lwd = 3, col = 2, add = T)

#We know that the true effect b_XY = 0
#And that gets only captured when we control for Z
#i.e. stratify by iy.