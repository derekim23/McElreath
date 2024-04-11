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


#Consider the following casual diagram
#         u
#       /   \
# X -> Z ->  Y 
#
#u should have arrows going into Z and Y

f <- function(n=100, bXZ=1, bZY = 1){
  u = rnorm(n)
  X = rnorm(n)
  Z = rnorm(n, bXZ * X + u)
  Y = rnorm(n, bZY * Z + u)
  bX = coef(lm(Y~X))['X']
  bXZ = coef(lm(Y~X+Z))['X']
  return (c(bX, bXZ))
}

sim <- mcreplicate(1e4, f(), mc.cores = 8)

dens(sim[1,], lwd = 3, xlab = "posterior mean", xlim= c(-1.1,2.1), ylim = c(0, 3))
dens(sim[2,], lwd = 3, col = 2,  add = T)


sim2 <- mcreplicate(1e4, f(bZY = 0), mc.cores = 16)

dens(sim2[1,], lwd = 3, xlab = 'posterior mean', xlim = c(-1.1,0.6))
dens(sim2[2,], lwd = 3, col =2, add = T)
#Wrong causal effect estimated by adding Z, a significant variable.

n = 100
bXZ = 1
bZY = 0
u = rnorm(n)
X = rnorm(n)
Z = rnorm(n, bXZ * X + u)
Y = rnorm(n, bZY * Z + u)
mod = lm(Y~X+Z)

summary(mod) #Z is indeed significant, because there's a fork path
#from Z to Y.



#x->y->z pipe: You DO NOT want to stratify by z!
f <- function(n = 100, bXY = 1, bYZ = 1){
  x = rnorm(n)
  y = rnorm(n, bXY * x)
  z = rnorm(n, bYZ * y)
  bX = coef(lm(y~x))['x']
  bXZ = coef(lm(y~x + z))['x']
  return(c(bX, bXZ))
}

sim <- mcreplicate(1e4, f(), mc.cores = 32)

dens(sim[1,], lwd = 3, xlab = 'posterior mean', xlim = c(0,1.5),
     ylim = c(0,6))
dens(sim[2,], lwd = 3, col =2, add = T)


#z->x->y  stratifying by z will cause you to lose precision
f <- function(n = 100, bZX = 1, bXY = 1){
  z <- rnorm(n)
  x <- rnorm(n, bZX * z)
  y <- rnorm(n, bXY * x)
  bX <- coef(lm(y~x))['x']
  bXZ <- coef(lm(y~x + z))['x']
  return(c(bX, bXZ))
}

sim <- mcreplicate(1e4, f(), mc.cores = 16)

dens(sim[1,], lwd = 3, xlab = 'posterior mean', xlim = c(0.5,1.5),
     ylim = c(0,6))
dens(sim[2,], lwd = 3, col =2, add = T)


#Bias amplification
#    u
#   | \
#z->x->y

f <- function(n = 100, bZX = 1, bXY = 1){
  u <- rnorm(n)
  z <- rnorm(n)
  x <- rnorm(n, bZX * z + u)
  y <- rnorm(n, bXY * x + u)
  bX <- coef(lm(y~x))['x']
  bXZ <- coef(lm(y~x+z))['x']
  c(bX, bXZ)
}

sim <- mcreplicate(1e4, f(bXY = 0), mc.cores = 16)
dens(sim[1,], lwd = 3, xlab = 'posterior mean', xlim = c(-0.5,1))
dens(sim[2,], lwd = 3, col = 2, add = T)

#the true effect is 0, but the effect is biased in y ~ x
#and this is made even worse in y ~ x + z
