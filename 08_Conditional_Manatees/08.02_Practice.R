#8M3.In parts of North America, ravens depend upon wolves for 
#their food. This is because ravens are carnivorous but cannot 
#usually kill or open carcasses of prey. Wolves however can and 
#do kill and tear open animals, and they tolerate ravens 
#co-feeding at their kills. This species relationship is generally 
#described as a “species interaction.” Can you invent a 
#hypothetical set of data on raven population size in which this 
#relationship would manifest as a statistical interaction? 
#Do you think the biological interaction could be linear? 
#Why or why not?

prey <- rnorm(100, 1500, 10)
wolf <- rnorm(100, prey * 0.75, 10) 
  #0 prey ~> 0 wolf on average
raven <- rnorm(100, wolf * 2 * prey, 20)
  #0 prey ~ 0 raven on average
  #0 wolf ~ 0 raven on average
#The idea is that the slope explaining the relationship
#between wolves and raven depends linearly on the 
#count of the prey.
#After all, the larger the number of prey,
#the more likely it is that a given wolf may capture
#and eat more and allow more ravens to survive.

#While this sounds mechanistically plausible, I don't 
#think a pure linear relationship makes sense.

#We may end up with preposterous situations where
#having just one prey and a million wolves
#or the reverse would regardless result in predictions of 
#a very large number of ravens.

#The interaction would have to be more complicated
#so that it somehow gets "bottlenecked" by either of those 
#factors.

#perhaps the interaction term looking more like
#max(wolf, prey) might come close to that mechanistically,
#but then the model would be less interpretable.

plot(raven~prey)
plot(raven~wolf)
plot(raven~wolf*prey)

#“8M4.  Repeat the tulips analysis, but this time use priors 
#that constrain the effect of water to be positive and the 
#effect of shade to be negative. 
#Use prior predictive simulation. 
#What do these prior assumptions mean for the interaction prior, 
#if anything?”

m8.5 <- quap(
  alist(blooms_std ~ dnorm(mu, sigma),
        mu <- a + bw * water_cent + bs * shade_cent + 
          bws * water_cent * shade_cent,
        a ~ dnorm(0.5, 0.25),
        bw ~ dexp(1),
        bs ~ -dexp(1),
        bws ~ dnorm(0, 0.25),
        sigma ~ dexp(1)),
  data = d) 

#bws * w * s + bw * w = (bws * s + bw)w
#bws * w * s + bs * s = (bws * w + bs)s
#Given that bw (direct effect) is perforce positive and 
#bs (also direct effect) negative...
#question becomes, what should the total effect of water
#be on blooms and likewise for shade?

#if our belief is that the sign should be the same
#as that of the direct effect for each, then |bws * s| <= bw
#and bws * w <= |bs|
#or -bw <= bws * s <= bw
#and bs <= bws * w <= -bs
# bs - bw <= bws (s + w) <= bw - bs

