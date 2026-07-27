library(afscOSA)

nbins <- 12 # age bins
Neff <- 100  # multinomial sample size
nyrs <- 20 # really no. of replicates for now
e1 <- matrix(rep(plogis(seq(-10,10, len=nbins)), times=nyrs),
             ncol=nbins, byrow=TRUE)

## Simulate multinomial data
set.seed(121)
# correctly specified
o1 <- t(apply(e1, 1, function(x) rmultinom(1, Neff, x)))
# wrong Neff (weights too big)
o2 <- t(apply(e1, 1, function(x) rmultinom(1, floor(Neff/2), x)))
# wrong selex at largest bins
ewrong <- e1
ewrong[,(nbins-2):nbins] <- ewrong[,(nbins-2):nbins]/1.5
o3 <- t(apply(ewrong, 1, function(x) rmultinom(1, Neff, x)))

# simulate D-M data
theta <- 2
prob <- e1[1,]/sum(e1[1,])
alpha <- Neff*prob*theta
o4 <- t(apply(e1, 1, function(x) compResidual::rdirM(1, Neff, alpha)))
o5 <- t(apply(e1, 1, function(x) compResidual::rdirM(1, Neff, alpha)))
o6 <- t(apply(e1, 1, function(x) compResidual::rdirM(1, Neff, alpha)))
prob <- ewrong[1,]/sum(ewrong[1,])
alpha <- Neff*prob*theta
o7 <- t(apply(e1, 1, function(x) compResidual::rdirM(1, Neff, alpha)))


