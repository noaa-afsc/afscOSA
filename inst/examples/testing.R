
# this file contains some preliminary code used during development, they are not
# 'testthat' kind of tests but may be useful in the future so left here for now.

# library(afscOSA)
#
# nbins <- 10 # age bins
# Neff <- 50  # multinomial sample size
# nyrs <- 30 # really no. of replicates for now
# e1 <- matrix(rep(plogis(seq(-10,10, len=nbins)), times=nyrs),
#              ncol=nbins, byrow=TRUE)
#
# ## Simulate data
# set.seed(1233)
# # correctly specified
# o1 <- t(apply(e1, 1, function(x) rmultinom(1, Neff, x)))
# # wrong Neff (weights too big)
# o2 <- t(apply(e1, 1, function(x) rmultinom(1, floor(Neff/2), x)))
# # wrong selex at largest bins
# ewrong <- e1
# ewrong[,(nbins-2):nbins] <- ewrong[,(nbins-2):nbins]/2
# o3 <- t(apply(ewrong, 1, function(x) rmultinom(1, Neff, x)))
#
# x1 <- run_osa(obs=o1, exp=e1, N=rep(Neff, nyrs), fleet='Correct', index=1:nbins,
#              index_label = 'age', years=1:nyrs, seed=10)
# x2 <- run_osa(obs=o2, exp=e1, N=rep(Neff, nyrs), fleet='Misspecified Neff', index=1:nbins,
#               index_label = 'age', years=1:nyrs, seed=10)
# x3 <- run_osa(obs=o3, exp=e1, N=rep(Neff, nyrs), fleet='Misspecified Bins', index=1:nbins,
#               index_label = 'age', years=1:nyrs, seed=10)
#
# plot_osa(list(x1,x2,x3), add_agg_CI = TRUE,
#          use_agg_proportions = FALSE)
#
# stop()
#
# # Quick simulation test of coverage for aggregate fits
# nbins <- 60 # age bins
# Neff <- 200  # multinomial sample size
# nyrs <- 50 # really no. of replicates for now
# s <- function() {
#   x0 <- sample(-(5:1), size = 1)
#   x1 <- x0 + sample(4:10, size=1)
#   s <- plogis(seq(x0,x1, len=nbins))
#   return( s/sum(s))
# }
# e <- matrix(replicate(nyrs, expr=s()),
#              ncol=nbins, byrow=TRUE)
# matplot(t(e))
#
# Neff <- sample(1:50, size = nyrs)
# eobs <- e*Neff # expected counts
# sum(rowSums(eobs)-Neff) # check
# otmp <- t(sapply(1:nrow(e), \(x) rmultinom(1, Neff[x], prob = e[x,])))
# xx <- run_osa(obs=otmp, exp=e, N=Neff, years=1:nyrs, index=1:nbins, fleet='test')
# plot_osa(list(xx))
#
# # simulation test
# osim <- list()
# for(i in 1:10000){
# osim[[i]] <- t(rowSums(sapply(1:nrow(e), \(x) rmultinom(1, Neff[x], prob = e[x,]))))
# }
#
# osim <- do.call(rbind, osim)
#
# # quick check it matches analystical
# simCI <- apply(osim, 2, \(x) quantile(x, probs=c(.025, .975)))
# plot(1:nbins, y=colSums(eobs), ylim=c(0, max(simCI)), type='b')
# lines(1:nbins, y=simCI[1,], col=2)
# lines(1:nbins, y=simCI[2,], col=2)
# # analytical - aggregate expected counts, convert to probability
# # and sum sample size to get a new multinomial
# eexp <- colSums(eobs)/sum(colSums(eobs))
# mathCI <- sapply(1:nbins, \(b) qbinom(p=c(.025, .975), size=sum(Neff), prob=eexp[b]))
# lines(1:nbins, y=mathCI[1,], col=3)
# lines(1:nbins, y=mathCI[2,], col=3)
#
#
# # # check you can pass OSA residuals if calculated elsewhere
# # out3 <- run_osa(fleet = 'Survey3', index_label = 'Age',
# #                 obs = obs, exp = exp, N = N, index = ages, years = yrs)
# # res <- matrix(out3$res$resid, nrow=length(unique(out3$res$year)),
# #               ncol=length(unique(out3$res$index)))
# # test <- run_osa(fleet = 'Survey3 -test', index_label = 'Age', res=res,
# #                 obs = obs, exp = exp, N = N, index = ages, years = yrs)
# #
# # plot_osa(list(out3, test))
