test_that("Dirichlet-multinomial works", {
  x1 <- run_osa(obs=o4, exp=e1, N=rep(Neff, nyrs), fleet='D-M: Correct', index=1:nbins,
                theta=theta,
                index_label = 'age', years=1:nyrs, seed=10)
  x2 <- run_osa(obs=o5, exp=e1, N=rep(Neff, nyrs), fleet='D-M: Underdispersed', index=1:nbins,
                index_label = 'age', years=1:nyrs, seed=10, theta=theta/5)
  x3 <- run_osa(obs=o6, exp=e1, N=rep(Neff, nyrs), fleet='D-M: Overdispersed', index=1:nbins,
                index_label = 'age', years=1:nyrs, seed=10, theta=theta*50)
  x4 <- run_osa(obs=o7, exp=e1, N=rep(Neff, nyrs), fleet='D-M: Misspecified bins', index=1:nbins,
                index_label = 'age', years=1:nyrs, seed=10, theta=theta*50)

  expect_warning(plot_osa(list(x1,x2,x3,x4)), regexp="The following Pearson residuals")
})
