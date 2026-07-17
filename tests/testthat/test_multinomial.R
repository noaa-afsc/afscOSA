
test_that("multinomial works", {
x1 <- run_osa(obs=o1, exp=e1, N=rep(Neff, nyrs), fleet='Correct', index=1:nbins,
              index_label = 'age', years=1:nyrs, seed=10)
x2 <- run_osa(obs=o2, exp=e1, N=rep(Neff, nyrs), fleet='Misspecified Neff', index=1:nbins,
              index_label = 'age', years=1:nyrs, seed=10)
x3 <- run_osa(obs=o3, exp=e1, N=rep(Neff, nyrs), fleet='Misspecified Bins', index=1:nbins,
              index_label = 'age', years=1:nyrs, seed=10)

expect_warning(plot_osa(list(x1,x2,x3)), regexp="The following Pearson residuals")
})

