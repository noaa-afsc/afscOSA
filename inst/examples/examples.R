library(afscOSA)
library(dplyr)
X<-matrix(c(
  9,    1,    5,    9,    9,    3,    6,    3,    3,     4,
  1,    5,    7,    1,    3,    6,    8,    9,    6,     5,
  3,    7,    4,    2,    1,    8,    4,    5,    1,     2,
  4,    2,    3,    5,    3,    1,    0,    1,    0,     2,
  1,    3,    3,    0,    5,    5,    4,    3,    1,     0,
  7,    7,    3,    8,    4,    2,    3,    4,   14,    12
), nrow=6, ncol=10, byrow=TRUE)

P<-matrix(c(
  0.32, 0.08, 0.16, 0.24, 0.32, 0.20, 0.20, 0.16, 0.16, 0.16,
  0.16, 0.16, 0.24, 0.20, 0.12, 0.16, 0.32, 0.28, 0.20, 0.20,
  0.12, 0.24, 0.20, 0.12, 0.04, 0.24, 0.16, 0.12, 0.04, 0.08,
  0.04, 0.16, 0.16, 0.12, 0.04, 0.08, 0.00, 0.08, 0.04, 0.20,
  0.12, 0.08, 0.12, 0.00, 0.12, 0.12, 0.12, 0.08, 0.04, 0.04,
  0.24, 0.28, 0.12, 0.32, 0.36, 0.20, 0.20, 0.28, 0.52, 0.32
), nrow=6, ncol=10, byrow=TRUE)

osa1 <- afscOSA::run_osa(obs=t(X),exp=t(P), N=rowSums(X),fleet='Test1', years=1:10, index=1:6)
osa2 <- afscOSA::run_osa(obs=t(X),exp=t(P), N=rowSums(X),fleet='Test2', years=1:10, index=1:6)


afscOSA::plot_osa(list(osa1))
afscOSA::plot_osa(list(osa1, osa2))
afscOSA::plot_osa(list(osa1), addCI=FALSE)
afscOSA::plot_osa(list(osa1, osa2), addCI=FALSE)
afscOSA::plot_osa(list(osa1), hjust=-.5)
afscOSA::plot_osa(list(osa1, osa2), vjust=1.2)
