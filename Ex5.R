set.seed(1647)

n <- 50
teorico <- 1-pexp(2,rate=0.27)
Fn <- ecdf(rexp(n, 0.27))
estimado<- 1 - Fn(2)
diferença <- abs(estimado-teorico)