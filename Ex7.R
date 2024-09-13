set.seed(1479)
sample_means = rep(NA,7110)

for(i in 1:7110){
  sample_means[i] = mean(rbinom(75,19,0.15))
}

x <- mean(sample_means)
y <- 19*0.15
abs(x-y)
