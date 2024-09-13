set.seed(939)
m <- 1000
n <- 1243
lambda <- 2.64
nivel_conf <- 0.95
a <- qnorm((1-nivel_conf)/2 + nivel_conf, mean = 0, sd = 1)
soma <- data.frame()
for(i in c(1:m)){
  data <- data.frame(x = rexp(n,lambda))
  mean_data <- mean(data$x)
  amplitude <- 2*a/(sqrt(n)*mean_data)
  soma <- rbind(soma, amplitude)
}
colnames(soma) <- c("valor")
media_amplitudes <- mean(soma$valor)
media_amplitudes