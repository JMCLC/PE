library(ggplot2)
set.seed(819)
Dados <- data.frame(matrix(nrow=0, ncol = 2))
colnames(Dados) <- c("N","Valor")
a <- qnorm(0.04/2 + 0.96, mean = 0, sd = 1)
n <- 100
while (n <= 5000) {
  j <- 1
  amplitudes <- data.frame(matrix(nrow=0, ncol = 1))
  colnames(amplitudes) <- c("Valor")
  for(j in 1:1400) {
    media <- mean(rexp(n,1.7))
    amplitude <- 2*a/(sqrt(n)*media)
    amplitudes[nrow(amplitudes) + 1,] <- amplitude
  }
  Dados[nrow(Dados) + 1,] = c(n, mean(amplitudes$Valor))
  n <- n + 100
}
ggplot(Dados, aes(x = N, y = Valor)) +
  geom_line(color="brown2") +
  geom_point(color="brown2") +
  theme(
    panel.background = element_rect(fill = "gray5", colour = "gray5"),
    panel.grid = element_line(size = 0.2, linetype = 'solid', colour = "gray45"), 
    plot.background = element_rect(fill = "gray5"),
    text = element_text(colour = "white", face = "bold"),
    axis.text = element_text(color = "white"),
    legend.position = "none"
  ) +
  ggtitle("Média de amplitudes de intervalos de confiança") +
  ylab("Média das Amplitudes") +
  xlab("Tamanho População")
