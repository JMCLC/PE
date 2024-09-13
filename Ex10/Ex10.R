library(ggplot2)
set.seed(12)
Dados <- data.frame(matrix(nrow=0, ncol = 3))
colnames(Dados) <- c("N","Valor", "ValorContaminado")
a <- qnorm(0.02/2 + 0.98, mean = 0, sd = 1)
n <- 100
while (n <= 2500) {
  j <- 1
  amplitudes <- data.frame(matrix(nrow=0, ncol = 1))
  amplitudesContaminadas <- data.frame(matrix(nrow=0, ncol = 1))
  colnames(amplitudes) <- c("Valor")
  colnames(amplitudesContaminadas) <- c("Valor")
  for(j in 1:700) {
    media <- mean(rexp(n,3.62))
    amplitude <- 2*a/(sqrt(n)*media)
    amplitudes[nrow(amplitudes) + 1,] <- amplitude
    if (floor(runif(1, 1,100)) <= 15) {
      media <- mean(rexp(n,0.1))
      amplitude <- 2*a/(sqrt(n)*media)
      amplitudesContaminadas[nrow(amplitudesContaminadas) + 1,] <- amplitude
    }
  }
  Dados[nrow(Dados) + 1,] = c(n, mean(amplitudes$Valor), mean(amplitudesContaminadas$Valor))
  n <- n + 100
}
ggplot(Dados, aes(x = N)) +
  geom_line(aes(y=Valor, colour = "Não Contaminada")) +
  geom_line(aes(y=ValorContaminado, colour = "Contaminada")) +
  geom_point(aes(y=Valor, colour = "Não Contaminada")) +
  geom_point(aes(y=ValorContaminado, colour = "Contaminada")) +
  theme(
    panel.background = element_rect(fill = "gray5", colour = "gray5"),
    panel.grid = element_line(size = 0.2, linetype = 'solid', colour = "gray45"), 
    plot.background = element_rect(fill = "gray5"),
    text = element_text(colour = "white", face = "bold"),
    axis.text = element_text(color = "white"),
    legend.background=element_rect(fill="gray5"),
    legend.key=element_rect(colour="gray5"),
    legend.key.size=unit(1.5, "lines"),
  ) +
  ggtitle("Média de amplitudes de amostras contaminadas e não contaminadas") +
  labs(colour="Amostras") +
  ylab("Média das Amplitudes") +
  xlab("Tamanho População")