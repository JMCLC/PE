library(dplyr)
n <- c(4,29,66)
set.seed(939)
Dados = data.frame(matrix(nrow=0, ncol = 2))
colnames(Dados) = c("N","Valor")
for (currentN in n) {
  for (i in 1:680) {
    amostras <- runif(currentN, min = 5, max = 9)
    media <- mean(amostras)
    Dados[nrow(Dados) + 1,] = c(currentN, media)
  }
}
Dados4 <- Dados %>% filter(N == 4)
Curva4 <- rnorm(680,mean(Dados4$Valor),sd(Dados4$Valor))
Dados29 <- Dados %>% filter(N == 29)
Curva29 <- rnorm(680,mean(Dados29$Valor),sd(Dados29$Valor))
Dados66 <- Dados %>% filter(N == 66)
Curva66 <- rnorm(680,mean(Dados66$Valor),sd(Dados66$Valor))
par(bg = 'gray5', mfrow=c(1,3))
hist(Dados4$Valor,main="Histograma de Frequência Relativa para N = 4",col = "brown4",prob=TRUE,xlab = "Média das Amostras",ylab = "Frequência", fg = "gray45", col.lab = "white", col.axis = "white", col.main = "white")
lines(density(Curva4),col="brown2",lwd=4)
hist(Dados29$Valor,main="Histograma de Frequência Relativa para N = 29",col = "brown4",prob=TRUE,xlab = "Média das Amostras",ylab = "Frequência", fg = "gray45", col.lab = "white", col.axis = "white", col.main = "white")
lines(density(Curva29),col="brown2",lwd=4)
hist(Dados66$Valor,main="Histograma de Frequência Relativa para N = 66",col = "brown4",prob=TRUE,xlab = "Média das Amostras",ylab = "Frequência", fg = "gray45", col.lab = "white", col.axis = "white", col.main = "white")
lines(density(Curva66),col="brown2",lwd=4)