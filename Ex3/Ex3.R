library(ggplot2)
library(tidyr)
library(readxl)
Dados <- gather(read_excel("D:/RStudio/QualidadeARO3.xlsx", col_types = c(
  "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
  )[c("Entrecampos", "Paio-Pires")], Estação, Valor)
ggplot(Dados, aes(x = Valor, fill = Estação)) +
  geom_histogram(position = "identity", binwidth = 1, alpha = 0.8) +
  ggtitle("Níveis de ozono registados em 2020") +
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
  scale_fill_manual("Ano", values=c("cyan2", "brown2")) +
  ylab("Contagem")
