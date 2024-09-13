library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
dados <- read_excel("./ResiduosPerCapita.xlsx", range = "A13:C43", col_names = c("País", "2004", "2018"), col_types = c("text", "numeric", "numeric"))
dados_filtrados = filter(dados, País == "PL - Polónia" | País == "NL - Países Baixos" | País == "FI - Finlândia")
dados_duplicados <- dados_filtrados %>% gather("Ano", "Valor", -País)
ggplot(dados_duplicados, aes(x = País, y = Valor, fill = Ano)) + 
  geom_col(position = "dodge") +
  ggtitle("Produção de Resíduos per Capita") +
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
  scale_fill_manual("Ano", values=c("brown3", "brown4")) +
  geom_text(aes(label = Valor), vjust = 1.5, position = position_dodge(.9), colour = "white", fontface = "bold")