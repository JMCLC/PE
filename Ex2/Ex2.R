library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)
library(tibble)
Anos <- read_excel("EsperancaVida.xlsx", range = "A10:A70", col_names = c("Ano"))
HesperancaHomem <- rownames_to_column(as.data.frame(t(select(
  read_excel("./EsperancaVida.xlsx", range = "AJ9:BQ70"), c("PL - Polónia", "CZ - República Checa", "RO - Roménia")
))), "País") %>% gather("Ano", "Valor", -País)
HesperancaHomemOrganizado <- HesperancaHomem[order(HesperancaHomem["País"]),]
HesperancaHomemOrganizado["Ano"] <- Anos["Ano"]
HesperancaHomemOrganizado["Género"] <- c("Homem")
HesperancaHomemFiltrado <- filter(HesperancaHomemOrganizado, Ano >= 2002 & Ano <= 2019)
HesperancaMulher <- rownames_to_column(as.data.frame(t(select(
  read_excel("./EsperancaVida.xlsx", range = "BR9:CY70"), c("PL - Polónia", "CZ - República Checa", "RO - Roménia")
))), "País") %>% gather("Ano", "Valor", -País)
HesperancaMulherOrganizado <- HesperancaMulher[order(HesperancaMulher["País"]),]
HesperancaMulherOrganizado["Ano"] <- Anos["Ano"]
HesperancaMulherOrganizado["Género"] <- c("Mulher")
HesperancaMulherFiltrado <- filter(HesperancaMulherOrganizado, Ano >= 2002 & Ano <= 2019)
HesperancaFinal <- rbind(HesperancaHomemFiltrado, HesperancaMulherFiltrado)
ggplot(HesperancaFinal, aes(x = Ano, y = Valor, color = interaction(País, Género, sep=' - '))) + 
  geom_line(lwd = 1.5) +
  ggtitle("Esperança de Vida") +
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
  labs(color = "País - Género") +
  geom_point(data = HesperancaFinal %>% filter(Ano == 2019), aes(x = Ano, y = Valor), shape = 21, fill = "white", size = 2, stroke = 1.7) +
  geom_point(data = HesperancaFinal %>% filter(Ano == 2002), aes(x = Ano, y = Valor), shape = 21, fill = "white", size = 2, stroke = 1.7) +
  geom_text(data = HesperancaFinal %>% filter(Ano == 2019 & País != "CZ - República Checa"), aes(x = Ano, y = Valor, label = Valor), size = 3.2, vjust = 2.3, fontface = "bold") +
  geom_text(data = HesperancaFinal %>% filter(Ano == 2019 & País == "CZ - República Checa"), aes(x = Ano, y = Valor, label = Valor), size = 3.2, vjust = -1, fontface = "bold") +
  geom_text(data = HesperancaFinal %>% filter(Ano == 2002 & País != "CZ - República Checa"), aes(x = Ano, y = Valor, label = Valor), size = 3.2, vjust = 1.7, fontface = "bold") +
  geom_text(data = HesperancaFinal %>% filter(Ano == 2002 & País == "CZ - República Checa"), aes(x = Ano, y = Valor, label = Valor), size = 3.2, vjust = -1, fontface = "bold")