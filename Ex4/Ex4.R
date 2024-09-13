library(ggplot2)
library(tidyr)
library(readxl)
Dados <- read_excel("./Utentes.xlsx", col_types = c(
  "numeric", "numeric", "numeric","numeric")
)[c("Colesterol", "TAD")]
ggplot(Dados, aes(x=Colesterol, y=TAD)) + 
  geom_point(color="brown2") +
  geom_smooth(method=lm, color="brown2", fill="brown2") +
  theme(
    panel.background = element_rect(fill = "gray5", colour = "gray5"),
    panel.grid = element_line(size = 0.2, linetype = 'solid', colour = "gray45"), 
    plot.background = element_rect(fill = "gray5"),
    text = element_text(colour = "white", face = "bold"),
    axis.text = element_text(color = "white"),
    legend.position = "none"
  ) +
  ggtitle("Dados Utentes")