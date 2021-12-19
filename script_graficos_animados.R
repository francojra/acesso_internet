
# Gráficos animados ------------------------------------------------------------------------------------------------------------------------
# Autora: jeanne Franco --------------------------------------------------------------------------------------------------------------------

## Fonte dos dados: Our World in Data

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

dados <- read.csv2("dados_acesso_internet.csv", header = T, sep = ",", dec = ".")
View(dados) # Visualizar dados 
is.na(dados) # Verificar se existe NAs no conjunto de dados
names(dados)

# Manipulação e descrição dos dados -------------------------------------------------------------------------------------------------------------

library(dplyr)

d <- dados %>%
  select(Individuals.using.the.Internet....of.population., Year) %>%
  rename(acesso = Individuals.using.the.Internet....of.population.) %>%
    group_by(Year) %>%
  summarise(media = mean(acesso),
             se = sd(acesso) / sqrt(length(acesso)))
d

# Gráfico ----------------------------------------------------------------------------------------------------------------------------------

library(ggplot2) # Pacote para produzir os gráficos
library(viridis) # Pacote para paleta de cores
library(gganimate) # Pacote para animação do gráfico

p <- ggplot(d, aes(x = Year, y = media, fill = Year)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(x = Year, ymin = media - se, ymax = media + se), 
                width = 0.14, position = position_dodge(.9), size = 0.9) +
  scale_fill_viridis(option = "magma") +
  labs(x = "Tempo (anos)", y = "Acesso à internet no mundo") +
  theme_dark(base_size = 15) +
  theme(legend.position = "none") 
p

p1 <- ggplot(d, aes(x = Year, y = media, fill = Year)) +
  geom_point() +
  geom_errorbar(aes(x = Year, ymin = media - se, ymax = media + se), 
                width = 0.14, position = position_dodge(.9), size = 0.9) +
  scale_color_brewer() +
  labs(x = "Tempo (anos)", y = "Acesso à internet no mundo") +
  theme_dark(base_size = 15) +
  theme(legend.position = "none") 
p1

p1 + transition_time(Year)
