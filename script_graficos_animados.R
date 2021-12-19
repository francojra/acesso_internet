
# Gráficos animados ------------------------------------------------------------------------------------------------------------------------
# Autora: jeanne Franco --------------------------------------------------------------------------------------------------------------------

## Fonte dos dados: Our World in Data

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

dados <- read.csv2("dados_acesso_internet.csv", header = T, sep = ",", dec = ".")
View(dados) # Visualizar dados 
is.na(dados) # Verificar se existe NAs no conjunto de dados
names(dados) # Nomes das variáveis dos dados

# Manipulação e descrição dos dados -------------------------------------------------------------------------------------------------------------

library(dplyr)

d <- dados %>%
  select(Individuals.using.the.Internet....of.population., Year) %>%
  rename(acesso = Individuals.using.the.Internet....of.population.) %>%
    group_by(Year) %>%
  summarise(media = mean(acesso),
             se = sd(acesso) / sqrt(length(acesso)))
d

d1 <- dados %>%
  select(Entity, Individuals.using.the.Internet....of.population., Year) %>%
  rename(acesso = Individuals.using.the.Internet....of.population.) %>%
  group_by(Entity, Year) %>%
  summarise(media = mean(acesso)) 
d1

## Valores das médias de acesso da maior para menor e por ano e por país

d2_resume <- arrange(d1, desc(media))
view_follow(d2_resume)

# Gráfico ----------------------------------------------------------------------------------------------------------------------------------

library(ggplot2) # Pacote para produzir os gráficos
library(viridis) # Pacote para paleta de cores
library(gganimate) # Pacote para animação do gráfico

p <- ggplot(d, aes(x = Year, y = media, fill = Year)) +
  geom_col(position = "dodge") +
  geom_point(position = position_dodge(.9), size = 1.7) +
  geom_errorbar(aes(x = Year, ymin = media - se, ymax = media + se), 
                width = 0.14, position = position_dodge(.9), size = 0.9) +
  scale_fill_viridis() +
  labs(x = "Tempo (anos)", y = "Acesso à internet no mundo",
       caption = "Fonte dos dados: Our World in Data") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none") 
p 

p1 <- ggplot(dados, 
  aes(x = Year, y = Individuals.using.the.Internet....of.population.,
      group = Entity, color = Entity)) +
  geom_line(size = 0.68) +
  scale_color_viridis_d() +
  labs(x = "Tempo (anos)", y = "Acesso à internet no mundo",
       caption = "Fonte dos dados: Our World in Data") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none")
p1

gr <- p1 + geom_point() + transition_reveal(Year)

anim_save("gr.gif")
