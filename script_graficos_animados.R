
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

## Valores das médias de acesso do maior para menor e por ano

d2_resume <- arrange(d1, desc(media))
View(d2_resume)

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
  geom_segment(aes(xend = 2020, 
                   yend = Individuals.using.the.Internet....of.population.)) +
  geom_text(aes(x = 2020, label = Entity), hjust = 0) + 
  labs(x = "Tempo (anos)", y = "Acesso à internet no mundo",
       caption = "Fonte dos dados: Our World in Data") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none")
p1

gr <- p1 + geom_point() + transition_reveal(Year)

anim_save("gr.gif")


# Acesso à internet por país ---------------------------------------------------------------------------------------------------------------

a1 <- dados %>%
  select(Entity, Individuals.using.the.Internet....of.population., Year) %>%
  filter(Entity == "Qatar") 
a1

a2 <- dados %>%
  select(Entity, Individuals.using.the.Internet....of.population., Year) %>%
  filter(Entity == "United Arab Emirates") 
a2

a3 <- dados %>%
  select(Entity, Individuals.using.the.Internet....of.population., Year) %>%
  filter(Entity == "Iceland") 
a3

a4 <- dados %>%
  select(Entity, Individuals.using.the.Internet....of.population., Year) %>%
  filter(Entity == "Kuwait") 
a4

a5 <- dados %>%
  select(Entity, Individuals.using.the.Internet....of.population., Year) %>%
  filter(Entity == "Brazil") 
a5

all <- right_join(a1, a2, by = "Year")

all1 <- left_join(a3, a4, by = "Year")

all2 <- full_join(all, all1, by = "Year")

allf <- full_join(all2, a5, by = "Year")

View(all)

p2 <- ggplot(a1, 
  aes(x = Year, y = media,
      group = Entity, color = Entity)) +
  geom_line(size = 0.68) +
  scale_color_viridis_d() +
  geom_segment(aes(xend = 2020, 
                   yend = media)) +
  geom_text(aes(x = 2020, label = Entity), hjust = 0) + 
  labs(x = "Tempo (anos)", y = "Acesso à internet no mundo",
       caption = "Fonte dos dados: Our World in Data") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none")
p2


"1990", "1991", "1992", "1993", "1994",
                   "1995", "1996", "1997", "1998", "1999",
                   "2000", "2001", "2002", "2003", "2004",
                   "2005", "2006", "2007", "2008", "2009",
                   "2010", "2011", "2012", "2013", "2014", "2015",
                   "2016", "2017", "2018")