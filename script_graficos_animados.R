
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
  labs(x = "Tempo (anos)", y = "Acesso à internet pela população (%)",
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
  labs(x = "Tempo (anos)", y = "Acesso à internet pela população (%)",
       caption = "Fonte dos dados: Our World in Data") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none")
p1

gr <- p1 + geom_point() + transition_reveal(Year)

anim_save("gr.gif")

# Acesso à internet por país ---------------------------------------------------------------------------------------------------------------

### Primeiro salvei várias tabelas com dados de um país de 1990 a 2018
### Depois uni os dados em uma tabela para análise


# Manipulação dos dados --------------------------------------------------------------------------------------------------------------------


a1 <- dados %>%
  select(Entity, Individuals.using.the.Internet....of.population., Year) %>%
  filter(Entity == "Qatar") 
a1

write.table(a1, "a1.txt", row.names = F, dec = ",")

a2 <- dados %>%
  select(Entity, Individuals.using.the.Internet....of.population., Year) %>%
  filter(Entity == "United Arab Emirates") 
a2

write.table(a2, "a2.txt", row.names = F, dec = ",")

a3 <- dados %>%
  select(Entity, Individuals.using.the.Internet....of.population., Year) %>%
  filter(Entity == "Iceland") 
a3

write.table(a3, "a3.txt", row.names = F, dec = ",")

a4 <- dados %>%
  select(Entity, Individuals.using.the.Internet....of.population., Year) %>%
  filter(Entity == "Kuwait") 
a4

write.table(a4, "a4.txt", row.names = F, dec = ",")

a5 <- dados %>%
  select(Entity, Individuals.using.the.Internet....of.population., Year) %>%
  filter(Entity == "Brazil") 
a5

write.table(a5, "a5.txt", row.names = F, dec = ",")

# Carregar tabela completa  ----------------------------------------------------------------------------------------------------------------


library(readxl) # Pacote que permite função para ler tabela do excel

data <- read_excel("dados_acesso_internet.xlsx")
View(data)


# Gráfico ----------------------------------------------------------------------------------------------------------------------------------


p2 <- ggplot(data, 
  aes(x = Year, y = acesso,
      group = pais, color = pais)) +
  geom_line(size = 1.4) +
  geom_segment(aes(xend = 2016, yend = acesso), 
                 linetype = 2.5, colour = "black") +
  geom_point(size = 3) + 
  geom_text(aes(x = 2016, label = pais), size = 7, hjust = 0) + 
  transition_reveal(Year) +
  coord_cartesian(clip = 'off') +
  scale_x_continuous(breaks = c(1990, 1991, 1992,
                                1993, 1994, 1995, 1996, 1997, 1998, 1999,
                                2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 
                                2008, 2009, 2010, 2011, 2012, 2013, 2014,
                                2015, 2016, 2017, 2018)) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Tempo (anos)", y = "Acesso à internet pela população (%)",
       caption = "Fonte dos dados: Our World in Data") +
  theme_gray() +
  theme(legend.position = "none", plot.margin = margin(5, 40, 2, 0.2),
        axis.text.x = element_text(angle = 60, size = 14, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"))
p2

animate(p2, height = 600, width = 800)
anim_save("internet_example.gif")
