#ggplot2

install.packages("ggplot2")
library(ggplot2)

#tipos de gráficos
 
#geom_line: para retas definidas por pares (x,y)
#geom_abline: para retas definidas por um intercepto e uma inclinação
#geom_hline: para retas horizontais
#geom_boxplot: para boxplots
#geom_histogram: para histogramas
#geom_density: para densidades
#geom_area: para áreas
#geom_bar: para barras


#gráfico de pontos mtcars
ggplot(data = mtcars, aes(x = disp, y = mpg)) + 
  geom_point()


#adicionando cores
ggplot(data = mtcars) + 
  geom_point(aes(x = disp, y = mpg, colour = as.character(am)))


#adicionando tamanho por cilindros
ggplot(data = mtcars) + 
  geom_point(aes(x = disp, y = mpg, colour = as.character(am), size = cyl))


#boxplot
ggplot(data = mtcars) +
  geom_boxplot(aes(x = as.character(gear), y = mpg, fill = as.character(gear)))


#Exemplo1
head(diamonds)
ggplot(diamonds, aes(x = price)) +
  geom_histogram()
#Mudando as cores
ggplot(diamonds, aes(x = price)) +
  geom_histogram(colour = "white", fill = "blue")

#gráfico de carat e preço
cor(diamonds$price, diamonds$carat)
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point()

#transparência dos pontos
cor(diamonds$price, diamonds$carat)
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.1)

#incluindo o parâmetro cor
cor(diamonds$price, diamonds$carat)
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.1, aes(colour = cut))

#adicionando facet (vários gráficos)
cor(diamonds$price, diamonds$carat)
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.1, aes(colour = cut)) + 
  facet_wrap(~cut)

#mudando nome dos eixos usando uma amostra de 1000 objetos aleatoriamente escolhidos
ggplot(diamonds %>% sample_n(1000), aes(x = carat, y = price)) +
  geom_point(aes(colour = cut)) +
  facet_wrap(~cut) +
  labs(x = "Peso (Quilates)", y = "Preço (US$)", colour = "Corte")

#Exemplo2
head(economics)
View(economics)

ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line()
ggplot(economics, aes(x = date, y = uempmed)) +
  geom_line()

#fundindo dois gráficos
#primeiro temos que padronizar as duas variáveis para a mesma escala
economics %>% 
  mutate(unemploy = (unemploy - min(unemploy))/(max(unemploy) - min(unemploy)),
         uempmed = (uempmed - min(uempmed))/(max(uempmed) - min(uempmed))) %>%
  ggplot(aes(date, unemploy)) +
    geom_line() +
    geom_line(aes(y = uempmed), colour = "blue")
  
library(tidyr)    
economics %>% 
  select(date, unemploy, uempmed) %>%
  gather(indice, valor, -date) %>% 
  group_by(indice) %>% 
  mutate(valor_pad = (valor - min(valor))/(max(valor) - min(valor))) %>%
  ggplot(aes(x = date, y = valor_pad, colour = indice)) +
  geom_line() +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Desemprego", "Tempo Desempregado")) +
  labs(x = "Data", y = "Escala de 0 a 1")

#escalas de cor
scale_colour_hue()
#escalas de cor manual
scale_color_manual(values = c("red", "blue"))


#colocar a reta de regressão no gráfico de dispersão
ggplot(diamonds %>% sample_n(1000), aes(x = carat, y = price, colour = cut)) +
  geom_point() +
  geom_smooth(method = "lm")


geom_abline(intercept = -2256.361, slope = 7756.426, colour = "blue", size = 2)

#regressão liner. A função retorna o beta e o interceptor
coef(lm(price ~carat, data = diamonds))


#Exemplo3
table(diamonds$cut)

diamonds %>% 
  group_by(cut) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = cut,  y = n, fill = cut)) + 
  geom_bar(stat = "identity")

ggplot(diamond, aes(x = cut, y = n)) +
  geom_bar()


diamonds %>% 
  group_by(cut) %>% 
  summarise(n = n()) %>% 
  mutate(perc = n/sum(n)) %>% 
  ggplot(aes(x = 1, y = perc, fill = cut)) + 
  geom_bar(stat = "identity", position = "stack") +
  coord_polar(theta = "y")

#Fazendo mapas com ggplot
install.packages("maps")
library(maps)

mapa <- map_data("world")
quakes %>% 
  ggplot(aes(x = long, y = lat)) +
  geom_density2d(aes(size = stations)) +
  geom_map(aes(map_id = region), map = mapa, data = mapa)
  

#Exemplo4
diamonds %>% sample_n(10000) %>%
  group_by(cut) %>%
  mutate(correlacao = cor(carat, price)) %>% 
  ungroup() %>% 
  mutate(cut = paste(cut, "cor =", round(correlacao, 2))) %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point(aes(colour = cut)) +
  geom_text(aes(x = 3, y = 2500, label = round(correlacao, 2))) +
  facet_wrap(~cut) +
  labs(x = "Peso (Quilates)", y = "Preço (US$)") +
  guides(colour = F)



