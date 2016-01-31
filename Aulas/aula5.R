

install.packages("tree")
library(tree)
library(magrittr)
library(ggplot2)
#para séries temporais, package ts
#para textos, package tm
#para modelos mistos, lm4

#regressão linear do consumo por milha pelo peso
mtcars
ajuste_lm <- lm(mpg ~ wt, data = mtcars)
ajuste_lm

#y = a + b * x
#[1] = a, [2] = b
coeficientes <- coef(ajuste_lm)
coeficientes

mtcars %>% 
ggplot() +
  geom_point(aes(x = wt, y = mpg)) +
  labs(x = "peso (1000lbs)", y = "consumo (milhas/galão)") +
  geom_abline(intercept = coeficientes[1], slope = coeficientes[2])

#correlação = cor(var1, var2)

#função plot
# opção para mostrar 4 gráficos em uma mesma figura
par(mfrow = c(2,2))
# gráficos de diagnóstico do modelo ajuste_lm
plot(ajuste_lm)
# retorna ao normal
par(mfrow = c(1,1))
#residuals vs fitted. o que é observado e o ajustado

#mais de um parametro pra explicar uma variavel
ajuste_lm2 <- lm(mpg ~ wt + cyl, data = mtcars)
anova(ajuste_lm2)

#mostrar os residuals
res <- residuals(ajuste_lm)
res

#identificar outliers
mtcars %>% 
  add_rownames %>% 
  filter(res %>% abs > 5)

#algoritmo de seleção stepwise
# modelo aditivo completo
ajuste_lm_completo <- lm(mpg ~ ., data = mtcars)

# modelo forward
step(ajuste_lm_completo, direction = "forward")

# modelo backward
step(ajuste_lm_completo, direction = "backward")

# modelo both
step(ajuste_lm_completo, direction = "both")

#glm
ajuste_gama <- glm(Y ~ X + I(X^2) + Z,
                   data = dados,
                   family = Gamma(link = "log"))

#Regressão logística: Ligação logit
ajuste_glm <- glm(am ~ wt, data = mtcars, family = binomial)
table(mtcars$am, predict(ajuste_glm, type ='response') > 0.5)

#arvore de decisão
library(tree)
ajuste_tree <- tree(am ~ wt, data = mtcars)
summary(ajuste_tree)

plot(ajuste_tree)

text(ajuste_tree, pretty = 0)

#nesse caso, os valores da árvore serão 0 ou 1
ajuste_tree <- tree(factor(am) ~ wt, data = mtcars)

#cross-validation - serve para testar a predição e ver se existem erros
#diz qual a melhor árvore
set.seed(123)
cv_tree <- cv.tree(ajuste_tree)
plot(cv_tree)

#poda da árvore para selecionar a melhor árvore
# seleciona a arvore com 2 nós
melhor_tree <- prune.tree(ajuste_tree, best = 2)
# Grafico que representa a arvore `melhor_tree`
plot(melhor_tree)
text(melhor_tree, pretty = 0)

#explorando os dados
cv_tree$size[which.min(cv_tree$dev)]










