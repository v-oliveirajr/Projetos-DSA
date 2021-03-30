# Lista de Exercícios Parte 3 - Capítulo 11

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("C:/FCD/BigDataRAzure/Cap12")
getwd()


# Definindo o Problema: Analisando dados das casas de Boston, nos EUA e fazendo previsoes.

# The Boston Housing Dataset
# http://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html

# Seu modelo deve prever a MEDV (Valor da Mediana de ocupação das casas). Utilize um modelo de rede neural!

# Carregando o pacote MASS
library(MASS)

# Importando os dados do dataset Boston
set.seed(101)
dados <- Boston
head(dados)
View(dados)

# Resumo dos dados
str(dados)
summary(dados)
any(is.na(dados))

# Carregando o pacote para Redes Neurais
install.packages("neuralnet")
library(neuralnet)
?neuralnet

# Pré-processamento e amostragem

summary(dados$zn)

# Função de padronização
padronizar <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Padronizando
dados_p <- as.data.frame(lapply(dados, padronizar))
View(dados_p)

# Função de voltar ao normal (medv)
voltar <- function(x){
  return((x*(max(dados$medv)-min(dados$medv)))+min(dados$medv))
}

# Amostragem e divisão entre dados de treino e teste
library(dplyr)
?slice
?sample
dtreino <- dados_p %>% slice_sample(prop = 0.75,replace = F)
dteste <- setdiff(dados_p,dtreino)
View(dtreino)

# Criação do modelo

modelo1 <- neuralnet(medv~.,data = dtreino)
gwplot(modelo1)
plot(modelo1)
dteste$medv_previsto <- predict(modelo1,dteste)
dteste$medv <- dteste$medv %>% voltar()
dteste$medv_previsto <- dteste$medv_previsto %>% voltar()
dteste$Erro <- abs(dteste$medv - dteste$medv_previsto)*100/dteste$medv
dteste$Erro <- round(dteste$Erro,digits = 2)
dteste$Erro <- paste(dteste$Erro,'%')
View(dteste)

modelo1

barplot(dteste$medv,dteste$medv_previsto,col = c('skyblue','brown'),main = 'Previsão do Medv',legend.text = T)
legend('topright',legend = c('Medv real','Medv previsto'), fill = c('skyblue','brown'))
?barplot

# Valores mais próximos da média são previstos com menor erro