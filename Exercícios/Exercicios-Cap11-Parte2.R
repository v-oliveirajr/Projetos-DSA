# Lista de Exercícios Parte 2 - Capítulo 11

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("C:/FCD/BigDataRAzure/Cap12")
getwd()


# Regressão Linear
# Definição do Problema: Prever as notas dos alunos com base em diversas métricas
# https://archive.ics.uci.edu/ml/datasets/Student+Performance
# Dataset com dados de estudantes
# Vamos prever a nota final (grade) dos alunos

# Carregando o dataset
df <- read.csv2('estudantes.csv')

# Explorando os dados
head(df)
summary(df)
str(df)
any(is.na(df))

# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("dplyr")
library(ggplot2)
library(ggthemes)
library(dplyr)
library(magrittr)

# Pré-processamento dos dados

?sapply
df2 <- df
df2$traveltime <- as.character(df2$traveltime)
df2$traveltime <- sapply(df2$traveltime,
                        function(x){
                           if(x=='1'){return('<15min')}
                          if(x=='2'){return('15-30min')}
                          if(x=='3'){return('30min-1hour')}
                          else {return('>1hour')}
                        })

df2$studytime <- sapply(df2$studytime,
                         function(x){
                           if(x=='1'){return('<2hours')}
                           if(x=='2'){return('2-5hours')}
                           if(x=='3'){return('5-10hours')}
                           else {return('>10hours')}
                         })

df2$address <- sapply(df2$address, function(x){ifelse(x=='R','rural','urban')})
df2$famsize <- sapply(df2$famsize, function(x){ifelse(x=='LE3','<= 3','< 3')})
df2$Medu <- as.character(df2$Medu)
df2$Medu <- sapply (df2$Medu,
                    function(x){
                      if(x=='0')return('none')
                      if(x=='1')return('primary-4th_grade')
                      if(x=='2')return('5th-9th_grade')
                      if(x=='3')return('secondary')
                      if(x=='4')return('higher')
                      else return(NA)
                    })
df2$Fedu <- sapply (df2$Fedu,
                    function(x){
                      if(x=='0')return('none')
                      if(x=='1')return('primary-4th_grade')
                      if(x=='2')return('5th-9th_grade')
                      if(x=='3')return('secondary')
                      if(x=='4')return('higher')
                      else return(NA)
                    })
df2$traveltime <- as.character(df2$traveltime)
df2$studytime <- as.character(df2$studytime)
df2$Medu <- as.character(df2$Medu) 
df2$Fedu <- as.character(df2$Fedu)
df2$address <- as.character(df2$address)
df2$famsize <- as.character(df2$famsize)
View(df2)
str(df2)
summary(df2)
table(df2$studytime)
cor(df2[c("G1",'G2','G3')])

# Separando entre dados de treino e teste
df2_dados <- df2[1:295,]
df2_teste <- df2[296:395,]

str(df2_dados)
str(df2_teste)

# Criando o modelo

modelo <- lm(G3~Medu+Fedu+address+famsize+traveltime+studytime+G1+G2,data = df2_dados)

?predict
df2_teste$G3_previsto <- predict(modelo,df2_teste)
df2_teste
str(df2_teste)

summary(modelo)

# Versão 2

modelo_2 <- lm(G3~paid+goout+absences+failures+reason+Mjob+Fjob+G1+G2,data=df2_dados)

summary(modelo_2)

# Prevendo a nota do terceiro perído

df2_teste$G3_previsto <- predict(modelo_2,df2_teste)
View(df2_teste)
df2_teste$Erro <- (abs((df2_teste$G3_previsto - df2_teste$G3) / df2_teste$G3_previsto ) * 100) %>%
  paste('%')

# Versão 0

modelo0 <- lm(G3~failures+studytime+traveltime+Medu+health+goout+freetime,data=df)
summary(modelo0)

# Com os dados originais (valores numéricos) o modelo encontrado tem menor eficácia.

# Sem as variáveis G2 e G3 os modelos não possuem eficácia satisfatória




