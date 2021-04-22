library(readr)
library(data.table)
library(dplyr)
library(ROSE)


# Fazendo a leitura das colunas do dataset
colunas <- fread(file = 'train.csv',header = T,stringsAsFactors = T,nrows = 0)
View(colunas)

# Carregando o dataset de amostras

df <- fread(file = 'train_sample.csv',header = T,stringsAsFactors = T,drop = 1, data.table=F)
View(head(df))
View(tail(df))
str(df)
summary(df)
table(df$is_attributed)

# Análise exploratória

View(df)
prop.table(table(df$is_attributed))

# Temos uma proporção de 99,7% e 0,03% entre fraude e não-fraude, faremos um undersampling para o balanceamento
# através da biblioteca ROSE

balan_df <- ovun.sample(is_attributed~., data = df, method = 'under',p = 0.5)
str(balan_df)
balan_df <- balan_df$data
prop.table(table(balan_df$is_attributed))
100*prop.table(table(balan_df$device))


