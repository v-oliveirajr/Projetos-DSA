# Este script apenas faz uma amostragem de 10 milhões de valores do arquivo train.csv, por ser muito grande

getwd()

require(readr)
require(data.table)
require(dplyr)
df <- fread(file = "train.csv")
?slice_sample
?sample

df <- data.frame(df)

sample <- slice_sample(df, n = 10000000)

head(sample)

fwrite(sample,file = "train_sample.csv")
