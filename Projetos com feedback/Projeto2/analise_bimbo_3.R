require (data.table)
require (dplyr)
require(tidyr)
require(sqldf)

?fread
tabla <- fread(file = 'train_sample.csv',data.table = F)
producto <- read.csv(file = 'producto_tabla_nuevo.csv')

# Fazendo o join entre os datasets original e o de produto

View(tabla)
View(producto)
?sqldf
tabla.Producto_ID <- sqldf("SELECT P.NOMBRE AS [NOMBRE]
                            FROM tabla AS T
                            LEFT JOIN producto AS P 
                            ON T.PRODUCTO_ID = P.PRODUCTO_ID")

# O resultado da query SQL é um dataframe, para fazermos a conversão para um vetor e assim usá-lo como coluna basta usar o subset de todo o dataframe [,]

tabla$Producto_ID <- as.factor(tabla.Producto_ID[,])
str(tabla)

# A coluna possui 50 valores diferentes, faremos: divisão para categorias de produtos; codificação destas categorias visto que usaremos um modelo de regressão


as.factor(gsub("Roles.*","Doces",tabla$Producto_ID))
as.factor(gsub("Twinkies.*","Doces",tabla$Producto_ID))

# Podemos usar várias funções gsub em conjunto, porém existem maneiras mais práticas e profissionais

require(stringr)

productos_doces <- c('Donitas.*|Twinkies.*|Roles.*|Submarinos.*|Barritas.*|Mini.*|Bran Frut Pina.*|Doces.*|Bimbunuelos.*
                     |Div Tira.*|Napolitano.*|Barra.*|Barri.*|Bolsa.*|Bolsita.*|Bombonete.*|Bon o Bon.*|Bran Frut.*|BranFrut.*
                     |Bubulubu.*|Cajeta.*|Camioncitos.*|Canelita.*|Choco Doces.*|Chocoretas.*|Classicas.*|Concha.*|Costalito.*
                     |Cuadritas.*|Dulcigomas.*|Duo Barrita|Healthy.*|Lata.*|Maxi.*|Multiempaque.*|Multipack.*
                     |Pastilla.*|Raztachoc.*|Surtido.*|Tartaletas.*|Fresca.*|Joya.*|Manzana.*')



productos_panes <- c('Pan.*|Cuernitos.*|Rebanada.*|Bimbollos.*|Conchas.*|Donas.*|Medias.*|Tostado.*|7 Granos.*|12.*
                     |Bagels.*|Baguette.*|Bigotes.*|Bimkingo.*|Bisquet.*|Bollo.*|Butter.*|Burritos.*|Cachitos.*|Cajilla.*
                     |Charola.*|Colchones.*|Combo.*|Conchitas.*|Cuern.*|Dona.*|Duo Bollos.*|Duo Panes.*|Dutch.*|Empanaditas.*
                     |English.*|Exhibidor.*|Figatza.*|Hot Dog.*|Hotcakes.*|Integral.*|Leche Bollos.*|Lonchibox.*|Mantecada.*
                     |Marias.*|Molido.*|Oatnut.*|Orej.*|Palitrigos.*|Rosca.*|Runners.*|Sand.*|Semita.*|Silueta.*|SuperPanes.*
                     |Thins.*|Tortilla.*|Tri Pack.*|Tripack.*|Twin Pack Thins.*|Vidrio.*|Whole.*|Wonder.*|Empanaditas.*|Burrito.*|Costalito.*|Super.*|12 Granos Multigra.*')



productos_pasteles <- c('Besos.*|Gansito.*|Bollos.*|Chocotorro.*|Madalenas.*|Mantecada.*|Nito.*|Pinguinos.*|Polvoron.*
                      |Big Muffin.*|Bimbolunch.*|Biscotel.*|Dalmata.*|Mi.*|Napi.*|Nuevo Napi.*|Pachoncitos.*|Pastel.*
                        |Pay.*|Pinguis.*|Pipucho.*|Rocko.*|Rollo.*|Tuin.*|Tuinky.*|Leche.*|Pasteles o Bizcochos.*|Duo Pasteles.*|Pastel Chocolate Rec Orig.*')



productos_bizcochos <- c('Canelitas.*|Chocodonitas.*|Doraditas.*|Empanizador.*|Principe.*|Tortillina.*|Triki.*|Deliciosas.*
                         |Decanelas.*|Animalitos.*|Canapinas.*|Chicharron.*|Chips.*|Chipotles.*|Chocochi.*|Churritos.*
                         |Deliciosas.*|Duo Bigotes.*|Duo Plativolos.*|Galleta.*|Ganchera.*|Gansi.*|Golden.*|Hot Nuts.*|Kiyakis.*
                         |Kranky.*|Lors.*|Magnas.*|Nacho.*|Pallet.*|Papatinas.*|Pastiseta.*|Plativolos.*|Saladas.*|Salmas.*
                         |Sponch.*|Suavicremas.*|Takis.*|Tarima.*|Tartinas.*|Tira.*|Tostachos.*|Tostada.*|Tostadita.*|Totopos.*
                         |Tubo Galleta.*|Div Bizcochos.*|Galleton.*|Obleas.*|Tortinachos.*|Totopo.*|Balcon.*|Salma.*|Tost.*|Churritos Chile y Limon.*|Salmitas.*')

tabla$Producto_ID <- str_replace(tabla$Producto_ID,productos_doces,'Doces')
tabla$Producto_ID <- str_replace(tabla$Producto_ID,productos_panes,'Panes')
tabla$Producto_ID <- str_replace(tabla$Producto_ID,productos_pasteles,'Pasteles')
tabla$Producto_ID <- str_replace(tabla$Producto_ID,productos_bizcochos,'Bizcochos')

tabla$Producto_ID <- str_replace(tabla$Producto_ID,'12Granos Multigra TwinPack.*','Panes')
tabla$Producto_ID <- str_replace(tabla$Producto_ID,'Duo.*','Doces')
tabla$Producto_ID <- str_replace(tabla$Producto_ID,'Kiyakis','Bizcochos')


productos_bebidas <- c('ActiFresh.*|Agua.*|Ami.*|Coca.*|Fanta.*|Ice.*|Nectar.*|Powerade.*|Sidral.*|Sprite.*')
tabla$Producto_ID <- str_replace(tabla$Producto_ID,productos_bebidas,'Bebidas')
tabla$Producto_ID <- str_replace(tabla$Producto_ID,'Bizcochos.*','Bizcochos')


table(tabla$Producto_ID)

View(tabla)

# Agora elaborando a codificação para os produtos: 1 - Bizcochos, 2 - Pasteles, 3 - Doces, 4 - Panes, 5 - Bebidas

tabla$Producto_ID <- gsub('Bizcochos',1,tabla$Producto_ID)
tabla$Producto_ID <- gsub('Pasteles',2,tabla$Producto_ID)
tabla$Producto_ID <- gsub('Doces',3,tabla$Producto_ID)
tabla$Producto_ID <- gsub('Panes',4,tabla$Producto_ID)
tabla$Producto_ID <- gsub('Bebidas',5,tabla$Producto_ID)

table(tabla$Producto_ID)

# Removendo as colunas ID que não serão utilizadas

tabla <- tabla[,-2:-5]
str(tabla)

tabla$Producto_ID<- as.numeric(tabla$Producto_ID)

# Salvando o arquivo

fwrite(tabla,file = 'train_sample_modified.csv')

tabla <- fread(file = 'train_sample_modified.csv',data.table = F)



View(hey)

isTRUE(all.equal(hey,tabla))


# Análise Exploratória

require(ggplot2)

tabla$Producto_ID<- as.factor(tabla$Producto_ID)

str(tabla)

ggplot(data = tabla,aes(x = Producto_ID,y = Venta_uni_hoy)) +
  geom_boxplot() + ggtitle('Unidades vendidas por tipo de produto')

# Existem muitos outliers que podem comprometer a eficácia de possíveis modelos de regressão

ggplot(data = tabla,aes(x = Producto_ID,y = Dev_uni_proxima)) +
  geom_boxplot() + ggtitle('Unidades devolvidas por tipo de produto')


