require (data.table)
require (dplyr)
require(tidyr)

tpro <- read.csv('producto_tabla.csv')
View(tpro)

# Filtragem dos dados: queremos obter o nome genérico dos produtos, sem o peso ou qtde

?separate

teste <- separate(tpro,NombreProducto,into = c("Nombre",'Resto'),sep = "\\d\\dg")
teste <- separate(teste,Nombre,into = c("Nombre",'Resto'),sep = "\\d") %>% select(Producto_ID,Nombre)
View(teste)

?arrange

?unique
nuevo <- teste %>%arrange(Nombre,by_group = T) 
?distinct
View(nuevo)

nuevo [1,2] = '7 Granos'
nuevo [2,2] = 'Whole Wheat'
nuevo [3,2] = 'Tarima MiniMilk Kitty'
nuevo [4,2] = 'Tarima MiniMilk Kitty'

head(nuevo)

require(sqldf)
# Criamos uma função que retorna o nome completo no dataset original para preenchermos os valores missing gerados pela filtragem

valor_original <- function(valor){fn$sqldf("SELECT *
             FROM tpro
             WHERE tpro.Producto_ID = $valor")} 

valor_original(714)
head(nuevo, n = 15L)
valor_original(43111)
nuevo [5,2] = "Whole Wheat"
valor_original(43160)
nuevo [6,2] = "7 Granos"
valor_original(43364)
nuevo [7,2] = "12Granos Multigra TwinPack"
valor_original(48227)
nuevo [8,2] = '12Granos Multigra TwinPack'
valor_original(48228)
nuevo [9,2] = '7 Granos'

nuevo <- nuevo%>%arrange(Nombre,by_group = T) 

write.csv(nuevo,file = 'producto_tabla_nuevo.csv',row.names = F)
