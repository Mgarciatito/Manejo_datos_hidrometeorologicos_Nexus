
# TALLER DE HIDROLOGIA - SESIONES PREVIAS --------------------------------------


### Clase de objetos basicos en R
## Numericos (Numeros reales: 1,2,3,4,5,6)
#Integer
#Complex
#Logicos , true or false

1
1+2
#1+2
pi
a<- 1
class(a)
a <- T
class(a)
a <- 'beto'
class(a)

pepe <- 10
maria <- 15
luis <- 20
resultado <- pepe + maria +luis
resultado


#Tipos de datos
#numerico
a <- 1
class(a)

#Caracteres
b <- 'Miguel'
class(b)


#Datos de calida de aire
head(airquality)
datos <- airquality
View(datos)
plot(datos, col='Blue')
plot(datos$Temp, type = 'l')


#Iniciando (vector) (se guardan en coordenadas)
x <- c(1,2,3,4,5,6)# en orden para guardar info de cualquier cosa
class(x)

a <- c('p','x','a','s','e','f','r','g','q')
a[c(5,8)]

y <- c(7,8,9,10,11,12)
y

b <- x+y
b

mean(b)
min(b)
max(b)



### Arrays tambien puede ser descrita como vector multidimensional

a <- array(c(7,8,9,10,11,12), dim = c(2,3))#2 filas 3 columnas
a

b <- array(c(1,2,3,4,5,6), dim = c(2,3))
b

a+b

a <- matrix(1:12, nrow = 5, ncol = 4)
a

## Manipulacion de datos 

estacion_a <- matrix(1:12)
mean(estacion_a)
estacion_b <- matrix(13:24)
mean(estacion_b)

#Podemos unir columnas
new <- cbind(estacion_a, estacion_b)
new

#Agregamos filas
total <- rbind(new, colMeans(new))

# convertimos a dataframes
tabla <- data.frame(total)

## Unimos Columnas
cbind(tabla, rowSums(total))


#Dataframes
# son estructuras de datos de dos dimensiones que pueden contener información heterogenea

pp <- data.frame(estacion_1 = c(110,115,113,114), estacion_2 = c(220,118,119,116))
pp
plot(pp$estacion_1, type = 'l')

#Probando
1:8

letters[1:26]

LETTERS[1:5]

#Caso practico en Hidrologia 
# creamos un vector con c()

lluvia <- c(15.1,12.5,13.6,14,16,17,18,19)
lluvia[5]
lluvia [5:8]
lluvia [5:9]

mean(lluvia)
sum(lluvia)
sd(lluvia)
var(lluvia)

summary(lluvia)

boxplot(lluvia)
boxplot(lluvia, ylab= 'pp', col='blue')

hist(lluvia, main= 'Pp', xlab = 'Pp diaria', freq = F, col= 'Blue') #Contando solamente la densidad
hist(lluvia, main= 'Pp', xlab = 'Pp diaria', freq = T, col= 'Red') #cantidad de datos por clase

#### PONGAMONOS SERIOS #####################

setwd('D:/Taller_Nexus/Insumos/')

pp <- read.csv('Pre_practica.csv', header = T, sep = ",")
View(pp)

nuevo <- pp[2:6]
plot(nuevo, col='blue')


plot(pp$CHORRILLOS,col='blue')
plot(pp$MINA.COLQUI, col='blue', ylab= 'Pp(mm)', type='l',
     main='Serie de Pp estacion Ticlio')

extraido <- pp[1:4]

View(extraido)

write.csv(extraido, '../Salida/Pp_extraida_practica01.csv')


pp2 <- read.csv('../Salida/Pp_extraida_practica01.csv', header = T, sep = ",")
View(pp2)

nuevo_2 <- pp2[2:5]
plot(nuevo_2, col='blue')



library(xts) #para manejar series de tiempo

data <- as.xts(read.zoo("Pre_practica.csv", header = TRUE ,sep = ",",
                               format = "%d/%m/%Y",check.names = FALSE))

plot.zoo(as.zoo(data), col='blue', main= 'Series de Tiempo', xlab='Años')
plot.zoo(as.zoo(data), col='blue', main= 'Series de Tiempo', xlab='Años', type = 'p')

library(lattice)

xyplot(data,xlab = "Fecha",ylab = "Precipitación [mm/dia]",ylim=c(0,500))

boxplot(coredata(data), col='blue', main= 'Boxplots')

hist(coredata(data$TINGO), freq = T, col='blue')

data.anual <- apply.yearly(data, FUN = apply, 2, sum)

xyplot(data.anual, xlab= 'Años', ylab='Precipitación(mm)', col='blue', type='l')

plot.zoo(as.zoo(data.anual), col='blue', main= 'Series de Tiempo', xlab='Años')

