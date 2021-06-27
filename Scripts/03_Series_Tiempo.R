# Leyendo Datos Pisco -----------------------------------------------------
rm(list = ls())

pacman::p_load(raster, ncdf4, tseries, tidyverse, ggplot2,
               ggthemes, ggpubr)

setwd('D:/Taller_Nexus/')

Pp_pisco <- read.csv('Salida/data_pp.csv', header = T, sep = ",") 

str(Pp_pisco)

View(Pp_pisco)

Pp_pisco$Fecha<- as.Date(Pp_pisco$Fecha)

str(Pp_pisco)

#1ER CASO

Tingo <- plot(Pp_pisco$Fecha,Pp_pisco$TINGO, type = "l", col= 'blue',
              main= 'Serie de Tiempo Tingo', xlab= 'Años', ylab= 'Estacion')

Chorrillos <- plot(Pp_pisco$Fecha, Pp_pisco$CHORRILLOS, type = "l", col= 'blue',
               main= 'Serie de Tiempo Chorrillos', xlab= 'Años', ylab= 'Estacion')

par(mfrow=c(2,2))

lince <- plot(Pp_pisco$Fecha,Pp_pisco$ACOBAMBA, type = "l", col= 'blue',
              main= 'Serie de Tiempo Lince', xlab= 'Años', ylab= 'Pp')

rimac <- plot(Pp_pisco$Fecha, Pp_pisco$MINA.COLQUI, type = "l", col= 'blue',
               main= 'Serie de Tiempo Rimac', xlab= 'Años', ylab= 'Pp')

santa_eulalia <- plot(Pp_pisco$Fecha, Pp_pisco$PUCRO, type = "l", col= 'blue',
                      main= 'Serie de Tiempo Santa Eulalia', xlab= 'Años', ylab= 'Pp')

blanco <- plot(Pp_pisco$Fecha, Pp_pisco$SHEQUE, type = "l", col= 'blue',
               main= 'Serie de Tiempo Rio Blanco', xlab= 'Años', ylab= 'Pp')


# 2DO CASO
###Puntos
colnames(Pp_pisco)
milloc <- ggplot(Pp_pisco, aes(x = Pp_pisco$Fecha, y = Pp_pisco$CHORRILLOS )) + 
  geom_point(aes(colour='red'))

ticlio <- ggplot(Pp_pisco, aes(x = Fecha, y = CHORRILLOS )) + 
  geom_point(aes(colour='blue'))


ggplot(Pp_pisco, aes(Fecha, ACOBAMBA)) + 
  geom_point() + 
  geom_smooth(span = 0.4)

#Poligonos de frecuencia

ggplot(Pp_pisco, aes(ACOBAMBA)) + geom_histogram(colour= 'blue')

#Frecuencia

ggplot(Pp_pisco, aes(CHORRILLOS)) + geom_freqpoly(colour= 'blue')


ggplot(Pp_pisco, aes(Fecha, ACOBAMBA)) + 
  geom_path(colour = "grey50") 


#GGPLOT LINEAS

colnames(Pp_pisco)
summary(Pp_pisco$SHEQUE)

plt1 <- ggplot(Pp_pisco, aes(Fecha, SHEQUE)) +
  geom_line(color = 'blue') +
  theme_bw() +
  theme_solarized() +
  scale_x_date(date_breaks = "1 year", date_labels = "%y", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0,350, 50))

summary(Pp_pisco$TINGO)

plt2 <- ggplot(Pp_pisco, aes(Fecha, TINGO)) +
  geom_line() +
  theme_bw() +
  theme_solarized() +
  scale_x_date(date_breaks = "1 year", date_labels = "%y", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0,320, 50), expand = c(0, 0))

plt <- ggarrange(plt1, plt2, ncol = 1, nrow = 2)

plt
#PONGAMONOS MAS SERIOSSSSSS

#Transformamos los datos en una serie temporal 

serie <- ts(Pp_pisco[2:9], start = c(1981/01/01) ,frequency = 12)

print(serie)
plot.ts(serie, type= "p", main= "Serie de Tiempo Rímac", col= "blue")
plot.ts(serie, type= "l", main= "Serie de Tiempo Rímac", col= "red")


library(xts) #para manejar series de tiempo

data <- as.xts(read.zoo("Insumos/Pre_practica.csv", header = TRUE ,sep = ",",
                        format = "%d/%m/%Y",check.names = FALSE))

plot(data)

plot(data[,1:5], type="l")

plot.zoo(as.zoo(data), col='blue', main= 'Series de Tiempo', xlab='Años')


library(lattice)

xyplot(data ,xlab = "Fecha",ylab = "Precipitación [mm/dia]",ylim=c(0,500))

data_anual <- apply.yearly(data,  FUN=apply, 2, sum)

xyplot(data_anual)

#####YAAAAA TERMINAMOS CON LA FEEEEE

# Analisis exploratorio con hydroTSM

library(hydroTSM)

hydroplot(as.zoo(data[,1]), var.type="Precipitation", pfreq = "dma", ylab = "Prec")
