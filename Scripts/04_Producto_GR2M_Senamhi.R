# This script show how to read the PISCO_HyM_GR2M_v1.1.nc file
# By Harold Llauca
# install.packages("ncdf4")
# install.packages("lubridate")
rm(list=ls())
cat('\f')

# Load required packages
require(ncdf4)
require(lubridate)

# Set your work directory
my_location <- 'D:/Taller_Nexus/'
setwd(my_location)

# Read netCDF data and load variables and dimensions
#==========================================================
mync   <- nc_open('Grillados/netcdf_GR2M/PISCO_HyM_GR2M_v1.1.nc')
print(mync)

# Load data
comid <- ncvar_get(mync,'comid')
time  <- floor_date(as.Date('1960-01-01')+
                      months(floor(ncvar_get(mync,'time'))),"month")
pr <- as.data.frame(t(ncvar_get(mync,'pr'))) # Precipitation
ae <- as.data.frame(t(ncvar_get(mync,'ae'))) # Actual evapotranspiration
sm <- as.data.frame(t(ncvar_get(mync,'sm'))) # Soil Moisture
ru <- as.data.frame(t(ncvar_get(mync,'ru'))) # Runoff
qr <- as.data.frame(t(ncvar_get(mync,'qr'))) # Discharge

  # Save as texfiles
  colnames(pr) <- paste0('PR_',comid)
  colnames(ae) <- paste0('AE_',comid)
  colnames(sm) <- paste0('SM_',comid)
  colnames(ru) <- paste0('RU_',comid)
  colnames(qr) <- paste0('QR_',comid)
  rownames(pr) <- time
  rownames(ae) <- time
  rownames(sm) <- time
  rownames(ru) <- time
  rownames(qr) <- time
  # write.table(pr, 'pr.txt',sep='\t')
  # write.table(ae, 'ae.txt',sep='\t')
  # write.table(sm, 'sm.txt',sep='\t')
  # write.table(ru, 'ru.txt',sep='\t')
  # write.table(qr, 'qr.txt',sep='\t')


# Plot data for an specific COMID
#==========================================================
myCOMID <- 20 # VER EL COMID EN LA TABLA DE ATRIBUTOS DEL SHAPEFILE, SEGUN ESO
  # ES EL CAUDAL DE LA CUENCA , POR EJEMPLO SI EL COMID DE LA CUENCA SANTA EULALIA
  #EN EL SHAPEFILE ES 25 , PONER 25 AQUI, Y NOS ARROJARA EL CAUDAL DE ESA CUENCA.
  #ESTE ES EL PRODUCTO DESARROLLADO POR EL SENAMHI
  
  par(mfrow=c(3,2))
  par(mar=c(1,3,3,1), oma=c(0.5,0.5,0.5,0.5))
  par(cex=0.6)
  par(tck=-0.02)
  par(mgp=c(1.5,0.5,0))
  
  ind <- which(comid==myCOMID)
  
  plot(x=time, y=pr[,ind], type='l', col='blue',
       main=paste0('PR_',myCOMID), ylab='[mm/month]')
  
  plot(x=time, y=ae[,ind], type='l', col='green',
       main=paste0('AE_',myCOMID), ylab='[mm/month]')
  
  plot(x=time, y=sm[,ind], type='l', col='red',
       main=paste0('SM_',myCOMID), ylab='[mm/month]')
  
  plot(x=time, y=ru[,ind], type='l', col='black',
       main=paste0('RU_',myCOMID), ylab='[mm/month]')
  
  plot(x=time, y=qr[,ind], type='l', col='magenta',
       main=paste0('QR_',myCOMID), ylab='[m3/s]')


# Adicionalmente ----------------------------------------------------------

#Agregue una funcion para guardarlo como archivo csv en ves de text
  
  write.csv(qr[,ind],'Salida/Caudales_m3_comid.csv', quote = F, row.names = F)
  #Los datos estan hasta marzo del 2020
  #Si la cuenca que desea no esta dentro del shapefile, debe aplicarse de manera
  #manual.
  