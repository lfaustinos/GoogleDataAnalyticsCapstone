#Analicemos las posiciones del GPS
GPSdf <- subset(cyclisticdfe, select = c(start_lat,start_lng,end_lat,end_lng,member_casual,month,day,day_of_week))
GPSdf <- na.omit(GPSdf)
summary(GPSdf)
summary(cyclisticdfe)
#Tomando en cuenta que la mayor distancia lineal en Chicago es de aproximadamente 50km y suponiendo que cada grado lat o lng equivale a 111km, los datos no deben estar a más de un cuarto de grado de la mediana
#Veamos cuantos datos salen de este criterio
sum(GPSdf$start_lat< (41.90 -0.25))#123
sum(GPSdf$start_lat> (41.90 +0.25))#1
sum(GPSdf$end_lat< (41.90 -0.25))#108
sum(GPSdf$end_lat> (41.90 +0.25))#3
sum(GPSdf$start_lng< (-87.64 -0.25))#0
sum(GPSdf$start_lng> (-87.64 +0.25))#1
sum(GPSdf$end_lng< (-87.64 -0.25))#7
sum(GPSdf$end_lng> (-87.64 +0.25))#12
#Como son pocos, borraremos estos datos
GPSdf <- GPSdf[GPSdf$start_lat> (41.90 -0.25), ]
GPSdf <- GPSdf[GPSdf$start_lat< (41.90 +0.25), ]
GPSdf <- GPSdf[GPSdf$end_lat> (41.90 -0.25), ]
GPSdf <- GPSdf[GPSdf$end_lat< (41.90 +0.25), ]
GPSdf <- GPSdf[GPSdf$start_lng> (-87.64 -0.25), ]
GPSdf <- GPSdf[GPSdf$start_lng< (-87.64 +0.25), ]
GPSdf <- GPSdf[GPSdf$end_lng> (-87.64 -0.25), ]
GPSdf <- GPSdf[GPSdf$end_lng< (-87.64 +0.25), ]
#veamos de nuevo el resumen de los datos
summary(GPSdf)
hist(GPSdf$start_lat,breaks=42)#Las barras representan aproximadamente 1km. Hay una acumulación entre 41.88 a 41.90
hist(GPSdf$end_lat,breaks=48)#Las barras representan aproximadamente 1km. Hay una acumulación entre 41.88 a 41.90
hist(GPSdf$start_lng,breaks=32)#Las barras representan aproximadamente 1km. Hay una acumulación entre -87.65 a -87.62
hist(GPSdf$end_lng,breaks=38)#Las barras representan aproximadamente 1km. Hay una acumulación entre -87.65 a -87.62
#Contemos los viajes en estos intervalos
sum(GPSdf$start_lat<41.90 & GPSdf$start_lat>41.88)#1478801 de 5661471 poco mas del 26%
sum(GPSdf$start_lng<(-87.62) & GPSdf$start_lng>(-87.65))#2664408 de 5661471 poco mas del 47%
sum(GPSdf$end_lat<41.90 & GPSdf$end_lat>41.88)#1464204 de 5661471 poco mas del 26%
sum(GPSdf$end_lng<(-87.62) & GPSdf$end_lng>(-87.65))#2642145 de 5661471 poco mas del 47%
#Los datos anterioresrevelan que debemos ser más cuidadosos en la equivalencia de la longitud en la latitud que estamos.Veamos a cuanto equivale un grado lng a 41.90 lat
a = (sin(0/2))^2+cos(41.90*pi/180)*cos(41.90*pi/180)*(sin((1/2)*pi/180))^2
d = 6371000*2*atan(sqrt(a)/sqrt(1-a)) #82763 m por lo tanto 0,01grados equivalen a 827.63 m
#Lo anterior implica que 47% de los viajes se acumulan en una banda de aproximadamente 2.5km entre -87.65 y -87.62 grados longitud

#Aumentemos dos km a la latitud
sum(GPSdf$start_lat<41.91 & GPSdf$start_lat>41.87)#2419989 de 5661471 poco mas del 42%
#Apliquemos las dos condiciones a los datos
sum(GPSdf$start_lat<41.91 & GPSdf$start_lat>41.87& GPSdf$start_lng<(-87.62) & GPSdf$start_lng>(-87.65))# 1587048 de 5661471 28%
sum(GPSdf$end_lat<41.91 & GPSdf$end_lat>41.87& GPSdf$end_lng<(-87.62) & GPSdf$end_lng>(-87.65))# 1557770 de 5661471 27.5%
#Aumentemos un 2 km de longitud
sum(GPSdf$start_lat<41.91 & GPSdf$start_lat>41.87& GPSdf$start_lng<(-87.61) & GPSdf$start_lng>(-87.66))# 2021887 de 5661471 35%
sum(GPSdf$end_lat<41.91 & GPSdf$end_lat>41.87& GPSdf$end_lng<(-87.61) & GPSdf$end_lng>(-87.66))# 1991715 de 5661471 35% Ya son mas de la tercera parte
#El centro de estos datos es 41.89 lat y -87.635 W. Hubbart ST entre N,franklin St y N. Well St.
#Veamos si hay un diferencia entre member y casual
GPSdf_casual <- subset(GPSdf, member_casual == "casual")
GPSdf_member <- subset(GPSdf, member_casual == "member")
sum(GPSdf_casual$start_lat<41.91 & GPSdf_casual$start_lat>41.87& GPSdf_casual$start_lng<(-87.61) & GPSdf_casual$start_lng>(-87.66))# 819166 de 2316599 35%
sum(GPSdf_casual$end_lat<41.91 & GPSdf_casual$end_lat>41.87& GPSdf_casual$end_lng<(-87.61) & GPSdf_casual$end_lng>(-87.66))# 800371 de 2316599 35% 
sum(GPSdf_casual$start_lat<41.91 & GPSdf_casual$start_lat>41.87& GPSdf_casual$start_lng<(-87.61) & GPSdf_casual$start_lng>(-87.66))# 819166 de 2316599 35%
sum(GPSdf_casual$end_lat<41.91 & GPSdf_casual$end_lat>41.87& GPSdf_casual$end_lng<(-87.61) & GPSdf_casual$end_lng>(-87.66))# 800371 de 2316599 35% 
#se preservan las proporciones
sum(GPSdf_member$start_lat<41.91 & GPSdf_member$start_lat>41.87& GPSdf_member$start_lng<(-87.61) & GPSdf_member$start_lng>(-87.66))# 1202721 de 3344872 35%
sum(GPSdf_member$end_lat<41.91 & GPSdf_member$end_lat>41.87& GPSdf_member$end_lng<(-87.61) & GPSdf_member$end_lng>(-87.66))# 1191344 de 3344872 35% 
sum(GPSdf_member$start_lat<41.91 & GPSdf_member$start_lat>41.87& GPSdf_member$start_lng<(-87.61) & GPSdf_member$start_lng>(-87.66))# 1202721 de 3344872 35%
sum(GPSdf_member$end_lat<41.91 & GPSdf_member$end_lat>41.87& GPSdf_member$end_lng<(-87.61) & GPSdf_member$end_lng>(-87.66))# 1191344 de 3344872 35% 
#se preservan las proporciones
#No hay diferencias entre member y casual en esta zona

summary(GPSdf_casual)
summary(GPSdf_member)
#No hay diferencias entre la distribución de los datos

sum(GPSdf$start_lat == 41.89 & GPSdf$start_lng == -87.63)
sum(GPSdf$start_lat == 41.89 & GPSdf$start_lng == -87.64)
which(cyclisticdfe$start_lat == 41.89 & cyclisticdfe$start_lng == -87.63)#2419 Al parecer esta estación no tiene nombre
which(cyclisticdfe$start_lat == 41.89 & cyclisticdfe$start_lng == -87.64)#824879 Al parecer esta estacion tampoco tiene nombre

cyclisticdfe_mystery <- subset(cyclisticdfe, start_lat == 41.89 & start_lng == -87.63)#No es vacía
cyclisticdfe_mystery <- na.omit(cyclisticdfe_mystery)#Quedó vacía
cyclisticdfe_mystery <- subset(cyclisticdfe, start_lat == 41.89 & start_lng == -87.64)#No es vacía
cyclisticdfe_mystery <- na.omit(cyclisticdfe_mystery)#Quedó vacía
#Las estaciones que podrian ser el centro de acumulación de la tercera parte de los datos no tiene nombre
cyclisticdfe_mystery <- subset(cyclisticdfe, start_lat == 41.89 & start_lng == -87.65)
cyclisticdfe_mystery <- na.omit(cyclisticdfe_mystery)#N Green St & W Lake St
view(cyclisticdfe_mystery)#N Green St & W Lake St

cyclisticdfe_mystery <- subset(cyclisticdfe, start_lat == 41.89 & start_lng == -87.62)#No es vacía
cyclisticdfe_mystery <- na.omit(cyclisticdfe_mystery)#Quedó vacía

cyclisticdfe_mystery <- subset(cyclisticdfe, start_lat == 41.88 & start_lng == -87.63)#No es vacía
cyclisticdfe_mystery <- na.omit(cyclisticdfe_mystery)#Quedó vacía

cyclisticdfe_mystery <- subset(cyclisticdfe, start_lat == 41.90 & start_lng == -87.63)#No es vacía
cyclisticdfe_mystery <- na.omit(cyclisticdfe_mystery)#N Clark St & W Elm St
view(cyclisticdfe_mistery)#N Clark St & W Elm St
#La conclusion es que no podemos confiar en que la acumulación de los viajes sea real, se debe preguntar a la empresa sobre la ubicación de sus estaciones para seguir anaizando los datos.

sum(GPSdf$start_lat<41.93 & GPSdf$start_lat>41.85& GPSdf$start_lng<(-87.62) & GPSdf$start_lng>(-87.65))# 1920228 de 5661471 33%
sum(GPSdf$end_lat<41.93 & GPSdf$end_lat>41.85& GPSdf$end_lng<(-87.62) & GPSdf$end_lng>(-87.65))# 2186876de 5661471 38%


rm(dfcyclistic)



