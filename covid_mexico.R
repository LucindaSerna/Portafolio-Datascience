#Análisis rápido de datos sobre Covis-19 en México
library(dplyr) 
library(readxl) r
library(readr)
library(ggplot2)

# Importamos el data set aquí está directo de mis archivos pero el repositorio está en Github
covid_dataset <-covid_dataset_covid_dataset <-read_csv("C:/Users/Lucinda/OneDrive/Documentos/R-udemy/covid_dataset.csv")
View(covid_dataset)

# Extraigo una muestra aleatoria de 100k registros y le asigno  una nueva variable. A partir de ahora trabajaré con este dataset
covid_df<-sample_n(covid_dataset, 100000)
View(covid_df)

# Se  hace un resumen estadistico del dataset y tambien  se muestra los tipos de datos por columna
#primero  sacó resumen estadísstico  del df 
summary(covid_df)
#tipo de datos de cada columna
str(covid_df) #La Mayor?a de los datos son num?ricos y algunos
#de texto

# Se diltran los renglones que dieron positivo para SARS-COVID y calcula el numero de registros
# Los casos positivos son aquellos que en la columna CLASIFICACION_FINAL tienen 1, 2 o 3
covid_df %>% 
  select(CLASIFICACION_FINAL)%>%
  filter( CLASIFICACION_FINAL<=3)
#Nota escog? filtrarlo con <=3  ya que cheque que la columna
# no tuviera valores cero, de esa forma me quedan los valores
#1,2,3


# Se Cuenta el numero de registros nulos por columna
apply(covid_df, MARGIN=2,function(x)sum(is.na(x)))

#En  las columnas de covid_df solo la columna FECHA_DEF tiene valores 
#NA 95044,


#
#Calcula   la media de edades de los contagiados de covid
covid_df%>%
  summarise(promedio=mean(EDAD))
#La edad promedio de los contagiados de Covid es 41.2

#Se realiza un Histograma de las edades de los contagiados
edades<-covid_df$ EDAD
hist(edades)

## Se Realiza una grafica de densidad de edades de los contagiados
V<-ggplot(covid_df,aes(x=EDAD)) #primero creamos un objeto solo con
#la columna EDAD
V+geom_density()
#En efecto el histograma y la gr?fica de densidad de la columna
#EDAD nos demuestran que el calculo de la media es correcto.


# Se va Agregar una columna nueva al dataframe que tenga valor 1 cuando la fecha de defuncion no es valor nulo y 0 cuando es nulo 
## La columna que contiene la fecha de defuncion se llama FECHA_DEF 
covid_df<-covid_df%>%
  mutate(FECHA_DEF,NUEVA_COL=ifelse(is.na(FECHA_DEF)==TRUE, 0,1))

# Se Hará un boxplot de edades de los muertos por covid vs lo que no murieron para ver si detectamos diferencias y escribe tus conclusiones                               
#primero camos agregar una nueva columna que nos diga la defunci?n
# de cada persona. En este caso basandonos en la NUEVA_COL
# si NUEVA_COL =1 entonces marcamos como DEF si NUEVA_COL=0 marcamos como
#NO DEF
covid_df<-covid_df%>%
  mutate(NUEVA_COL,DEFUNCION=ifelse(NUEVA_COL==0, 'NO DEF','DEF'))
#Ahora haremos el boxplot
qplot(x=DEFUNCION,y=EDAD,data=covid_df,geom="boxplot",fill=DEFUNCION)
#Al analizar la gr?fica lo primero que se nota es que  los no difuntos 
#son en media m?s j?venes que los difuntos  la media de unnos es 35-45 y
# de los otros 60. Lo que m?s llama la atenci?n son los valores at?picos
#sobre todo en las Defunciones ya que hay bastantes ente 0 y 25a?os.
#Creo que como conclusi?n podemos decir que hay mayor probabilidad de morir
#si eres mayorse 50


# Se Transforma la columna CLASIFICACION_FINAL, que tenga valor de 1 si tiene 1, 2 o 3 como valor y que tenga 0 en cualquier otro caso
covid_df<-covid_df%>% 
  transform(CLASIFICACION_FINAL=ifelse(CLASIFICACION_FINAL<=3 ,1,0))
covid_df%>%View()
# Se Cuenta el número de casos positivos agrupado por estado y realiza una grafica de barras de los 10 estados con mas casos
covid_estados<-covid_df%>%
  group_by(ENTIDAD_RES)%>%
  summarise(casos=n())
#ordenamos los datos
covid_estados<- covid_estados[order(covid_estados$casos),] 
covid_estados
#Ya los tenemos ordenados de menor a mayor y ahora #
#haremos la gr?fica  con los ?ltimos 10
#covid_estad[23:32,]
ggplot(data=covid_estados[22:32,],aes(x=ENTIDAD_RES,y=casos))+
  geom_bar(stat="identity")
#cada estado viene identificado con un n?mero
#La gr?fica muestra los estado con mayor n?mero de contagios
# es el estado 9  y es muy deproporcional a los dem?s ya que hubo
# m?s de veinte mil contagios de ah? hay un estado con m?s de diez
#mil ocntagios y los dem?s tiene mas o menos el mismo n?mero de contagios


# Se renombra la columna llamada CLASIFICACION FINAL para que ahora su nombre sea: "CONTAGIADO"

covid_df <-covid_df%>%
  rename(CONTEO=CLASIFICACION_FINAL)
View(covid_df)

# Se realiza una funcion que al aplicarla nos diga el procentaje del total de registros que estan contagiados por Covid


porcentaje_covid<-function(Tabla){
  x=(sum(Tabla$CONTEO))
  x_1=x/1000
  paste("El porcentaje de los contagiados es:",x_1)}

porcentaje_covid(covid_df)
#La funci?n toma la columna COMTEO que previamente hab?amos hecho los casos 
#positivos tuviera valor 1, los suma y obtenimos el total de los n?meros 
# de contagio en la variable x. Luego en x_1 calculamos el pocentaje.



#Vamos a hacer un an?lis de la variable diabetes que est? clasificado en 1 y 2.
#Como no pude encontrar en la p?gina de los datos covid  que significaba
#el 1 y 2 haremos el an?lisis como diabetes1  y diabetes2.

#Primero hagamos un conteo de cuantos del sexo_1 y sexo_" hay 
#Para esto haremos los del sexo_2=2 con valo cero
covid_diab<-covid_df%>%transform(DIABETES=ifelse(DIABETES==1,1,0))
diabetes1<-sum(covid_diab$DIABETES)
diabetes2<- 100000-diabetes1
diabetes1
diabetes2
# Entonces de las personas analizadas tenemos que #10983 tienes diabetes1
#y 89017 con diabetes 2
#
#Ahora buscaremos hacer una boxplot
#vamos a  volver a hacer un cambio en covid_diab ahora cambiando a
#diabetes1 si es igual a 1 y diabetes 2 si es igual 2
covid_diab<-covid_df%>%transform(DIABETES=ifelse(DIABETES==1,"diabetes1","diabetes2"))
qplot(x=DIABETES,y=EDAD,data=covid_diab,geom="boxplot",fill=DIABETES)
#La diabetes1 ocurre en u rango de edad mayor al de la dibates 2 y 
#sus caos at?picos ocurren a menor edad. 




