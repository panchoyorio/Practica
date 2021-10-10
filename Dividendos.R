
library(readxl)
library(openxlsx)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(car)
library(urca)
library(tseries)
library(astsa)
library(forecast)
library(foreign)
library(timsac)
library(vars)
library(mFilter)
library(dynlm)
library(nlme)
library(strucchange)
library(quantmod)
library(xts)
library(Hmisc)

#file.choose()
ruta_excel <-   "C:\\Users\\fcoyo\\Downloads\\Dividendos_2015_2021.xlsx"
Dividendos <- read_excel(ruta_excel,
                         sheet = "Dividendos_2015_2021 (2)")
##Contar NA en base
sapply(Dividendos, function(Dividendos) sum(is.na(Dividendos)))

#Se reemplazan todos los NA de la columna moneda por "$"
Dividendos$moneda[is.na(Dividendos$moneda)] <- "$"

##Contar NA en base
sapply(Dividendos, function(Dividendos) sum(is.na(Dividendos)))


## Se omiten todos los datos con NA
Dividendos <- na.omit(Dividendos)

str(Dividendos$fec_pago)

##Comprobando si quedan NA
sapply(Dividendos, function(Dividendos) sum(is.na(Dividendos)))

boxplot(Dividendos$val_acc)

plot(Dividendos$val_acc)

max (Dividendos$val_acc)

#order(Dividendos$val_acc)

#order(Dividendos$val_acc, 
#      decreasing = TRUE, 
#      na.last = TRUE,   
#      method = c("auto", "shell", "radix"))
#Dividendos[Dividendos$val_acc > , ] # Atención a la coma y el espacio al final

  

#fec_pago = Dividendos2[,3]

#https://rpubs.com/Cesar_AHN/deteccion-eliminacion-valores-outliers-en-r-boxplot

Dividendos_boxplot <- boxplot(Dividendos$val_acc, col = "skyblue", frame.plot=T)

Dividendos_boxplot$out

#summarise(Dividendos$val_acc)

val_acc = Dividendos[ ,11]

summary(val_acc)


# Convertir la variable numerica "val_acc" en categorica
# para ello definimos los puntos de corte
breakPoints <- c(0, 1, 10, 100, Inf)
categories <- c("Low", "Medium", "High", "Very High") 

# y cortamos la variable vall_Acc segun esta categorizacion
Dividendos$val_acc.F <- cut(Dividendos$val_acc, breaks = breakPoints, labels = categories)

#A lo mejor no hemos definido bien los puntos de corte. 
#Vamos a obtener una serie de medidas estadísticas sobre la 
#variable:
  
summary(Dividendos$val_acc)

# Histograma valor acciones
hist(Dividendos$val_acc, main="Valor acciones")

#Rango <- range(0.001,1000)
#Rango
#Aplitud <- diff(Rango)

sapply(Dividendos, function(Dividendos) sum(is.na(Dividendos)))
## Se omiten todos los datos con NA
Dividendos <- na.omit(Dividendos)

Dividendos2 <- Dividendos %>% mutate(tot_acc = val_acc * num_acc_der)

summary(Dividendos2$tot_acc)
boxplot(Dividendos2$tot_acc)

#write.xlsx(Dividendos2, "Dividendos_con_PXQ.xlsx")


# quitamos los outliers del limite superior

breakPoints <- c(999, 100000, 10000000, 100000000000, Inf)
categories <- c("Low", "Medium", "High", "Very High") 

# y cortamos la variable tot_acc segun esta categorizacion
Dividendos2$tot_acc.F <- cut(Dividendos2$tot_acc, breaks = breakPoints, labels = categories)

sapply(Dividendos2, function(Dividendos2) sum(is.na(Dividendos2)))

#Para ver la cantidad de valores low, medium, high y very high
data.frame(table(Dividendos2$tot_acc.F))

#omitimos NA
Dividendos2 <- na.omit(Dividendos2)



tot_acc <- Dividendos2[Dividendos2$tot_acc < 100000000000, c("tot_acc")]

count(Dividendos2, "tot_acc")


# y al hacer un summary de la variable vemos que los valores han cambiado
summary(tot_acc)

###Veamos un boxplot
boxplot(tot_acc)
plot(tot_acc)

#Nos fijamos en una empresa (nemo) en particular
#val_acc_CFIVPE4E_E <- Dividendos[Dividendos$nemo == "CFIVPE4E-E", c("val_acc")]
#val_acc_CFIVPE4E_E

#summary(val_acc_CFIVPE4E_E$val_acc)
##Hacemos el diagrama de bigotes
#boxplot(val_acc_CFIVPE4E_E)

##Lo interesante sería poder dibujar este gráfico de caja y bigotes para cada
##usuario. Se puede hacer poniendo la variable «pasos» en función de la variable
#«usuario».
boxplot(Dividendos2$tot_acc ~ Dividendos2$nemo)


##Si ejecuto el sig codigo, se eliminan 2000 datos
#Dividendos2<-Dividendos2[!(Dividendos2$tot_acc %in% Dividendos2_boxplot$out),]

boxplot(Dividendos2$tot_acc, col = "skyblue", frame.plot=F)

Dividendos2_boxplot <- boxplot(Dividendos2$val_acc, col = "skyblue", frame.plot=T)

Dividendos2_boxplot

outliersReplace <- function(data, lowLimit, highLimit){
  data[data < lowLimit] <- mean(data)
  data[data > highLimit] <- median(data)
  data     #devolvemos el dato       
}

#tot_acc_2 <- outliersReplace(tot_acc, 999, 100000000000)

str(tot_acc)
boxplot(Dividendos2$fec_lim)

#Dividendos_ts = ts(Dividendos2, start = 2015, frequency = 365)
#Dividendos_ts
###Tratar la base como indice por mientras, despues le especificamos que es serie de tiempo
###Reemplazar valores NA en moneda por $
###utilizar algun comando ya sea de r o de excel para que se entreguen juntos los valores de la misma fecha
###Encontrar los maximos para eliminarlos (outliers)


##Subdividir gráficos por moneda

boxplot(Dividendos2$tot_acc ~ Dividendos2$moneda)
tot_acc_CLP <- Dividendos2[Dividendos2$moneda == "$", c("tot_acc")]
tot_acc_CLP

boxplot(tot_acc_CLP)

tot_acc_USD <- Dividendos2[Dividendos2$moneda == "US$", c("tot_acc")]
tot_acc_USD

boxplot(tot_acc_USD)

tot_acc_EU <- Dividendos2[Dividendos2$moneda == "EURO", c("tot_acc")]
tot_acc_EU

boxplot(tot_acc_EU)


#plot(Dividendos2$tot_acc ~ Dividendos2$moneda)
plot(tot_acc_CLP)
plot(tot_acc_USD)
plot(tot_acc_EU)

summary(tot_acc_CLP)
summary(tot_acc_USD)
summary(tot_acc_EU)

##Se crean columnas con las conversiones a moneda nacional
#Valor dolar y euro deben ser actualizados con regularidad (buscar comando)
val_USD = 803.26 

Dividendos2 <- Dividendos2 %>% mutate( conversion_USD = tot_acc * val_USD)

val_EURO = 931.30

Dividendos2 <- Dividendos2 %>% mutate( conversion_EURO = tot_acc * val_EURO)

OUTLIERS <- Dividendos2_boxplot$out

