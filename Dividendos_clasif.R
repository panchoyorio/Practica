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
library(dplyr)

###Se empieza a trabajar con la base que contiene P*Q, y que posee clasificaciones
#por nombre de empresa

#file.choose()
ruta_excel <-  "C:\\Users\\fcoyo\\Documents\\Dividendos_con_PXQ - recuperado.xlsx"
Dividendos_clasif <- read_excel(ruta_excel,
                         sheet = "BASE COMPLETA")
##Eliminamos las ultimas 4 columnas que vienen vacias 
Dividendos_clasif <- Dividendos_clasif[ ,-19]
Dividendos_clasif <- Dividendos_clasif[ ,-18]
Dividendos_clasif <- Dividendos_clasif[ ,-17]
Dividendos_clasif <- Dividendos_clasif[ ,-16]

##Contamos NA en base
sapply(Dividendos_clasif, function(Dividendos_clasif) sum(is.na(Dividendos_clasif)))

##Omitimos aquellas acciones con tot_acc = 0 
##Se pospone el paso hasta saber si usar otra columna para P*Q

##Se realizan bloxpot para cada sector empresarial

boxplot(Dividendos_clasif$tot_acc ~ Dividendos_clasif$Clasificación, main = "Boxplot de cada sector")


##Boxplot individuales 
tot_acc_ali <- Dividendos_clasif[Dividendos_clasif$Clasificación == "Alimentos, bebidas y tabaco", c("tot_acc")]
tot_acc_ali

boxplot(tot_acc_ali, main = "Alimentos, bebidas y tabaco")

tot_acc_cfi <- Dividendos_clasif[Dividendos_clasif$Clasificación == "CFI", c("tot_acc")]
tot_acc_cfi

boxplot(tot_acc_cfi, main="CFI")

tot_acc_cong <- Dividendos_clasif[Dividendos_clasif$Clasificación == "Conglomerados", c("tot_acc")]
tot_acc_cong

boxplot(tot_acc_cong, main="Conglomerados")

tot_acc_const <- Dividendos_clasif[Dividendos_clasif$Clasificación == "Construcción", c("tot_acc")]
tot_acc_const

boxplot(tot_acc_const, main="Construcción")

tot_acc_consu <- Dividendos_clasif[Dividendos_clasif$Clasificación == "Consumo", c("tot_acc")]
tot_acc_consu

boxplot(tot_acc_consu, main="Consumo")

tot_acc_elec <- Dividendos_clasif[Dividendos_clasif$Clasificación == "Eléctrico", c("tot_acc")]
tot_acc_elec

boxplot(tot_acc_elec, main="Eléctrico") 

tot_acc_ener <- Dividendos_clasif[Dividendos_clasif$Clasificación == "Energía", c("tot_acc")]
tot_acc_ener

boxplot(tot_acc_ener, main="Energía") 

tot_acc_ETF <- Dividendos_clasif[Dividendos_clasif$Clasificación == "ETF", c("tot_acc")]
tot_acc_ETF

boxplot(tot_acc_ETF, main="ETF")

tot_acc_fore <- Dividendos_clasif[Dividendos_clasif$Clasificación == "Forestal", c("tot_acc")]
tot_acc_fore

boxplot(tot_acc_fore, main="Forestal") 

tot_acc_mine <- Dividendos_clasif[Dividendos_clasif$Clasificación == "Minería", c("tot_acc")]
tot_acc_mine

boxplot(tot_acc_mine, main="Minería")

tot_acc_otros <- Dividendos_clasif[Dividendos_clasif$Clasificación == "Otros", c("tot_acc")]
tot_acc_otros

boxplot(tot_acc_otros, main="Otros")

tot_acc_rec <- Dividendos_clasif[Dividendos_clasif$Clasificación == "Recreación y servicios educacionales", c("tot_acc")]
tot_acc_rec

boxplot(tot_acc_rec, main="Recreación y servicios educacionales")

tot_acc_ssal <- Dividendos_clasif[Dividendos_clasif$Clasificación == "Servicios de salud", c("tot_acc")]
tot_acc_ssal

boxplot(tot_acc_ssal, main = "Servicios de salud")

tot_acc_sfin <- Dividendos_clasif[Dividendos_clasif$Clasificación == "Servicios financieros", c("tot_acc")]
tot_acc_sfin

boxplot(tot_acc_sfin, main = "Servicios financieros")

tot_acc_ssan <- Dividendos_clasif[Dividendos_clasif$Clasificación == "Servicios sanitarios y gas", c("tot_acc")]
tot_acc_ssan

boxplot(tot_acc_ssan, main = "Servicios sanitarios y gas")

tot_acc_tele <- Dividendos_clasif[Dividendos_clasif$Clasificación == "Telecomunicaciones", c("tot_acc")]
tot_acc_tele

boxplot(tot_acc_tele, main = "Telecomunicaciones")

tot_acc_tferr <- Dividendos_clasif[Dividendos_clasif$Clasificación == "Transportes de ferrocarriles y por carreteras", c("tot_acc")]
tot_acc_tferr

boxplot(tot_acc_tferr, main = "Transportes de ferrocarriles y por carreteras")



tot_acc_tmar <- Dividendos_clasif[Dividendos_clasif$Clasificación == "Transportes y servicios marítimos", c("tot_acc")]
tot_acc_tmar

boxplot(tot_acc_tmar, main = "Transportes y servicios marítimos")

###Cuantos hay por sector 

data.frame(table(Dividendos_clasif$Clasificación))

### Subdivición según moneda 
boxplot(Dividendos_clasif$tot_acc ~ Dividendos_clasif$moneda, main = "Dividendos, por moneda")
tot_acc_CLP <- Dividendos_clasif[Dividendos_clasif$moneda == "$", c("tot_acc")]
tot_acc_CLP

boxplot(tot_acc_CLP, main = "Dividendos, en pesos")

tot_acc_USD <- Dividendos_clasif[Dividendos_clasif$moneda == "US$", c("tot_acc")]
tot_acc_USD

boxplot(tot_acc_USD, main = "Dividendos, en dólares")

tot_acc_EU <- Dividendos_clasif[Dividendos_clasif$moneda == "EURO", c("tot_acc")]
tot_acc_EU

boxplot(tot_acc_EU, main = "Dividendos, en euros")

###Cuantos hay por moneda

data.frame(table(Dividendos_clasif$moneda))

#Experimentando con las fechas

CLP_USD <- getFX( "USD/CLP", from = Sys.Date() - 360*2  , auto.assign = F ) 
CLP_EUR <- getFX( "EUR/CLP", from = Sys.Date() - 360*2  , auto.assign = F ) 

valorUSD <- last(CLP_USD)
valorEUR <- last(CLP_EUR)

#Dividendos_clasif %>% 
#  filter(moneda  == "US$")

#filter(Dividendos_clasif, moneda == "US$")

Dividendos_clasif %>%
  mutate(tot_acc_pesos = ifelse(moneda == "US$", tot_acc*valorUSD),
                    ifelse(moneda == "EURO", tot_acc*valorEUR,
                           ifelse(moneda == "$", tot_acc*1, NA)))



Dividendos_clasif <- Dividendos_clasif %>% mutate(tot_acc_conv = case_when(moneda == "US$" ~ tot_acc*820.0848,
                            moneda == "EURO" ~ tot_acc*948.5658,
                            moneda == "$" ~ tot_acc*1,
                            TRUE ~ NA_real_))


valorUSD
valorEUR


