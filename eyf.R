rm( list=ls() )  
gc() 

library(tidyverse)
library(lightgbm)
library(mlrMBO)
library(mlr)
library(caret)
library(fastICA)
library(data.table)
library(drifter)

setwd("~/Desktop/DM/1er_Año/DMEyF")

datos_tr <- fread("./datasetsOri/paquete_premium_202009.csv")
datos_te <- fread("./datasetsOri/paquete_premium_202011.csv")

cero_var_tr <- nearZeroVar(datos_tr)
cero_var_tr <- cero_var_tr[-73]

#elimino varibles con varianza cercana a cero.
datos_tr[,cero_var_tr] <- NULL

datos_te[,cero_var_tr] <- NULL

#defino drift de variables, me quedo con las mas altas en funcion aprox del histograma 
drift <- calculate_covariate_drift(datos_tr, datos_te, bins = 20)
drift <- drift %>% filter(drift > 0.2, variables != "clase_ternaria") 

hist(drift$drift)

datos_tr <- datos_tr %>% select(-drift$variables)
datos_te <- datos_te %>% select(-drift$variables)


datos_tr <- datos_tr[, ':=' (edad_antig = cliente_edad*cliente_antiguedad, #interaccion entre antiguedad y edad
                 rentab_t =  mrentabilidad/mrentabilidad_annual, # si la rentabilidad del mes fue menor q la proporcional del año...
                 ing_prop_ten = (mpayroll+mtransferencias_recibidas)/(mcaja_ahorro + (180*mcaja_ahorro_dolares)), #sueldo / tenencias
                 gastos = mcuenta_debitos_automaticos + mcomisiones_mantenimiento + 
                   mcomisiones_otras + ctrx_quarter + Visa_msaldototal + Visa_mconsumototal,# gastos totales
                 ing_prop_gas = (mpayroll+mtransferencias_recibidas)/(mcuenta_debitos_automaticos + mcomisiones_mantenimiento + 
                                                                        mcomisiones_otras + ctrx_quarter + Visa_msaldototal + Visa_mconsumototal))]# ingreso proporcional a gasto total
                  
     
datos_te <- datos_te[, ':=' (edad_antig = cliente_edad*cliente_antiguedad, #interaccion entre antiguedad y edad
                             rentab_t =  mrentabilidad/mrentabilidad_annual, # si la rentabilidad del mes fue menor q la proporcional del año...
                             ing_prop_ten = (mpayroll+mtransferencias_recibidas)/(mcaja_ahorro + (180*mcaja_ahorro_dolares)), #sueldo / tenencias
                             gastos = mcuenta_debitos_automaticos + mcomisiones_mantenimiento + 
                               mcomisiones_otras + ctrx_quarter + Visa_msaldototal + Visa_mconsumototal,# gastos totales
                             ing_prop_gas = (mpayroll+mtransferencias_recibidas)/(mcuenta_debitos_automaticos + mcomisiones_mantenimiento + 
                                                                                    mcomisiones_otras + ctrx_quarter + Visa_msaldototal + Visa_mconsumototal))]# ingreso proporcional a gasto total
                              


fwrite(datos_tr, file = "./dataset/datos_tr.csv")
fwrite(datos_te, file = "./dataset/datos_te.csv")
