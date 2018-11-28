# Leemos los datos crudos 
datos_crudos <- read_csv("Datos/Datos_SemiLimpios.csv")

# Resumen de datos
summary(datos_crudos)

# Quitar las últimas dos obs porque no las tenemos
# Quitamos BBDXY_Index, DXY_Index
# Pasamos produccion OPEC a millones 
datos_limpios <- datos_crudos[1:(nrow(datos_crudos)-2),] %>% 
  select(-BBDXY_Index, - DXY_Index)%>%
  mutate(OPEP_TOTPROD=OPEP_TOTPROD/1000)

summary(datos_limpios)

# Datos originales (sin transformar)
datos_1<-datos_limpios

# Datos con variable dependiente transformada
datos_2<-datos_limpios%>%
  mutate(WTI=log(WTI))

# Datos con variable dependiente y variables independientes transformadas
datos_3<-datos_limpios%>%
  mutate(WTI=log(WTI),
         OPEP_TOTPROD=log(OPEP_TOTPROD),
         OPEP_TOTDEM=log(OPEP_TOTDEM))


# Estandarizamos los datos
datos_1_estand<-datos_1%>%
  select(-Fecha)%>%
  scale()%>%
  as_data_frame()%>%
  mutate(Fecha=datos_limpios$Fecha)%>%
  select(Fecha,everything())

datos_2_estand<-datos_2%>%
  select(-Fecha)%>%
  scale()%>%
  as_data_frame()%>%
  mutate(Fecha=datos_limpios$Fecha)%>%
  select(Fecha,everything())

datos_3_estand<-datos_3%>%
  select(-Fecha)%>%
  scale()%>%
  as_data_frame()%>%
  mutate(Fecha=datos_limpios$Fecha)%>%
  select(Fecha,everything())

# Guardaos los datos sin estandarizar
write.csv(datos_1,'datos/datos_1.csv',row.names=FALSE)
write.csv(datos_2,'datos/datos_2.csv',row.names=FALSE)
write.csv(datos_3,'datos/datos_3.csv',row.names=FALSE)

# Guardamos los datos estandarizados
write.csv(datos_1_estand,'datos/datos_1_estand.csv',row.names = FALSE)
write.csv(datos_2_estand,'datos/datos_2_estand.csv',row.names = FALSE)
write.csv(datos_3_estand,'datos/datos_3_estand.csv',row.names = FALSE)