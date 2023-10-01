library(tidyverse)

#ARMAMOS LA TABLA DEL 2019
e2019 = read.csv("C:/Users/nicol/OneDrive/Documentos/TP DESCRIPTIVA/e2019/mesas_agrp_politicas.csv")
e2019tot = read.csv("C:/Users/nicol/OneDrive/Documentos/TP DESCRIPTIVA/e2019/mesas_totales.csv")
e2015 =read.csv("C:/Users/nicol/OneDrive/Documentos/TP DESCRIPTIVA/Elecciones generales de presidente y vicepresidente estimados por radio - Argentina (1).csv")
options(scipen = 99999999)
presi = c(000100000000000,000100000000000,000100000000000,000100000000000,000100000000000,000100000000000)
e2019 = e2019 %>% filter(CODIGO_CATEGORIA %in%  presi)
e2019tot = e2019tot %>% filter(CODIGO_CATEGORIA %in%  presi)
e2019god = e2019 %>% group_by(CODIGO_DISTRITO,CODIGO_AGRUPACION) %>%  summarise(cantidad = sum(VOTOS_AGRUPACION)) 
e2019god <- mutate(e2019god, PROVINCIA = case_when(CODIGO_DISTRITO == 1 ~ "Ciudad Autonoma de Buenos Aires", CODIGO_DISTRITO ==2 ~ "Buenos Aires" ,CODIGO_DISTRITO ==3 ~ 'Catamarca', CODIGO_DISTRITO ==4 ~ 'Cordoba', CODIGO_DISTRITO ==5 ~ 'Corrientes', CODIGO_DISTRITO ==6 ~ 'Chaco', CODIGO_DISTRITO ==7 ~ 'Chubut', CODIGO_DISTRITO ==8 ~ 'Entre Rios', CODIGO_DISTRITO ==9~'Formosa', CODIGO_DISTRITO ==10 ~ 'Jujuy',CODIGO_DISTRITO ==11~"La Pampa",CODIGO_DISTRITO ==12~"La Rioja",CODIGO_DISTRITO ==13~"Mendoza",CODIGO_DISTRITO ==14~"Misiones", CODIGO_DISTRITO ==15~"Neuquen", CODIGO_DISTRITO ==16~"Rio Negro",CODIGO_DISTRITO ==17~"Salta",CODIGO_DISTRITO ==18~"San Juan",CODIGO_DISTRITO ==19~"San Luis",CODIGO_DISTRITO ==20~"Santa Cruz", CODIGO_DISTRITO ==21~"Santa Fe",CODIGO_DISTRITO ==22~"Santiago Del Estero",CODIGO_DISTRITO ==23~"Tucuman", CODIGO_DISTRITO ==24~"Tierra Del Fuego"))
e2019god <- mutate(e2019god,PARTIDO = case_when(CODIGO_AGRUPACION == 135 ~ "JUNTOS POR EL CAMBIO",CODIGO_AGRUPACION ==131~ "FRENTE NOS",CODIGO_AGRUPACION ==133~"FRENTE DE IZQUIERDA Y DE TRABAJADORES",CODIGO_AGRUPACION ==136~"FRENTE DE TODOS",CODIGO_AGRUPACION ==137~"CONSENSO FEDERAL",CODIGO_AGRUPACION ==87~"UNITE POR LA LIBERTAD Y LA DIGNIDAD"), cantidad = cantidad)
e2019god$CODIGO_DISTRITO = NULL
e2019god$CODIGO_AGRUPACION=NULL
view(e2019god)

#SE ARMA LA TABLA DE 2015
# aca se arman las minitablas
colnames(e2015)
e2015JPC = e2015 %>% group_by(Nombre.de.provincia) %>% summarise(JPC = sum(Maurio.Macri..Cambiemos.,na.rm = TRUE))
e2015FPV = e2015 %>% group_by(Nombre.de.provincia) %>% summarise(FPV = sum(Daniel.Scioli..FPV.,na.rm = TRUE))
e2015UNA = e2015 %>% group_by(Nombre.de.provincia) %>% summarise(UNA = sum(Sergio.Massa..UNA.,na.rm = TRUE))
e2015FIT = e2015 %>% group_by(Nombre.de.provincia) %>% summarise(FIT = sum(Nicolás.del.Caño..FIT.,na.rm = TRUE))
e2015ROD = e2015 %>% group_by(Nombre.de.provincia) %>% summarise(ROD = sum(A..Rodríguez.Saá..C..Federal.,na.rm = TRUE))
e2015PROG = e2015 %>% group_by(Nombre.de.provincia) %>% summarise(PROG = sum(Margarita.Stolbizer..Progresistas.,na.rm = TRUE))

# aca hay q meter la columna partido en cada minitabla, y cambiar los colnames de todas a esos

e2015PROG = e2015PROG %>% mutate(PARTIDO = 'PROG')
colnames(e2015PROG) <- c('PROVINCIA',"VOTOS","PARTIDO")
e2015ROD = e2015ROD %>% mutate(PARTIDO = 'ROD')
colnames(e2015ROD) <- c('PROVINCIA',"VOTOS","PARTIDO")
e2015FIT = e2015FIT %>% mutate(PARTIDO = 'FIT')
colnames(e2015FIT) <- c('PROVINCIA',"VOTOS","PARTIDO")
e2015UNA = e2015UNA %>% mutate(PARTIDO = 'UNA')
colnames(e2015UNA) <- c('PROVINCIA',"VOTOS","PARTIDO")
e2015FPV = e2015FPV %>% mutate(PARTIDO = 'FPV')
colnames(e2015FPV) <- c('PROVINCIA',"VOTOS","PARTIDO")
e2015JPC = e2015JPC %>% mutate(PARTIDO = 'JPC')
colnames(e2015JPC) <- c('PROVINCIA',"VOTOS","PARTIDO")

#aca se unen los minitablas para armar god
e2015god = union(e2015PROG,e2015ROD)
e2015god = union(e2015god,e2015FIT)
e2015god = union(e2015god,e2015UNA)
e2015god = union(e2015god,e2015FPV)
e2015god = union(e2015god,e2015JPC)
view(e2019god)

#se juntan la tabla del 2015 con la del 2019
e2015god = e2015god %>% mutate(anio = 2015)
e2019god$CODIGO_DISTRITO = NULL
e2019god = e2019god %>% mutate(anio = 2019)
colnames(e2015god) <- c('PROVINCIA',"VOTOS","PARTIDO","ANIO")
colnames(e2019god) <- c('VOTOS',"PROVINCIA","PARTIDO","ANIO")
eDoble = union(e2015god,e2019god)
view(eDoble)
view(e2015god)
view

#cambiar nombres para que sean iguales. 

eDoble$PARTIDO[eDoble$PARTIDO == "FRENTE DE TODOS"] <- 'FDT'
eDoble$PARTIDO[eDoble$PARTIDO == "JUNTOS POR EL CAMBIO"] <- 'JPC'
eDoble$PARTIDO[eDoble$PARTIDO == "FPV"] <- 'FDT'
eDoble$PARTIDO[eDoble$PARTIDO == "FRENTE DE IZQUIERDA Y DE TRABAJADORES"] <- 'FIT'
eDoble$PARTIDO[eDoble$PARTIDO == "UNA"] <- 'UNA(massa)'


#VISUALIZACIONES DE LAS TABLAS DE LAS ELECCIONES

library(RColorBrewer)
torta <- e2015god %>% group_by(PARTIDO) %>% summarise(suma_votos=sum(VOTOS))
color <- brewer.pal(6, "Set2") 
pct <- round((torta$suma_votos/sum(e2015god$VOTOS))*100)
etiquetas <- paste( pct) # Añadimos porcentajes a etiquetas
etiquetas <- paste(etiquetas,"%",sep="")
pie(torta$suma_votos,labels=etiquetas,col=color,main="Porcentaje de votos por partido del 2015")
legend("topright", torta$PARTIDO, cex = 0.8,fill =color)

torta2 <- e2019god %>% group_by(PARTIDO) %>% summarise(suma_votos=sum(VOTOS))
color <- brewer.pal(6, "Set2") 
pct2 <- round((torta2$suma_votos/sum(e2019god$VOTOS))*100)
etiquetas2 <- paste( pct2) # Añadimos porcentajes a etiquetas
etiquetas2 <- paste(etiquetas2,"%",sep="")
pie(torta2$suma_votos,labels=etiquetas2,col=color,main="Porcentaje de votos por partido del 2019")
legend("bottomleft", torta2$PARTIDO, cex = 0.56,fill =color)

barplot(prop.table(table(eDoble$PARTIDO)))
partidos <- unique(eDoble$PARTIDO)
partidos
izquierda <- c("FRENTE NOS", "PROG", "FDT", "UNA(massa)", "ROD", "FIT", "CONSENSO FEDERAL")
derecha <- c("JPC", "UNITE POR LA LIBERTAD Y LA DIGNIDAD")
eDoble666 <- eDoble %>% mutate(Inclinacion = NA)
eDoble666 <- eDoble666 %>% mutate(Inclinacion = case_when(PARTIDO %in% izquierda ~ "Izquierda", PARTIDO %in% derecha ~ "Derecha"))

barplot(prop.table(table(eDoble666$Inclinacion)),col=c("orange","blue"))

ggplot() + 
  geom_bar(data=eDoble666,aes(x=VOTOS, y=as.character(ANIO),fill=Inclinacion), stat='identity') +
  coord_flip()
aver <- eDoble666 %>% filter(ANIO == 2015)
aver <- aver %>% filter(Inclinacion == "Derecha")
View(aver)
sum(aver$VOTOS)

#TABLA EPH

install.packages('eph')
library(readxl)
library(eph)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
dt <- get_microdata(year = 2015, 
                    trimester = 2,
                    type = 'individual')
prov <- read.csv("C:/Users/nicol/OneDrive/Documentos/provincias_god.csv")
dt2=dt
dt2 <- dt2[!(dt2$ESTADO == 4),]
dt2 <- dt2[!(dt2$ESTADO == 0),]
dt2 <- dt2[!(dt2$NIVEL_ED == 9),]
dt2$NIVEL_ED[dt2$NIVEL_ED == 7] <- 0
dt2 <- merge(x = dt, y = prov, by = "AGLOMERADO")
dtGod <- dt2 %>% group_by(Provincia) %>% summarise(Nivel_educativo = mean(NIVEL_ED))
dtGod <- dtGod %>% mutate(Anio = 2019)
dtEstadoTotal <- dt2 %>% group_by(Provincia) %>% summarise(cantidad=n())
dtEstado1 <- dt2 %>% filter(ESTADO==1) %>% group_by(Provincia) %>% summarise(Indice_de_ocupados=n())
dtEstado1 <- dtEstado1 %>% mutate(Indice_de_ocupados = dtEstado1$Indice_de_ocupados/dtEstadoTotal$cantidad)
dtEstado2 <- dt2 %>% filter(ESTADO==2) %>% group_by(Provincia) %>% summarise(Indice_de_desocupados=n())
dtEstado2 <- dtEstado2 %>% mutate(Indice_de_desocupados = dtEstado2$Indice_de_desocupados/dtEstadoTotal$cantidad)
dtEstado3 <- dt2 %>% filter(ESTADO==3) %>% group_by(Provincia) %>% summarise(Indice_de_inactividad=n())
dtEstado3 <- dtEstado3 %>% mutate(Indice_de_inactividad = dtEstado3$Indice_de_inactividad/dtEstadoTotal$cantidad)
dtGod <- merge(x = dtGod, y = dtEstado1, by = "Provincia")
dtGod <- merge(x = dtGod, y = dtEstado2, by = "Provincia")
dtGod <- merge(x = dtGod, y = dtEstado3, by = "Provincia")
dtEstadoTotal <- dt2 %>% filter(! is.na(PP04A)) %>% group_by(Provincia) %>% summarise(cantidad=n())
dtEstatal <- dt2 %>% filter(PP04A==1) %>% group_by(Provincia) %>% summarise(Indice_de_empleo_publico=n())
dtEstatal <- dtEstatal %>% mutate(Indice_de_empleo_publico = dtEstatal$Indice_de_empleo_publico/dtEstadoTotal$cantidad)
dtPrivado <- dt2 %>% filter(PP04A==2) %>% group_by(Provincia) %>% summarise(Indice_de_empleo_privado=n())
dtPrivado <- dtPrivado %>% mutate(Indice_de_empleo_privado = dtPrivado$Indice_de_empleo_privado/dtEstadoTotal$cantidad)
dtGod <- merge(x = dtGod, y = dtEstatal, by = "Provincia")
dtGod <- merge(x = dtGod, y = dtPrivado, by = "Provincia")
dt <- merge(x = dt, y = prov, by = "AGLOMERADO")
dtSalarioMedio <- dt %>% filter(P47T != 0, P47T != -9, ! is.na(PP04A)) %>% group_by(Provincia) %>% summarise(salario_Medio=mean(P47T))
dtGod <- merge(x = dtGod, y = dtSalarioMedio, by = "Provincia")
dtPlaneros <- dt %>% filter(V5_M != -9, V5_M > 0) %>% group_by(Provincia) %>% summarise(Cantidad_de_gente_con_planes = n())
dtTotales <- dt %>% filter(V5_M != -9) %>% group_by(Provincia) %>% summarise(cantidad_total = n())
dtProporcionPlaneros <- dt %>% filter(V5_M != -9) %>% group_by(Provincia) %>% summarise(proporcion_planeros = n())
dtProporcionPlaneros <- dtProporcionPlaneros %>% mutate(proporcion_planeros = dtPlaneros$Cantidad_de_gente_con_planes/dtTotales$cantidad_total)
dtGod <- merge(x = dtGod, y = dtProporcionPlaneros, by = "Provincia")
dtExtranjeros <- dt %>% filter(CH15 %in% c(4,5), ! is.na(CH15)) %>% group_by(Provincia) %>% summarise(cantidad_de_extranjeros = n())
dtTotales <- dt %>% group_by(Provincia) %>% summarise(cantidad_de_extranjeros = n())
prov2 <- prov %>% group_by(Provincia) %>% summarise(noimporta=n())
prov2 <- prov2 %>% mutate(noimporta = NULL)
dtExtranjeros <- merge(x = prov2, y = dtExtranjeros, by = "Provincia", all.x = TRUE)
View(dtExtranjeros)
dtExtranjeros$cantidad_de_extranjeros[is.na(dtExtranjeros$cantidad_de_extranjeros)] <- 0
dtProporcionExtranjero <- dt %>% group_by(Provincia) %>% summarise(proporcion_extranjeros = n())
dtProporcionExtranjero <- dtProporcionExtranjero %>% mutate(proporcion_extranjeros = dtExtranjeros$cantidad_de_extranjeros/dtTotales$cantidad_de_extranjeros)
dtGod <- merge(x = dtGod, y = dtProporcionExtranjero, by = "Provincia")
dtPrepagos <- dt2 %>% filter(CH08 %in% c(2,12,23,123)) %>% group_by(Provincia) %>% summarise(proporcion_de_gente_con_prepaga = n())
dtSociales <- dt2 %>% filter(! CH08 %in% c(2,12,23,123,4)) %>% group_by(Provincia) %>% summarise(proporcion_de_gente_sin_prepaga = n())
dtInsanos <- dt2 %>% filter(CH08 == 4) %>% group_by(Provincia) %>% summarise(proporcion_de_gente_sin_salud = n())
dtPrepagos <- dtPrepagos %>% mutate(proporcion_de_gente_con_prepaga = dtPrepagos$proporcion_de_gente_con_prepaga/dtTotales$cantidad_de_extranjeros)
dtSociales <- dtSociales %>% mutate(proporcion_de_gente_sin_prepaga = dtSociales$proporcion_de_gente_sin_prepaga/dtTotales$cantidad_de_extranjeros)
dtInsanos <- dtInsanos %>% mutate(proporcion_de_gente_sin_salud = dtInsanos$proporcion_de_gente_sin_salud/dtTotales$cantidad_de_extranjeros)
dtGod <- merge(x = dtGod, y = dtPrepagos, by = "Provincia")
dtGod <- merge(x = dtGod, y = dtSociales, by = "Provincia")
dtGod <- merge(x = dtGod, y = dtInsanos, by = "Provincia")
dtGodeph2019 <- dtGod
view(dtGod)


graf_linea <- data.frame(dtGod$Provincia,dtGod$Nivel_educativo,dtGod$salario_Medio)

library(ggplot2)
dtGod %>% ggplot(graf_linea, aes(x=Nivel_educativo, y=salario_Medio, group = Provincia, colour =Provincia )) + 
  geom_line()  + 
  geom_point( size=2, shape=21, fill="white") 

plot(dtGod$proporcion_planeros, dtGod$salario_Medio, type = "b")
points(dtGod$proporcion_planeros, dtGod$salario_Medio,    
       pch = 21,      # Símbolo
       cex = 2,       # Tamaño del símbolo
       bg = "red",  
       col = "red", 
       lwd = 3)

        
