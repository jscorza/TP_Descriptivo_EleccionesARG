install.packages('eph')
library(readxl)
library(eph)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(tidyverse)
dt <- get_microdata(year = 2019, 
                    trimester = 2,
                    type = 'individual')
prov <- read.csv("provincias_godd.csv")
dt2 <- merge(x = dt, y = prov, by = "AGLOMERADO")

#EDUCACION sacamos los 9 y 7 
dt2 <- dt2[!(dt2$NIVEL_ED == 9),]
dt2$NIVEL_ED[dt2$NIVEL_ED == 7] <- 0
dtGod <- dt2 %>% group_by(Provincia) %>% summarise(Nivel_educativo = mean(NIVEL_ED))
dtGod <- dtGod %>% mutate(Anio = 2019)

dt2 <- merge(x = dt, y = prov, by = "AGLOMERADO") # le vuelvo a meter los q saque.
#ESTADO sacamos 4 y 0 
dt2 <- dt2[!(dt2$ESTADO == 4),]
dt2 <- dt2[!(dt2$ESTADO == 0),]
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


dt2 <- merge(x = dt, y = prov, by = "AGLOMERADO") #le vuelvo a meter los q saque.

#EMPLEO PUBLICO VS PRIVADO
dtEstadoTotal <- dt2 %>% filter(! is.na(PP04A)) %>% group_by(Provincia) %>% summarise(cantidad=n())
dtEstatal <- dt2 %>% filter(PP04A==1) %>% group_by(Provincia) %>% summarise(Indice_de_empleo_publico=n())
dtEstatal <- dtEstatal %>% mutate(Indice_de_empleo_publico = dtEstatal$Indice_de_empleo_publico/dtEstadoTotal$cantidad)
dtPrivado <- dt2 %>% filter(PP04A==2) %>% group_by(Provincia) %>% summarise(Indice_de_empleo_privado=n())
dtPrivado <- dtPrivado %>% mutate(Indice_de_empleo_privado = dtPrivado$Indice_de_empleo_privado/dtEstadoTotal$cantidad)
dtGod <- merge(x = dtGod, y = dtEstatal, by = "Provincia")
dtGod <- merge(x = dtGod, y = dtPrivado, by = "Provincia")

#aca empezamos a laburar con dt, pero es lo mismo q era dt2 completo. 

# Salarios Medios 
dt <- merge(x = dt, y = prov, by = "AGLOMERADO")

dtSalarioMedio <- dt %>% filter(P47T != 0, P47T != -9, ! is.na(PP04A)) %>% group_by(Provincia) %>% summarise(salario_Medio=mean(P47T))
dtGod <- merge(x = dtGod, y = dtSalarioMedio, by = "Provincia")

# PLANEROS 
dtPlaneros <- dt %>% filter(V5_M != -9, V5_M > 0) %>% group_by(Provincia) %>% summarise(Cantidad_de_gente_con_planes = n())
dtTotales <- dt %>% filter(V5_M != -9) %>% group_by(Provincia) %>% summarise(cantidad_total = n())
dtProporcionPlaneros <- dt %>% filter(V5_M != -9) %>% group_by(Provincia) %>% summarise(proporcion_planeros = n())
dtProporcionPlaneros <- dtProporcionPlaneros %>% mutate(proporcion_planeros = dtPlaneros$Cantidad_de_gente_con_planes/dtTotales$cantidad_total)
dtGod <- merge(x = dtGod, y = dtProporcionPlaneros, by = "Provincia")

#EXTRANJEROS
dtExtranjeros <- dt %>% filter(CH15 %in% c(4,5), ! is.na(CH15)) %>% group_by(Provincia) %>% summarise(cantidad_de_extranjeros = n())
dtTotales <- dt %>% group_by(Provincia) %>% summarise(cantidad_de_extranjeros = n())
prov2 <- prov %>% group_by(Provincia) %>% summarise(noimporta=n())
prov2 <- prov2 %>% mutate(noimporta = NULL)
dtExtranjeros <- merge(x = prov2, y = dtExtranjeros, by = "Provincia", all.x = TRUE)
dtExtranjeros$cantidad_de_extranjeros[is.na(dtExtranjeros$cantidad_de_extranjeros)] <- 0
dtProporcionExtranjero <- dt %>% group_by(Provincia) %>% summarise(proporcion_extranjeros = n())
dtProporcionExtranjero <- dtProporcionExtranjero %>% mutate(proporcion_extranjeros = dtExtranjeros$cantidad_de_extranjeros/dtTotales$cantidad_de_extranjeros)
dtGod <- merge(x = dtGod, y = dtProporcionExtranjero, by = "Provincia")

#PREPAGA VS NO PREPAGA


dtPrepagos <- dt2 %>% filter(CH08 %in% c(2,12,23,123)) %>% group_by(Provincia) %>% summarise(proporcion_de_gente_con_prepaga = n())
dtSociales <- dt2 %>% filter(! CH08 %in% c(2,12,23,123,4)) %>% group_by(Provincia) %>% summarise(proporcion_de_gente_sin_prepaga = n())
dtInsanos <- dt2 %>% filter(CH08 == 4) %>% group_by(Provincia) %>% summarise(proporcion_de_gente_sin_salud = n())
dtPrepagos <- dtPrepagos %>% mutate(proporcion_de_gente_con_prepaga = dtPrepagos$proporcion_de_gente_con_prepaga/dtTotales$cantidad_de_extranjeros)
dtSociales <- dtSociales %>% mutate(proporcion_de_gente_sin_prepaga = dtSociales$proporcion_de_gente_sin_prepaga/dtTotales$cantidad_de_extranjeros)
dtInsanos <- dtInsanos %>% mutate(proporcion_de_gente_sin_salud = dtInsanos$proporcion_de_gente_sin_salud/dtTotales$cantidad_de_extranjeros)
dtGod <- merge(x = dtGod, y = dtPrepagos, by = "Provincia")
dtGod <- merge(x = dtGod, y = dtSociales, by = "Provincia")
dtGod <- merge(x = dtGod, y = dtInsanos, by = "Provincia")


view(dtGod)
