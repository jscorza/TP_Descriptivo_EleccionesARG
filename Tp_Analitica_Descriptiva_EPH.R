install.packages('eph')
library(readxl)
library(eph)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
dt <- get_microdata(year = 2019, 
                    trimester = 2,
                    type = 'individual')
prov <- read.csv("provincias_godd.csv")
dt2 = dt
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
View(dtGod)

poblaciongral = data.frame(edu = rep(dt$NIVEL_ED,dt$PONDERA),geo = rep(dt$AGLOMERADO,dt$PONDERA))
View(poblaciongral)
poblaciongral %>% group_by(geo) %>% summarise(edu = mean(edu))
colnames(poblaciongral)[2] <- "AGLOMERADO"
poblaciongral <- merge(x = poblaciongral, y = prov, by = "AGLOMERADO")

poblaciongral <- poblaciongral %>% group_by(Provincia) %>% summarise(edu = mean(edu))
View(poblaciongral)

totalPonderado <- sum(dt$PONDERA)


dt <- get_microdata(year = 2015, 
                    trimester = 2,
                    type = 'individual')
prov <- read.csv("provincias_god.csv")
dt2 <- dt2[!(dt2$ESTADO == 4),] #se sacan los entrevistados menores de 10 anios
dt2 <- dt2[!(dt2$ESTADO == 0),] #tambien se sacan los que no respondieron cuestionario
dt2 <- dt2[!(dt2$NIVEL_ED == 9),] # se saca a los que no sabe/no responde
dt2$NIVEL_ED[dt2$NIVEL_ED == 7] <- 0 #los sin instruccion tenian 7, y para pasarlo a categorico logico lo cambiamos a cero
dt2 <- merge(x = dt, y = prov, by = "AGLOMERADO") #le agregamos los nombres de las provincias


poblaciongral = data.frame(edu = rep(dt2$NIVEL_ED,dt2$PONDERA),prov = rep(dt2$Provincia,dt2$PONDERA),estado=rep(dt2$ESTADO,dt2$PONDERA),cat_ocu = rep(dt2$CAT_OCUP,dt2$PONDERA), 
                           cant_salario=rep(dt2$P21,dt2$PONDERA), tipo_empleo = rep(dt2$PP04A, dt2$PONDERA), total_subsidios = rep(dt2$V5_M, dt2$PONDERA), lugar_nacimiento = rep(dt2$CH15, dt2$PONDERA),
                           tipo_de_cobertura = rep(dt2$CH08, dt2$PONDERA)
                           
                           ) %>% as_tibble()

poblaciongral_sum <- poblaciongral %>% group_by(prov) %>% summarise(media_educativa = mean(edu),cantidad_ocupados = sum(estado==1), cantidad_inactivos=sum(cat_ocu==3),cantidad_desocupados=sum(cat_ocu==2),
                                                                    total_pòblacion = n(),
                                                                    cantidad_activos = sum(estado!=3), empleados_sector_privado = sum(tipo_empleo == 2, na.rm = T), empleados_sector_publico = sum(tipo_empleo == 1,  na.rm = T),
                                                                    total_subsidios= sum(total_subsidios > 0), cantidad_extranjeros = sum(lugar_nacimiento==5 | lugar_nacimiento == 4), 
                                                                    cant_prepaga = sum(tipo_de_cobertura %in% c(2,12,23,123)), cant_sin_prepaga = sum(!tipo_de_cobertura %in% c(2,12,23,123,4)))

poblaciongral_sum <- poblaciongral_sum %>% mutate(proporcion_asalariados = cantidad_ocupados/total_pòblacion, proporcion_desocupados=cantidad_desocupados/total_pòblacion)
View(poblaciongral_sum)

dtGod <- dtGod %>% mutate(Anio = 2015) #se agrega una columna con el anio correspondiente
dtGod <- dt2 %>% group_by(Provincia) %>% summarise(Nivel_educativo = sum(PONDERA[NIVEL_ED])/sum(PONDERA)) #Se agrega la media de educacion ponderada por provincia
dtEstadoTotal <- dt2 %>% group_by(Provincia) %>% summarise(PONDERA[cantidad=n()]) #
dtEstado1 <- dt2 %>% filter(ESTADO==1) %>% group_by(Provincia) %>% summarise(Indice_de_ocupados=n())
dtEstado1 <- dtEstado1 %>% mutate(Indice_de_ocupados = dtEstado1$Indice_de_ocupados/dtEstadoTotal$cantidad)
dtEstado2 <- dt2 %>% filter(ESTADO==2) %>% group_by(Provincia) %>% summarise(Indice_de_desocupados=n())
dtEstado2 <- dtEstado2 %>% mutate(Indice_de_desocupados = dtEstado2$Indice_de_desocupados/dtEstadoTotal$cantidad)
dtEstado3 <- dt2 %>% filter(ESTADO==3) %>% group_by(Provincia) %>% summarise(Indice_de_inactividad=n())
dtEstado3 <- dtEstado3 %>% mutate(Indice_de_inactividad = dtEstado3$Indice_de_inactividad/dtEstadoTotal$cantidad)
dtGod <- merge(x = dtGod, y = dtEstado1, by = "Provincia")
dtGod <- merge(x = dtGod, y = dtEstado2, by = "Provincia")
dtGod <- merge(x = dtGod, y = dtEstado3, by = "Provincia")
dtEstadoTotal <- dt2 %>% filter(!is.na(PP04A)) %>% group_by(Provincia) %>% summarise(cantidad=n())
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