library(tidyverse)

#armar 2019 god.
e2019 = read.csv("mesas_agrp_politicas.csv")
e2019tot = read.csv("mesas_totales.csv")
e2015 = read.csv("el2015.csv")
options(scipen = 99999999)
presi = c(000100000000000,000100000000000,000100000000000,000100000000000,000100000000000,000100000000000)
e2019 = e2019 %>% filter(CODIGO_CATEGORIA %in%  presi)
e2019tot = e2019tot %>% filter(CODIGO_CATEGORIA %in%  presi)
e2019god = e2019 %>% group_by(CODIGO_DISTRITO,CODIGO_AGRUPACION) %>%  summarise(cantidad = sum(VOTOS_AGRUPACION)) 
e2019god <- mutate(e2019god, PROVINCIA = case_when(CODIGO_DISTRITO == 1 ~ "Ciudad Autonoma de Buenos Aires", CODIGO_DISTRITO ==2 ~ "Buenos Aires" ,CODIGO_DISTRITO ==3 ~ 'Catamarca', CODIGO_DISTRITO ==4 ~ 'Cordoba', CODIGO_DISTRITO ==5 ~ 'Corrientes', CODIGO_DISTRITO ==6 ~ 'Chaco', CODIGO_DISTRITO ==7 ~ 'Chubut', CODIGO_DISTRITO ==8 ~ 'Entre Rios', CODIGO_DISTRITO ==9~'Formosa', CODIGO_DISTRITO ==10 ~ 'Jujuy',CODIGO_DISTRITO ==11~"La Pampa",CODIGO_DISTRITO ==12~"La Rioja",CODIGO_DISTRITO ==13~"Mendoza",CODIGO_DISTRITO ==14~"Misiones", CODIGO_DISTRITO ==15~"Neuquen", CODIGO_DISTRITO ==16~"Rio Negro",CODIGO_DISTRITO ==17~"Salta",CODIGO_DISTRITO ==18~"San Juan",CODIGO_DISTRITO ==19~"San Luis",CODIGO_DISTRITO ==20~"Santa Cruz", CODIGO_DISTRITO ==21~"Santa Fe",CODIGO_DISTRITO ==22~"Santiago Del Estero",CODIGO_DISTRITO ==23~"Tucuman", CODIGO_DISTRITO ==24~"Tierra Del Fuego"))
e2019god <- mutate(e2019god,PARTIDO = case_when(CODIGO_AGRUPACION == 135 ~ "JUNTOS POR EL CAMBIO",CODIGO_AGRUPACION ==131~ "FRENTE NOS",CODIGO_AGRUPACION ==133~"FRENTE DE IZQUIERDA Y DE TRABAJADORES",CODIGO_AGRUPACION ==136~"FRENTE DE TODOS",CODIGO_AGRUPACION ==137~"CONSENSO FEDERAL",CODIGO_AGRUPACION ==87~"UNITE POR LA LIBERTAD Y LA DIGNIDAD"), cantidad = cantidad)
e2019god$CODIGO_DISTRITO == NULL
e2019god$CODIGO_AGRUPACION=NULL
view(e2019god)

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



#aca se unen los minitablas para armar god
e2015god = union(e2015PROG,e2015ROD)
 e2015god = union(e2015god,e2015FIT)
 e2015god = union(e2015god,e2015UNA)
 e2015god = union(e2015god,e2015FPV)
 e2015god = union(e2015god,e2015JPC)
view(e2019god)

e2015god = e2015god %>% mutate(anio = 2015)
e2019god$CODIGO_DISTRITO = NULL
e2019god = e2019god %>% mutate(anio = 2019)
colnames(e2015god) <- c('PROVINCIA',"VOTOS","PARTIDO","ANIO")
colnames(e2019god) <- c('VOTOS',"PROVINCIA","PARTIDO","ANIO")
eDoble = union(e2015god,e2019god)
view(eDoble)

#cambiar nombres para que sean iguales. 

eDoble$PARTIDO[eDoble$PARTIDO == "FRENTE DE TODOS"] <- 'FDT'
eDoble$PARTIDO[eDoble$PARTIDO == "JUNTOS POR EL CAMBIO"] <- 'JPC'
eDoble$PARTIDO[eDoble$PARTIDO == "FPV"] <- 'FDT'
eDoble$PARTIDO[eDoble$PARTIDO == "FRENTE DE IZQUIERDA Y DE TRABAJADORES"] <- 'FIT'
eDoble$PARTIDO[eDoble$PARTIDO == "UNA"] <- 'UNA(massa)'

library(rpart)
ggplot(e2019god)+geom_boxplot(aes(VOTOS))

