# ============================================================
# 03_00 Model access.R
# ============================================================

source(here("scripts", "00_0 setup.R"))

##Data load from census sections

U_cens <- readRDS(
  here("data", "SF", "unitats_censals_estudi_sf.rds"))

# Renta INE periodo disponible 2023

#URL https://www.ine.es/jaxiT3/Tabla.htm?t=30896

renta_2023<-"https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/30896.csv?nocab=1"

download.file(
  url = renta_2023,
  destfile = "data/external/renta_2023.csv",
  mode = "wb")

renta <- read.csv2("data/external/renta_2023.csv")%>%
  rename(indicador = `Indicadores.de.renta.media.y.mediana`) %>%
  filter (Periodo=="2023" &
          Municipios=="08019 Barcelona" &
          indicador=="Renta neta media por hogar")%>%
  mutate(Seccio_Censal = str_sub(str_extract(Secciones, "\\d+"), 7),
         Total=as.numeric(Total))%>%
  rename(Renta_media_hogar=Total)%>%
  select(Seccio_Censal,Renta_media_hogar)

# Índex socioeconòmic territorial

#URL  
#"https://www.idescat.cat/pub/?id=ist&n=14034&by=sec&t=2024&f=zip&fi=csv"
# https://www.idescat.cat/dades/obertes/ist

#Carga

Index_SES <- read.csv(
  "data/external/Index_socioeconomic_territorial.csv",
  fileEncoding = "UTF-16LE")%>%
  filter (str_starts(secció.censal, "Barcelona"))%>%
  mutate(secció.censal = str_remove_all(secció.censal, "Barcelona DC")) %>%
  mutate(secció.censal = str_remove_all(secció.censal, " SC"))%>%
  mutate(Seccio_Censal = str_sub(secció.censal, start = 2))%>%
  rename(Index_SES=valor)%>%
  select(Seccio_Censal,Index_SES)


Indicadors <- read.csv2(
  "data/external/Indicadors_brut.csv",
  fileEncoding = "UTF-16LE")%>%
  filter (str_starts(secció.censal, "Barcelona"))%>%
  mutate(secció.censal = str_remove_all(secció.censal, "Barcelona DC")) %>%
  mutate(secció.censal = str_remove_all(secció.censal, " SC"))%>%
  mutate(Seccio_Censal = str_sub(secció.censal, start = 2))%>%
  select(Seccio_Censal,concepte,valor)%>%
  pivot_wider(
    id_cols     = "Seccio_Censal",  # fila identificadora
    names_from  = "concepte",       # columnas nuevas
    values_from = "valor"           # valores que rellenan
  )%>%
  rename(Perc_estudis_baixos = `població amb estudis baixos (%)`,
         Perc_joves_no_estudis_postESO = `població jove sense estudis postobligatoris (%)`,
         Perc_pob_estrangera_paisos_renda_baixa =`població estrangera de països de renda baixa o mitjana (%)`)%>%
  select(Seccio_Censal,Perc_estudis_baixos,Perc_joves_no_estudis_postESO,Perc_pob_estrangera_paisos_renda_baixa)
  

#Merge

Data_SES_UC<-U_cens[,8]%>%
             st_drop_geometry()%>%
  left_join(Index_SES,
            by="Seccio_Censal")%>%
  left_join(renta,
            by="Seccio_Censal")%>%
  left_join(Indicadors,
            by="Seccio_Censal")

saveRDS(Data_SES_UC,here("data",
                         "processed",
                         "Data_SES_UC.rds"))


