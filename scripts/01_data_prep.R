# ============================================================
# 01_data_prep.R
# ============================================================

source(here("scripts", "00_setup.R"))


# PARTICIPANTS SELECTION.
# In zone
# ============================================================

# Download Adreces ajuntament de Barcelona SF object

#Adreces<- "https://opendata-ajuntament.barcelona.cat/data/dataset/25752522-3528-4c14-b68d-5f09a3e393bd/resource/661fe190-67c8-423a-b8eb-8140f547fde2/download"

#download.file(
#  url = Adreces,
#  destfile = "data/adreces.csv",
#  mode = "wb"
#)

BCN_adreces<- st_read("data/adreces.csv")

# Download còdis de carrer de Barcelona

#Codis_carrerer<- "https://opendata-ajuntament.barcelona.cat/data/dataset/d7802fd1-cdfb-4562-9148-d18722d7e2d8/resource/2b010e59-6952-4b27-9c4e-47fcaf64c916/download"

#download.file(
#  url = Codis_carrerer,
#  destfile = "data/carrerer.csv",
#  mode = "wb"
#)

Codis_carrerer<- st_read("data/carrerer.csv")

# Merge carrerer amb graf edificis pero obtenir codi de tipus de vial 
# Elimina lletres i duplicats de númermo#########

BCN_adreces_codis<-BCN_adreces%>%
  
  left_join(Codis_carrerer[,c(1,3,4,5)],
            by=c("codi_carrer"="codi_via"))%>%
  
  mutate(Seccio_Censal = paste0(
    as.integer(districte),
    str_pad(as.character(secc_cens), width = 3, pad = "0")),
    
    llepost_i=trimws(llepost_i),
    llepost_i = na_if(llepost_i,""),
    codi_parc=trimws(codi_parc),
    codi_parc = na_if(codi_parc,""))%>%
  
  filter(
    !is.na(codi_parc)) %>%
  
  group_by(tipus_via, nom_carrer, numpost_i) %>%
  arrange(is.na(llepost_i), .by_group = TRUE) %>%
  filter(
    n() == 1 |                    # única fila → conservar
      !is.na(llepost_i) |           # varias → solo con letra
      row_number() == 1             # todas vacías → la primera
  ) %>%
  ungroup() %>%
  distinct(tipus_via, nom_carrer, as.numeric(numpost_i), .keep_all = TRUE)

### Borro UTS sense coordanades i alguns canvis inicials de majuscules

# Limpieza inicial:
# - eliminar registros sin coordenadas
# - normalizar nombre de calle

BCN_adreces_codis <- BCN_adreces_codis %>%
  mutate(
    
    # Nombre de calle normalizado
    nom_carrer = normaliza_carrer(nom_carrer),
    numpost_i  = as.integer(numpost_i),
    
    # Rango de numeración del portal
    numpost_i = as.integer(str_remove(as.character(numpost_i), "^0+")),
    numpost_f = as.integer(str_remove(as.character(numpost_f), "^0+")),
    
    #Clave
    Clave = paste(tipus_via,nom_carrer,numpost_i,sep = "|"))%>%
  
  filter(
    !is.na(nom_carrer),
    nom_carrer != "",
    !is.na(numpost_i),
    !is.na(x_etrs89),
    !is.na(y_etrs89)
  ) %>%
  select(
    codi_carrer,
    tipus_via,
    Clave,
    codi_parc,
    nom_carrer,
    nom_curt,
    nom_oficial,
    llepost_i,
    llepost_f,
    numpost_i,
    numpost_f,
    dist_post,
    districte,
    nom_districte,
    barri,
    nom_barri,
    Seccio_Censal,
    x_etrs89,
    y_etrs89,
    longitud_wgs84,
    latitud_wgs84
  ) 


# Checks no hay duplicados por calle y numero 
BCN_adreces_codis %>%
  count(Clave, sort = TRUE) %>%
  filter(n > 1)

BCN_adreces_codis %>%
  count(longitud_wgs84, sort = TRUE) %>%
  filter(is.na(n))

####### Carrers pacients #########

User_adreces<- readRDS(here("data", "Starting", "ADRECES_FINAL_11_09_026.rds"))

##### Transformació tipus de vial ######

User_adreces <- User_adreces %>%
  mutate(
    nom_carrer = normaliza_carrer(nom_carrer),
    numpost_i = as.integer(numpost_i),
    tipus_via = case_when(
      USUA_TIPUS_DE_VIAL == "CR" ~ "C",
      USUA_TIPUS_DE_VIAL == "AV" ~ "Av",
      USUA_TIPUS_DE_VIAL == "TS" ~ "Trav",
      USUA_TIPUS_DE_VIAL == "PL" ~ "Pl",
      USUA_TIPUS_DE_VIAL == "RI" ~ "Rier",
      USUA_TIPUS_DE_VIAL == "PG" ~ "Ptge",
      USUA_TIPUS_DE_VIAL == "BX" ~ "Bda",
      USUA_TIPUS_DE_VIAL == "PS" ~ "Pg",
      USUA_TIPUS_DE_VIAL == "VI" ~ "Via",
      USUA_TIPUS_DE_VIAL == "RD" ~ "Rda",
      USUA_TIPUS_DE_VIAL == "GV" ~ "G.V.",
      USUA_TIPUS_DE_VIAL == "RB" ~ "Rbla",
    )
  )

######## Normalización denominación direcciones de HC a Carrers de Barcelona hacer igual que la denominación original. #######

User_adreces <- User_adreces %>%
  mutate(
    # Normalizar el nombre original
    nom_carrer_norm = normaliza_carrer(nom_carrer),
    
# Aplicar equivalencias
    nom_carrer_join = case_when(
# Quitar "DE" o artículos
      nom_carrer_norm == normaliza_carrer("AGUSTINA DE SARAGOSSA") ~ "AGUSTINA SARAGOSSA",
      nom_carrer_norm == normaliza_carrer("DE CARLES III") ~ "CARLES III",
      nom_carrer_norm == normaliza_carrer("DE LES CORTS") ~ "CORTS",
      nom_carrer_norm == normaliza_carrer("DE GRACIA") ~ "GRACIA",
      nom_carrer_norm == normaliza_carrer("DE MALLORCA") ~ "MALLORCA",
      nom_carrer_norm == normaliza_carrer("DE XILE") ~ "XILE",
      nom_carrer_norm == normaliza_carrer("DE PAU CASALS") ~ "PAU CASALS",
      nom_carrer_norm == normaliza_carrer("DE JOSEP TARRADELLAS") ~ "JOSEP TARRADELLAS",
      nom_carrer_norm == normaliza_carrer("DE LA RIERA DE CASSOLES") ~ "RIERA DE CASSOLES",
      nom_carrer_norm == normaliza_carrer("DE BOSCH I GIMPERA") ~ "BOSCH I GIMPERA",
      nom_carrer_norm == normaliza_carrer("DE CAN MARCET") ~ "CAN MARCET",
      nom_carrer_norm == normaliza_carrer("DEL PARE MARIANA") ~ "PARE MARIANA",
      nom_carrer_norm == normaliza_carrer("DEL MESTRE ANTONI NICOLAU") ~ "MESTRE NICOLAU",
      nom_carrer_norm == normaliza_carrer("D'EN PUJOL") ~ "PUJOL",
      nom_carrer_norm == normaliza_carrer("SABINO DE ARANA") ~ "SABINO ARANA",
      nom_carrer_norm == normaliza_carrer("LA LLACUNA") ~ "LLACUNA",
      nom_carrer_norm == normaliza_carrer("LA TORRE") ~ "TORRE",
      nom_carrer_norm == normaliza_carrer("LA ALFAMBRA") ~ "ALFAMBRA",
      nom_carrer_norm == normaliza_carrer("RIERA BLANCA") ~ "BLANCA",
      nom_carrer_norm == normaliza_carrer("DELS SEGADORS") ~ "SEGADORS",
      nom_carrer_norm == normaliza_carrer("MARQUES DE MONT ROIG") ~ "MONT ROIG",
      nom_carrer_norm == normaliza_carrer("ADVOCAT MANUEL BALLBE") ~ "MANUEL BALLBE",
      
# Correcciones ortográficas
      nom_carrer_norm == normaliza_carrer("BERTRAN I ROZPIDE") ~ "BELTRAN I ROZPIDE",
      nom_carrer_norm == normaliza_carrer("JOAN SEBASTIA BACH") ~ "JOHANN SEBASTIAN BACH",
      nom_carrer_norm == normaliza_carrer("SOR EULALIA ANZIZU") ~ "SOR EULALIA D ANZIZU",
      nom_carrer_norm == normaliza_carrer("PRESIDENT JOSEP IRLA I BOSCH") ~ "JOSEP IRLA I BOSCH",
      nom_carrer_norm == normaliza_carrer("RICARD CALVO") ~ "RICARDO CALVO",
      nom_carrer_norm == normaliza_carrer("COMES") ~ "COMAS",
      nom_carrer_norm == normaliza_carrer("SANT GENIS") ~ "SANT GENIS A HORTA",
      nom_carrer_norm == normaliza_carrer("MESTRE ANTONI NICOLAU") ~ "MESTRE NICOLAU",
      
# Añadir palabras faltantes
      nom_carrer_norm == normaliza_carrer("CIUTAT BALAGUER") ~ "CIUTAT DE BALAGUER",
      nom_carrer_norm == normaliza_carrer("COMTE SALVATIERRA") ~ "COMTE DE SALVATIERRA",
      nom_carrer_norm == normaliza_carrer("DOMINGUEZ MIRALLES") ~ "DOMINGUEZ I MIRALLES",
      nom_carrer_norm == normaliza_carrer("CARAVEL LA NINA") ~ "CARAVEL LA LA NINA",
      nom_carrer_norm == normaliza_carrer("MARIA CUBI I SOLER") ~ "MARIA CUBI",
      
# Si no coincide con nada, dejar el valor original
      TRUE ~ nom_carrer_norm
    ),
    tipus_via = case_when(
      nom_carrer_join == "MANUEL GIRONA" & tipus_via == "C"  ~  "Pg",
      TRUE ~ tipus_via),
    
    Clave = paste(
      tipus_via,nom_carrer_join,numpost_i,sep = "|")
  )%>%
  select(-nom_carrer_norm)   

# Join SF carrers de BCN amb adreces users. Identificació postals.
# ==================================================================

BCN_adreces_users <- User_adreces %>%
  left_join(
    BCN_adreces_codis %>% select(-tipus_via, -nom_carrer, -numpost_i),
    by = "Clave"
  ) %>%
  distinct(ID, .keep_all = TRUE) %>%
  filter(!is.na(x_etrs89) | !is.na(y_etrs89))

BCN_adreces_users_SF <- st_as_sf(
  BCN_adreces_users,
  coords = c("x_etrs89", "y_etrs89"),
  crs    = 25831,
  remove = FALSE)

BCN_adreces_users_SF<-st_make_valid(BCN_adreces_users_SF)

#Perímetre de les ABS dels centres participants.

ABS_sf<-st_read(
  here("data", "external", "cartografia_centres", "ABS.shp"),
  quiet = TRUE
)

ABS_sel <- ABS_sf %>%
  filter(NOMABS %in% c("Barcelona - 04A","Barcelona - 04B","Barcelona - 04C","Barcelona - 05B","Barcelona - 05A","Barcelona - 02C","Barcelona - 02E"))

ABS_sel <- st_make_valid(ABS_sel)

## Detección de los que no están en ZONA

BCN_adreces_users_SF <- BCN_adreces_users_SF %>%
  st_join(
    ABS_sel %>% select(CODABSa, NOMABS),
    join = st_intersects,
    left = TRUE
  ) %>%
  mutate(
    included = if_else(is.na(CODABSa), "no included", "included")
  ) %>%
  distinct(ID, .keep_all = TRUE)

BCN_adreces_users_SF_included <- BCN_adreces_users_SF %>%
  filter(
    included =="included")

## Saver IDs de pacients en zona

ID_not_in_zone<-BCN_adreces_users_SF%>%
  select(ID,included)%>%
  as_tibble()%>%
  select(-geometry)

ID_in_zone<-ID_not_in_zone%>%
  filter(included=="included")

saveRDS(ID_in_zone,here(here("data", "Starting", "ID_in_zone_inclussion.shp")))

saveRDS(
  BCN_adreces_users_SF_included,
  here("data", "SF", "adreces_users_SF.rds")
)

BCN_adreces_users_SF_included_Data_tableSF <- BCN_adreces_users_SF %>%
  filter(included == "included") %>%
  st_drop_geometry() %>%
  select(-x,-y,-n_grupo,-idx,-offset,-x_vis,-y_vis)

saveRDS(BCN_adreces_users_SF_included_Data_tableSF,here("data", "SF", "BCN_adreces_users_SF_included_Data_table.rds"))


Situacio<-Situacio%>%
  select("USUA_CIP","ATDOM","USUA_SITUACIO")%>%
  filter(as.Date(ATDOM) <= as.Date("2024-06-16"))

# Selecció situació atdom sis mesos anteriors

  
# Primera carga de datos raw inicial

DF_work<-readRDS(here("data", "Starting", "DF_INICIAL_11_09_026.RDS"))

### Solo de los que dispongo dirección y estan en zona y por fecha de inclusión 6 meses antes (evitar ATDOMs transitorios y final de vida precipitados)

DF_work<-DF_work%>%
  inner_join(ID_in_zone[,c(1)], by="ID")%>%
   filter(as.Date(ATDOM) <= as.Date("2024-06-16"))
  

# Cargar el diccionario de metadatos desde csv
metadata_dict <- read_csv2(here("data", "metadata_dict.csv")) |>
  # eliminar filas con todo NA
  filter(if_any(everything(), ~ !is.na(.)))

# Preparar datos TB_pacientes

TB_pac_inicial <- DF_work

# Guardar dataset 

saveRDS(TB_pacientes, here("data","Tables_DB","TB_pac_inicial.RDS"))

# Flujo

validate_input_data(TB_pacientes, metadata_dict) # valida y lanza warnings/errors


