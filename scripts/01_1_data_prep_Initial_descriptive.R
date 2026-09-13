# ============================================================
# 01_1_data_prep_Initial_descriptive.R
# ============================================================

source(here("scripts", "00_0 setup.R"))

# Carga de datos raw

DF_Inicial<-readRDS(here("data","Starting", "DF_INICIAL_11_09_026.rds"))

patologias_interes  <- c(
  # Cardiovasculares
  "HTA", "IC", "Card. Isquémica", "Disrrítmias",
  # Neurocognitivas
  "Demencia", "Parkinson", "AVC",
  # Metabólicas
  "Diabetes", "Hiperlipidemia",
  # Renales
  "IRC",
  # Respiratorias
  "EPOC", "Insuf. Respiratoria",
  # Músculo-esqueléticas
  "Artrosis", "Fractura de fémur", "Osteoporosis",
  # Psiquiátricas
  "Depressión", "Ansiedad",
  # Complementarias
  "Anemia", "Infecciones urinarias", "Glaucoma"
)

DF_patologias <- DF_Inicial %>% 
  select(ID,all_of(patologias_interes))

DF_Inicial<-DF_Inicial%>%
  select(c(1:39))%>%
  left_join(DF_patologias, 
            by="ID")

##Solo de los que dispongo dirección y estan en zona

inclussion<-readRDS(here("data", "Starting", "ID_in_zone_inclussion.rds"))

DF_Inicial<-DF_Inicial%>%
  filter(ID %in% inclussion$ID)

# Cargar el diccionario de metadatos desde csv

metadata_dict_inicial <- read_csv2(here("data", "metadata_dict_inicial.csv")) |>

# eliminar filas con todo NA

filter(if_any(everything(), ~ !is.na(.)))

# Preparar datos TB_pacientes 

# Flujo

validate_input_data(DF_Inicial, metadata_dict_inicial) # valida y lanza warnings/errors

# Guardar dataset 

saveRDS(DF_Inicial, here("data","processed","DF_pacientes_inicial.RDS"))

