# ============================================================
# 01_1_data_prep_Initial_descriptive.R
# ============================================================

source(here("scripts", "00_setup.R"))

# Carga de datos raw

DF_Inicial<-readRDS(here("data","Starting", "DF_Descrip_inicial.rds"))

patologias_interes  <- c(
  # Cardiovasculares
  "HTA", "IC", "Card. Isquémica", "Valvulopatía", "Disrrítmias",
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
  select(USUA_CIP,all_of(patologias_interes))

DF_Inicial<-DF_Inicial%>%
  select(c(1:41))%>%
  left_join(DF_patologias, 
            by="USUA_CIP")

##Solo de los que dispongo dirección y estan en zona

Adreces<-readRDS(here("data", "processed", "adreces_SF.rds"))

# Cargar el diccionario de metadatos desde csv

metadata_dict_inicial <- read_csv2(here("data", "metadata_dict_inicial.csv")) |>
    # eliminar filas con todo NA
  filter(if_any(everything(), ~ !is.na(.)))

# Preparar datos TB_pacientes -----------------------------------------

# Flujo

validate_input_data(DF_Inicial, metadata_dict_inicial) # valida y lanza warnings/errors

# Guardar dataset 

saveRDS(DF_Inicial, here("data","processed","DF_pacientes_inicial.RDS"))

