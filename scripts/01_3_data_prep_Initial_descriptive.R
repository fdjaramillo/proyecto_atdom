# ============================================================
# 01_1_data_prep_Initial_descriptive.R
# ============================================================

source(here("scripts", "00_0 setup.R"))

# Carga de datos raw

DF_Inicial<-readRDS(here("data","Starting", "DF_INICIAL_11_09_026.rds"))%>%
  left_join(readRDS(here("data", "processed", "paciente_ruta_distancia.rds")),
            by="ID")

# Cargar el diccionario de metadatos desde csv
  
metadata_dict_inicial <- read_csv2(here("data", "metadata_dict_inicial.csv"))
  
# Preparar datos TB_pacientes 

validate_input_data(DF_Inicial, metadata_dict_inicial) # valida y lanza warnings/errors

# Guardar dataset 

saveRDS(DF_Inicial, here("data","processed","DF_pacientes_inicial.RDS"))

## Solo de los que dispongo dirección y están en zona

inclussion<-readRDS(here("data", "Starting", "ID_in_zone_inclussion.rds"))

DF_Inicial<-DF_Inicial%>%
  filter(ID %in% inclussion$ID)

# Save Dataframe inicial

saveRDS(DF_Inicial,here("data","processed","DF_pacientes_inicial.RDS"))

