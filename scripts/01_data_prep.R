# ============================================================
# 02_recode_variables.R

source(here("scripts", "00_setup.R"))

# Carga de datos raw

DF_work<-readRDS(here("data", "DF_work.RDS"))

##Solo de los que dispongo dirección y estan en zona

Adreces<-readRDS(here("data", "processed", "adreces_SF.rds"))

DF_work<-DF_work%>%
  inner_join(Adreces[,c(1)], by="ID")

# Cargar el diccionario de metadatos desde csv
metadata_dict <- read_csv2(here("data", "metadata_dict.csv")) |>
  # eliminar filas con todo NA
  filter(if_any(everything(), ~ !is.na(.)))

# Preparar datos TB_pacientes -----------------------------------------

TB_pacientes <- DF_work

# Guardar dataset 

saveRDS(TB_pacientes, here("data","Tables_DB","TB_pacientes.RDS"))

# Flujo

validate_input_data(TB_pacientes, metadata_dict) # valida y lanza warnings/errors


