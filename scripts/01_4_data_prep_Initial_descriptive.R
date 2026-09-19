# ============================================================
# 01_1_data_prep_Initial_descriptive.R
# ============================================================

source(here("scripts", "00_0 setup.R"))

# Carga de datos raw

DF_Inicial<-readRDS(here("data","Starting", "DF_INICIAL_11_09_026.rds"))%>%
  left_join(readRDS(here("data", "processed", "paciente_ruta_distancia.rds")),
            by="ID")%>%
  inner_join(
    readRDS(here("data", "Starting", "ID_in_zone_inclussion.rds")),
    by = "ID"
  )

names(DF_Inicial)

# Guardar dataset 

saveRDS(DF_Inicial, here("data","processed","DF_pacientes_inicial.RDS"))


