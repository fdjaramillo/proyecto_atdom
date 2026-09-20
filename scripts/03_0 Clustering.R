# ============================================================
# 03_0 Cluster to case_mix selection analysis.R
# ============================================================

source(here("scripts", "00_0 setup.R"))

#Load Data

df<-readRDS(here("data","processed","DF_pacientes_inicial.RDS"))

#1. DATASET PARA FAMDón de variables del clustering

vars_famd <- c(
  # Demografía
  "EDAT",
  "SEXE",
  
  # Función / fragilidad
  "BARTHEL",
  "FRAGIL_VIG",
  
  # Nutrición
  "MNA",
  
  # Situación social
  "GIJON",
  "VIU_SOL",
  
  # Neurológicas
  "Demencia",
  "Parkinson",
  "AVC",
  
  # Cardiovasculares
  "IC",
  "Card. Isquémica",
  
  # Respiratorias
  "EPOC",
  "Insuf. Respiratoria",
  
  # Metabólicas / renales
  "Diabetes",
  "IRC",
  
  # Oncológicas
  "Neo activa",
  "Metástasis",
  
  # Salud mental
  "Depressión",
  
  # Accesibilidad
  "distancia_caminando_m"
)




method <- c(
  DOMICILI_INF_TOT = 2,
  TOTAL_VISITS_INF = 2,
  coc_nurse = 2,
  DOMICILI_MF_TOT = 2,
  TOTAL_VISITS_MF = 2,
  coc_physician = 2,
  DOMICILI_CONJ = 2,
  TOTAL_VISITS_CONJF = 2,
  coc_conj = 2,
  SEM_num = 2,
  emergency_visits= 2,
  INGRES_num= 2
)

df_cluster <-df%>%
    transmute(
              ID = ID,
  
             # 🔵 CONTINUAS
              Edad = Edat,
              GMA = GMA_CODE,
              N_cròniques =C_GMA_N_CRONIQUES,
              #TIRS= TIRS_num,
             
  
              # 🟡 CATEGÓRICAS
              sexo = sex_female,
              incontinencia = incontinence_cat,
              #MACA = MACA,
              #Viu_sol = living_alone,
              barthel_cat = barthel_cat
        )%>%
    left_join(DF_work_2[,c(1,17)],
            by="ID")

  mutate(USUA_NIVELL_COBERTURA=as.factor(USUA_NIVELL_COBERTURA))


# imputación

imp<- df_cluster %>%
  select(-ID)
imp <- na.omit(df_cluster)

imp <- imputeFAMD(imp, ncp = 5)

# FAMD
res.famd <- FAMD(imp, ncp = 5, graph = FALSE)
names(df)

res.famd <- FAMD(imp$completeObs, ncp = 5, graph = FALSE)
summary(res.famd)

# Modelo B: edad, GMA y N_cròniques como suplementarias

vars_sup <- c("Edad", "GMA", "N_cròniques")

sup_index <- which(names(imp_famd) %in% vars_sup)
str(imp)

res.famd_B <- FAMD(
  imp,
  ncp = 5,
  graph = FALSE,
  sup.var = sup_index
)
summary(res.famd_B)

# clustering
res.hcpc <- HCPC(res.famd, nb.clust = -1, graph = FALSE)
res.hcpc$data.clust
res.hcpc$desc.var 
fviz_cluster(res.hcpc)

fviz_famd_ind(res.famd,
              habillage = factor(res.hcpc$data.clust$clust),
              addEllipses = TRUE)

df_cluster <- df_cluster %>%
  mutate(cluster = res.hcpc$data.clust$clust)%>%
  left_join(
    df,
    by = "ID")


table(df_cluster$cluster,df_cluster$USUA_UAB_UP)
round(prop.table(table(df_cluster$cluster, df_cluster$organit_atdom_2), margin = 2),2)
tab <- table(df_cluster$cluster, df_cluster$organit_atdom_2)

chisq.test(table(df_cluster$cluster, df_cluster$organit_atdom_2))
DescTools::CramerV(table(df_cluster$cluster, df_cluster$organit_atdom_2))

round(prop.table(tab, margin = 2), 2)
round(prop.table(tab, margin = 1), 2)

res.hcpc$desc.var
res.hcpc$data.clust %>%
  count(clust)

prop.table(table(df_cluster$cluster, df_cluster$centro_grupo, margin = 2))

fviz_contrib(res.famd, choice = "var", axes = 1)
fviz_contrib(res.famd, choice = "var", axes = 2)

desc1cluster<-descrTable(
  cluster ~ .,
  data = df_cluster,
  show.all = T,
  chisq.test.perm = T,
  method = method,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)

export2md(desc1cluster, format = "html")
names(df_cluster)
SEM_num
INGRES_num
Exitus

