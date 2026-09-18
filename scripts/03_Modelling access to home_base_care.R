# 03_00 Model access

Renta_media<- read.csv2(here("data", "external", "renta_media_hogar.csv"),
                        stringsAsFactors = FALSE
)



Renta_media<-Renta_media%>%
  mutate(SEC_CENS = str_sub(as.character(Seccion_TOTAL), -4, -1)
  )

Renta_media <- Renta_media %>%
  mutate(
    Seccio_Censal = paste0(
      as.integer(Distrito),
      str_pad(as.character(Seccion.Censal), width = 3, pad = "0")
    ),
    Seccio_Censal = as.character(as.integer(Seccio_Censal)),
    Media_renta_Hogar = as.numeric(Media_renta_Hogar)
  ) %>%
  select(Seccio_Censal, Media_renta_Hogar)

