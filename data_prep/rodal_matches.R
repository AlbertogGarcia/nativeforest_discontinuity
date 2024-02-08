library(tidyverse)
library(sf)
library(readxl)
CONAF_dir <- here::here("CONAF")
clean_data_dir <- here::here("data_prep", "clean")

select <- dplyr::select


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### read in shapefile
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# create list of all files with .shp extension in PROPIEDADES_RURALES folder
rodales_migrados <- st_read(paste0(CONAF_dir, "/data/original/rodales_migrados_desde_el_saff/rodales_migrados_desde_el_saff.shp"))%>%
  rename(Soli_numero = 1,
         "Nro_resolucion" = 2)

length(unique(rodales_migrados$NUMERO.RES))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### read in activity data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
actividades_BN <- read_xlsx(paste0(CONAF_dir, "/data/original/actividades BN resoluciones 2008 a diciembre 2022.xlsx"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### matching
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# try merging based on numero res

test <- rodales_migrados %>%
  left_join(actividades_BN, by = c("Nro_resolucion", "Soli_numero"))%>%
  select(ACTIVIDAD, `AñO.EJECU`, NOMBRE.ROD, SUPERFICIE, Nro_resolucion, `Cod. Predio`, Ano_Ejecucion, Numero_Rodal, Super.Rodal
       #  , everything()
         )%>%
  mutate(superficie_difference = SUPERFICIE - Super.Rodal,
         execution_year_difference = as.numeric(`AñO.EJECU`) - Ano_Ejecucion,
         as.numeric(gsub("([0-9]+).*$", "\\1", years)))%>%
  group_by(ACTIVIDAD, `AñO.EJECU`, NOMBRE.ROD, SUPERFICIE, Nro_resolucion, `Cod. Predio`)%>%
  filter(execution_year_difference == 0 | is.na(execution_year_difference) |
         superficie_difference == min(superficie_difference, na.rm = T)
         )


