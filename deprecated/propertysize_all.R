library(tidyverse)
library(rio)
library(readxl)

clean_data_dir <- here::here("data_prep", "clean")

select <- dplyr::select
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Read in and combine Native Forest Law admin data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# fcn to remove accents
accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}

# xls files downloaded from CONAF
#projects
proyecto_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/proyecto.xlsx"))

#properties and coordinates
predio_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/predio.xlsx"))
#owners
propietario_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/propietario.xlsx"))
#stands
rodal_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/rodal.xlsx"))
#activities
actividades_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/actividad.xlsx"))

coordinadas_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/coordinadas_predio.xlsx"))


property_df <- proyecto_df %>%
  full_join(predio_df, by = "rptpro_id") %>%
  full_join(propietario_df, by = "rptpre_id")%>%
  mutate(`Contest type` = ifelse(rptpro_tipo_concurso == "Otros Interesados", "Other interested", "Smallholder"),
         received_bonus = as.numeric(ifelse(rptpro_tiene_bonificacion_saff == "No" | is.na(rptpro_tiene_bonificacion_saff) , 0, 1)),
         submitted_management_plan = as.numeric(ifelse(rptpro_tiene_plan_saff == "No" | is.na(rptpro_tiene_plan_saff), 0, 1))
  )%>%
  separate(rptpre_rol, into = c("rol1", "rol2", "additional_props"), remove = FALSE)%>%
  mutate(ROL = paste0(rol1, "-", rol2),
         additional_props = ifelse(is.na(additional_props), 0, 1))

export(property_df, paste0(clean_data_dir, "/property_df.rds"))


activity_df <- property_df %>%
  left_join(rodal_df, by = "rptpre_id") %>%
  left_join(actividades_df, by = "rptro_id") %>%
  mutate(indic = 1) %>%
  pivot_wider(id_cols = c("rptpro_id", "rptpro_ano"),
        names_from = rptac_tipo, 
        values_from = indic,
        values_fill = list(indic = 0),
        values_fn = function(x){as.integer(length(x) > 0)})

NFL_df <- property_df %>%
  full_join(activity_df, by = c("rptpro_id", "rptpro_ano")) 

export(NFL_df, paste0(clean_data_dir, "/NFL_df.rds"))


regions_200 <- c(5,6,7,8,9, 10,14)
regions_500 <- c(1, 2, 3, 4, 15)
regions_800 <- c(11, 12)

discontinuity_all <- NFL_df %>%
  rename(propertysize_reported = rptpre_superficie_predial)%>%
  mutate(received_bonus = as.numeric(ifelse(rptpro_tiene_bonificacion_saff == "Si", 1, 0)),
         size_cutoff = ifelse(
           rptpro_numero_region %in% regions_200, 200,
           ifelse(rptpro_numero_region %in% regions_800, 800, 500)
         ),
         size_centered = propertysize_reported - size_cutoff,
         below_cutoff = propertysize_reported <= size_cutoff,
         smallholder = ifelse(`Contest type` == "Other Interested", 0, 1),
         price_per_hectare = rptpro_monto_total/propertysize_reported,
         proportion_bonusarea = rptpro_superficie/propertysize_reported
  )%>%
  distinct(rptpro_id, rptpro_ano, rptpre_id, ROL, .keep_all = TRUE)

export(discontinuity_all, paste0(clean_data_dir, "/discontinuity_all.rds"))