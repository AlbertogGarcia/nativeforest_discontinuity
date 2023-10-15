library(tidyverse)
library(ggplot2)
library(rdrobust)
library(rddensity)
library(rio)

clean_data_dir <- here::here("data_prep", "clean")


select <- dplyr::select


property_df <- readRDS(paste0(clean_data_dir, "/property_df.rds"))%>%
  filter(!duplicated(paste0(pmax(rptpro_id, rptpre_id, rptpro_ano, rptpro_puntaje, rptpre_comuna), pmin(rptpro_id, rptpre_id, rptpro_ano, rptpro_puntaje, rptpre_comuna))))


no_asignados_all <- readRDS(paste0(clean_data_dir, "/no_asignados_all.rds"))%>%
  mutate(`Contest type`= ifelse(`Tipo Concurso`=="Otros Interesados", "Other interested", "Smallholder"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### determine score cutoff for relevant contest years
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

score_cutoffs <- property_df %>%
  rename(ano_concurso = rptpro_ano)%>%
  filter(ano_concurso %in% no_asignados_all$`Año Concurso`)%>%
  filter(`Contest type` == "Other interested" & ano_concurso != 2014 | 
           ano_concurso == 2014 & `Contest type` != "Other interested" & grepl('Segundo', rptpro_nombre_concurso)
  )%>%
  group_by(ano_concurso, `Contest type`)%>%
  mutate(min_awarded_score = min(rptpro_puntaje, na.rm = T))%>%
  filter(!duplicated(paste0(pmax(ano_concurso, `Contest type`), pmin(ano_concurso, `Contest type`))))%>%
  select(ano_concurso, `Contest type`, min_awarded_score)
score_cutoffs


landcover_rejected <- readRDS(paste0(clean_data_dir, "/landcover_rejected.rds"))%>%
  mutate(Plantation = Pine + Eucalyptus,
         Forest = Plantation + Native)

landcover_enrolled <- readRDS(paste0(clean_data_dir, 
                                     "/landcover_enrolled_rol.rds"
))%>%
  mutate(Plantation = Pine + Eucalyptus,
         Forest = Plantation + Native)

landcover_cols <- c("Forest", "Plantation", "Urban", "Water", "Grassland/Ag", "Pine", "Native", "Bare", "Shrubland", "Eucalyptus","Orchard/Ag" , "pixels_count")

enrolled_props <- landcover_enrolled %>%
  inner_join(property_df, by = c("rptpro_id", "rptpre_id", "rptpro_ano"))%>%
  rowid_to_column(var = "my_ID")%>%
  rename(score = rptpro_puntaje,
         region = rptpre_region,
         ano_concurso = rptpro_ano,
         Comuna = rptpre_comuna,
         objectivo_manejo = rptpro_objetivo_manejo)%>%
  inner_join(score_cutoffs, by = c("Contest type", "ano_concurso"))%>%
  filter(`Contest type` == "Other interested" & ano_concurso != 2014 | 
           ano_concurso == 2014 & `Contest type` != "Other interested" & grepl('Segundo', rptpro_nombre_concurso)
  )%>%
  mutate(score_centered = score - min_awarded_score,
         Awarded = TRUE)%>%
  rename(Superficie = rptpro_superficie, 
         `Monto Solicitado` = rptpro_monto_total)%>%
  select(score_centered, Awarded, my_ID, score, region, `Contest type`, objectivo_manejo, ano_concurso, Comuna, 
         received_bonus, submitted_management_plan, Superficie, `Monto Solicitado`,
         landcover_cols, matches("2000"), matches("2001"))


ggplot(data = enrolled_props, aes(x = score_centered)) +
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_histogram() +
  theme_minimal()


rejected_props <- landcover_rejected %>%
  inner_join(no_asignados_all %>% rename(Postlcn = `Postulación`, ano_concurso = `Año Concurso`)
             , by = c("Postlcn", "Comuna", "ano_concurso")
  )%>%
  rowid_to_column(var = "my_ID")%>%
  rename(score = Puntaje,
         region = Región,
         objectivo_manejo = `Objetivo Manejo`)%>%
  select(my_ID, score, region, `Contest type`, objectivo_manejo, ano_concurso, Comuna, 
         Superficie, `Monto Solicitado`,
         landcover_cols, matches("2000"), matches("2001"))%>%
  inner_join(score_cutoffs, by = c("Contest type", "ano_concurso"))%>%
  mutate(score_centered = score - min_awarded_score,
         Awarded = FALSE)%>% 
  filter(score_centered <= 0)

ggplot(data = rejected_props, aes(x = score_centered)) +
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_histogram() +
  theme_minimal()


score_discontinuity_df <- bind_rows(rejected_props, 
                                    enrolled_props)%>%
  mutate(across(landcover_cols, ~ . / pixels_count, .names = "pct_{col}"),
         share_Native = ifelse(Forest>0, Native / Forest, 0),
         share_Plantation = ifelse(Forest>0, Plantation / Forest, 0),
         share_Pine = ifelse(Forest>0, Pine / Forest, 0),
         share_Euc = ifelse(Forest>0, Eucalyptus / Forest, 0),
         years_since_contest = 2021 - ano_concurso,
         area_ha = pixels_count*0.0225
  )

export(score_discontinuity_df, paste0(clean_data_dir, "/score_discontinuity_data.rds"))
