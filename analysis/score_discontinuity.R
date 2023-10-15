library(tidyverse)
library(ggplot2)
library(rdrobust)
library(rddensity)

clean_data_dir <- here::here("data_prep", "clean")

palette <- list("white" = "#FAFAFA",
                "light_grey" = "#d9d9d9",
                "dark" = "#0c2230",
                "red" = "#ed195a",
                "blue" = "#1c86ee",
                "dark_blue" = "#00008b",
                "green" = "#00ab5b",
                "dark_green" = "#496F5D",
                "gold" = "#DAA520",
                "purple" = "#880ED4")

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
         Awarded = FALSE)

score_anomaly <- rejected_props %>% filter(score_centered > 0)

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
         share_Euc = ifelse(Forest>0, Eucalyptus / Forest, 0)
  )

test_density <- rddensity(score_discontinuity_df$score_centered, c = 0)
summary(test_density)

plot_density_test <- rdplotdensity(rdd = test_density,
                                   X = score_discontinuity_df$score_centered,
                                   type = "both")

ggplot(score_discontinuity_df, aes(x = score_centered, y = Awarded, color = Awarded)) +
  # Make points small and semi-transparent since there are lots of them
  geom_point(position = position_jitter(width = 0, height = 0.15, seed = 1234)) +
  # Add vertical line
  geom_vline(xintercept = 0) +
  # Add labels
  labs(x = "Centered score", y = "Awarded") +
  # Turn off the color legend, since it's redundant
  guides(color = FALSE)





analysis_df <- score_discontinuity_df %>%
  mutate(years_since_contest = 2021 - ano_concurso,
         area_ha = pixels_count*0.0225,
  )%>%
  filter(received_bonus == 1 | Awarded == FALSE)





plot_bw = 10

conditionalMean_quantile <- analysis_df %>%
  filter(between(score_centered, -plot_bw, plot_bw))%>%
  mutate(bin = ntile(score_centered, n=25)) %>% 
  group_by(bin) %>% 
  summarise(score_centered = mean(score_centered, na.rm = T), 
            Native = mean(Native, na.rm = T), 
            Plantation = mean(Plantation, na.rm = T),
            Forest = mean(Forest, na.rm = T),
            Pine = mean(Pine, na.rm = T),
            Eucalyptus = mean(Eucalyptus, na.rm = T))

se_show = T

  Native <- ggplot() +
    geom_point(data = conditionalMean_quantile, aes(x = score_centered, y = Native), size = 2.5, alpha = 1, color = "black") +
    geom_point(data = analysis_df %>% filter(between(Native, min(conditionalMean_quantile$Native), max(conditionalMean_quantile$Native))), aes(x = score_centered, y = Native), size = .75, shape = 21, alpha = .5, color = palette$dark) +
    # Add a line based on a linear model
    geom_smooth(data = filter(analysis_df, score_centered <= 0), aes(x = score_centered, y = Native, color = Awarded), se = se_show, alpha = 0.2) +
    geom_smooth(data = filter(analysis_df, score_centered > 0), aes(x = score_centered, y = Native, color = Awarded), se = se_show, alpha = 0.2) +
    # Add a line based on conditional mean 
    geom_smooth(data = filter(analysis_df, score_centered <= 0), aes(x = score_centered, y = Native, color = Awarded), method = "lm", linetype = "dashed", se = F) +
    geom_smooth(data = filter(analysis_df, score_centered > 0), aes(x = score_centered, y = Native, color = Awarded), method = "lm", linetype = "dashed", se = F) +
    geom_vline(xintercept = 0) +
    labs(x = "Centered score", y = "Native forest")+
    theme_minimal()+
    xlim(-plot_bw, plot_bw)
  Native
  
  Eucalyptus <- ggplot() +
    geom_point(data = conditionalMean_quantile, aes(x = score_centered, y = Eucalyptus), size = 2.5, alpha = 1, color = "black") +
    geom_point(data = analysis_df %>% filter(between(Eucalyptus, min(conditionalMean_quantile$Eucalyptus), max(conditionalMean_quantile$Eucalyptus))), aes(x = score_centered, y = Eucalyptus), size = .75, shape = 21, alpha = .5, color = palette$dark) +
    # Add a line based on a linear model
    geom_smooth(data = filter(analysis_df, score_centered <= 0), aes(x = score_centered, y = Eucalyptus, color = Awarded), se = se_show, alpha = 0.2) +
    geom_smooth(data = filter(analysis_df, score_centered > 0), aes(x = score_centered, y = Eucalyptus, color = Awarded), se = se_show, alpha = 0.2) +
    # Add a line based on conditional mean 
    geom_smooth(data = filter(analysis_df, score_centered <= 0), aes(x = score_centered, y = Eucalyptus, color = Awarded), method = "lm", linetype = "dashed", se = F) +
    geom_smooth(data = filter(analysis_df, score_centered > 0), aes(x = score_centered, y = Eucalyptus, color = Awarded), method = "lm", linetype = "dashed", se = F) +
    geom_vline(xintercept = 0) +
    labs(x = "Centered score", y = "Eucalyptus forest")+
    theme_minimal()+
    xlim(-plot_bw, plot_bw)
  Eucalyptus
  
  Pine <- ggplot() +
    geom_point(data = conditionalMean_quantile, aes(x = score_centered, y = Pine), size = 2.5, alpha = 1, color = "black") +
    geom_point(data = analysis_df %>% filter(between(Pine, min(conditionalMean_quantile$Pine), max(conditionalMean_quantile$Pine))), aes(x = score_centered, y = Pine), size = .75, shape = 21, alpha = .5, color = palette$dark) +
    # Add a line based on a linear model
    geom_smooth(data = filter(analysis_df, score_centered <= 0), aes(x = score_centered, y = Pine, color = Awarded), se = se_show, alpha = 0.2) +
    geom_smooth(data = filter(analysis_df, score_centered > 0), aes(x = score_centered, y = Pine, color = Awarded), se = se_show, alpha = 0.2) +
    # Add a line based on conditional mean 
    geom_smooth(data = filter(analysis_df, score_centered <= 0), aes(x = score_centered, y = Pine, color = Awarded), method = "lm", linetype = "dashed", se = F) +
    geom_smooth(data = filter(analysis_df, score_centered > 0), aes(x = score_centered, y = Pine, color = Awarded), method = "lm", linetype = "dashed", se = F) +
    geom_vline(xintercept = 0) +
    labs(x = "Centered score", y = "Pine forest")+
    theme_minimal()+
    xlim(-plot_bw, plot_bw)
  Pine



main_outcomes <- c("Native", "Plantation", "Pine", "Eucalyptus")

preferred_bw = "optimal"
preferred_method = "conventional"

results <- data.frame()
for(o in main_outcomes){
  

  this_df <- analysis_df %>%
    rename(out = o)%>%
    drop_na(score_centered)
  
  
  my_covs<- cbind(
    this_df$years_since_contest
   , this_df$Trees_2000
   , this_df$Development_2000
   , this_df$Grassland_2000
   , this_df$Water_2000
   , this_df$Crop_2000
  )
  
  
  rd <- rdrobust(y = this_df$out, x = this_df$score_centered, c = 0
                      , covs = my_covs
  )
  
  rd_bw = rd$bws[1]
  
  results <- data.frame(
    "outcome" = o,
    rd$coef,
    rd$se,
    rd$pv,
    "bandwidth" = rd_bw,
    "bandwidth_method" = "optimal",
    "method" = c("conventional", "bias-corrected", "robust")
  )%>%
    rbind(results)
  
  
  rd2 <- rdrobust(y = this_df$out, x = this_df$score_centered, c = 0
                 , covs = my_covs
                 , h = rd_bw*2
  )
  rd2_bw = rd2$bws[1]
  
  results <- data.frame(
    "outcome" = o,
    rd2$coef,
    rd2$se,
    rd2$pv,
    "bandwidth" = rd2_bw,
    "bandwidth_method" = "2 x optimal",
    "method" = c("conventional", "bias-corrected", "robust")
  )%>%
    rbind(results)
  
}



rd_results <- results %>%
  rename(estimate = 2,
         se = 3,
         pval = 4)

preferred_results <- rd_results %>%
  filter(bandwidth_method == preferred_bw,
         method == preferred_method)


source("analysis/schart.R")

table(rd_results$outcome)

bw_order <- c("optimal", "2 x optimal")
method_order <- c("conventional", "bias-corrected",  "robust")

spec_results <- rd_results %>%
  group_by(outcome)%>%
  arrange(match(outcome, main_outcomes)
          , match(bandwidth_method, bw_order)
          , match(method, method_order)
          )%>%
  ungroup

preferred_rows <- which(spec_results$bandwidth_method == preferred_bw &
                          spec_results$method == preferred_method)

rd_spec_results <- spec_results %>%
  mutate(true = TRUE,
         trueb = TRUE
  )%>%
  pivot_wider(names_from = method, values_from = true, values_fill = FALSE)%>%
  pivot_wider(names_from = bandwidth_method, values_from = trueb, values_fill = FALSE)%>%
  select(estimate, se, everything(), - pval, - bandwidth
         )%>%
  select(-outcome)%>%
  as.data.frame()


par(oma=c(1,0,1,1))

Labels = list("Method:" = method_order,
              "Bandwidth:" = bw_order)

schart(rd_spec_results, 
       labels = Labels,
       index.est = 1, index.se=2, col.est = c("black", "royalblue"),
       ci= c(.9, .95),
       n = 6,
       highlight = preferred_rows, 
       bg.dot=c("black", "grey95", "white", "royalblue"),
       col.dot=c("black", "grey95", "white", "royalblue"),
       axes = FALSE
) # make some room at the bottom
text(x=3 , y=6100, "Native", col="black", font=2)
text(x=10.5 , y=6100, "Plantation", col="black", font=2)
text(x=17.5 , y=6100, "Pine", col="black", font=2)
text(x=24.5 , y=6100, "Eucalyptus", col="black", font=2)

  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Covariate continuity and pre-treatment plantation/native spec chart
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cov_outcomes <- c("Superficie", "Monto Solicitado", "area_ha", "Water"
                        , "Plantation_2001", "Forest_2001" 
                        )

cov_results <- data.frame()
for(o in cov_outcomes){
  
  
  this_df <- analysis_df %>%
    rename(out = o)%>%
    drop_na(score_centered)
  
  
  my_covs<- cbind(
    this_df$years_since_contest
    , this_df$Trees_2000
    , this_df$Development_2000
    , this_df$Grassland_2000
    , this_df$Water_2000
    , this_df$Crop_2000
  )
  
  rd <- rdrobust(y = this_df$out, x = this_df$score_centered, c = 0
                 , covs = my_covs
  )
  
  cov_results <- data.frame(
    "outcome" = o,
    rd$coef,
    rd$se,
    rd$pv,
    "bandwidth" = rd_bw,
    "bandwidth_method" = "optimal",
    "method" = c("conventional", "bias-corrected", "robust")
  )%>%
    rbind(cov_results)
}

cov_results <- cov_results %>%
  rename(estimate = 2,
         se = 3,
         pval = 4)

preferred_cov_results <- cov_results %>%
  filter(bandwidth_method == preferred_bw,
         method == preferred_method)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Placebo using those that do not submit management plan
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

placebo_analysis_df <- score_discontinuity_df %>%
  mutate(years_since_contest = 2021 - ano_concurso,
         area_ha = pixels_count*0.0225,
  )%>%
  filter(received_bonus == 0 | Awarded == FALSE)

placebo_results <- data.frame()
for(o in main_outcomes){
  
  
  this_df <- placebo_analysis_df %>%
    rename(out = o)%>%
    drop_na(score_centered)
  
  
  my_covs<- cbind(
    this_df$years_since_contest
    , this_df$Trees_2000
    , this_df$Development_2000
    , this_df$Grassland_2000
    , this_df$Water_2000
    , this_df$Crop_2000
  )
  
  rd <- rdrobust(y = this_df$out, x = this_df$score_centered, c = 0
                 , covs = my_covs
  )
  
  placebo_results <- data.frame(
    "outcome" = o,
    rd$coef,
    rd$se,
    rd$pv,
    "bandwidth" = rd_bw,
    "bandwidth_method" = "optimal",
    "method" = c("conventional", "bias-corrected", "robust")
  )%>%
    rbind(placebo_results)
}

placebo_results <- placebo_results %>%
  rename(estimate = 2,
         se = 3,
         pval = 4)

preferred_placebo_results <- placebo_results %>%
  filter(bandwidth_method == preferred_bw,
         method == preferred_method)
