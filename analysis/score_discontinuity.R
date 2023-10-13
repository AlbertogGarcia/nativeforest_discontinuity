library(tidyverse)
library(ggplot2)
library(rdrobust)

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
  select(score_centered, Awarded, my_ID, score, region, `Contest type`, objectivo_manejo, ano_concurso, Comuna, received_bonus, submitted_management_plan, 
         landcover_cols, matches("2000"))
  
  
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
         landcover_cols, matches("2000"))%>%
  inner_join(score_cutoffs, by = c("Contest type", "ano_concurso"))%>%
  mutate(score_centered = score - min_awarded_score,
         Awarded = FALSE)

score_anomaly <- rejected_props %>% filter(score_centered > 0)

ggplot(data = rejected_props, aes(x = score_centered)) +
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_histogram() +
  theme_minimal()


score_discontinuity_df <- bind_rows(rejected_props, 
                                    enrolled_props %>% filter(
                                    #  submitted_management_plan ==1
                                      received_bonus == 1
                                                              )
                                    )%>%
  mutate(across(landcover_cols, ~ . / pixels_count, .names = "pct_{col}"),
         share_Native = ifelse(Forest>0, Native / Forest, 0),
         share_Plantation = ifelse(Forest>0, Plantation / Forest, 0),
         share_Pine = ifelse(Forest>0, Pine / Forest, 0),
         share_Euc = ifelse(Forest>0, Eucalyptus / Forest, 0)
  )



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
  mutate(years_since_contest = 2021 - ano_concurso)


bw = 8

se_show = F
se_lm_show = T

Native <- ggplot(analysis_df, aes(x = score_centered, y = Native, color = Awarded)) +
  geom_point(size = 0.75, alpha = 0.25) +
  # Add a line based on a linear model for the people scoring 0 or less
  geom_smooth(data = filter(analysis_df, score_centered <= 0), se = se_show) +
  # Add a line based on a linear model for the people scoring more than 0
  geom_smooth(data = filter(analysis_df, score_centered > 0), se = se_show) +
  # Add a line based on a linear model for the people scoring 0 or less
  geom_smooth(data = filter(analysis_df, score_centered <= 0), method = "lm", linetype = "dashed", se = se_lm_show) +
  # Add a line based on a linear model for the people scoring more than 0
  geom_smooth(data = filter(analysis_df, score_centered > 0), method = "lm", linetype = "dashed", se = se_lm_show) +
  geom_vline(xintercept = 0) +
  labs(x = "Centered score", y = "Native", color = "Awarded")+
  theme_minimal()+
  xlim(-bw, bw) 
Native

Native_share <- ggplot(analysis_df, aes(x = score_centered, y = share_Native, color = Awarded)) +
  geom_point(size = 0.75, alpha = 0.25) +
  # Add a line based on a linear model for the people scoring 0 or less
  geom_smooth(data = filter(analysis_df, score_centered <= 0), se = se_show) +
  # Add a line based on a linear model for the people scoring more than 0
  geom_smooth(data = filter(analysis_df, score_centered > 0), se = se_show) +
  # Add a line based on a linear model for the people scoring 0 or less
  geom_smooth(data = filter(analysis_df, score_centered <= 0), method = "lm", linetype = "dashed", se = se_lm_show) +
  # Add a line based on a linear model for the people scoring more than 0
  geom_smooth(data = filter(analysis_df, score_centered > 0), method = "lm", linetype = "dashed", se = se_lm_show) +
  geom_vline(xintercept = 0) +
  labs(x = "Centered score", y = "Share of Forest that is Native", color = "Awarded")+
  theme_minimal()+
  xlim(-bw, bw) 
Native_share

Grassland <- ggplot(analysis_df, aes(x = score_centered, y = `Grassland/Ag`, color = Awarded)) +
  geom_point(size = 0.75, alpha = 0.25) +
  # Add a line based on a linear model for the people scoring 0 or less
  geom_smooth(data = filter(analysis_df, score_centered <= 0), se = se_show) +
  # Add a line based on a linear model for the people scoring more than 0
  geom_smooth(data = filter(analysis_df, score_centered > 0), se = se_show) +
  # Add a line based on a linear model for the people scoring 0 or less
  geom_smooth(data = filter(analysis_df, score_centered <= 0), method = "lm", linetype = "dashed", se = se_lm_show) +
  # Add a line based on a linear model for the people scoring more than 0
  geom_smooth(data = filter(analysis_df, score_centered > 0), method = "lm", linetype = "dashed", se = se_lm_show) +
  geom_vline(xintercept = 0) +
  labs(x = "Centered score", y = "Grassland", color = "Awarded")+
  theme_minimal()+
  xlim(-bw, bw) 
Grassland



outcomes <- c("Forest", "Plantation", "Native", "Pine", "Eucalyptus", "Grassland/Ag", "Orchard/Ag", "Bare"
              )


results <- data.frame()
for(o in outcomes){

  this_df <- analysis_df %>%
    rename(out = o)%>%
    drop_na(score_centered)
  
  
  my_covs<- cbind(
    this_df$years_since_contest
    , this_df$Trees_2000
   # , this_df$Development_2000
   # , this_df$Grassland_2000
   # , this_df$Water_2000
   # , this_df$Crop_2000
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

source("analysis/schart.R")

table(rd_results$outcome)

spec_chart_outcomes <- c("Pine", "Eucalyptus", "Native")
bw_order <- c("optimal", "2 x optimal")
method_order <- c("bias-corrected", "conventional", "robust")

rd_spec_results <- rd_results %>%
  filter(outcome %in% spec_chart_outcomes)%>%
  mutate(true = TRUE,
         trueb = TRUE
         )%>%
  group_by(outcome)%>%
  arrange(match(outcome, spec_chart_outcomes)
          , match(bandwidth_method, bw_order)
          , match(method, method_order)
          )%>%
  ungroup%>%
  pivot_wider(names_from = method, values_from = true, values_fill = FALSE)%>%
  pivot_wider(names_from = bandwidth_method, values_from = trueb, values_fill = FALSE)%>%
  select(estimate, se, everything(), - pval, - bandwidth, - outcome)%>%
  as.data.frame()


my_labels <- c(
  "Method:" = c("optimal", "2 x optimal"),
  "Bandwidth:" = c("bias-corrected", "conventional", "robust")
)

par(oma=c(1,0,1,1))


schart(rd_spec_results,labels = my_labels, index.est = 1, index.se=2, col.est = c("black", "royalblue"),
       n = 6,
       bg.dot=c("black", "grey95", "white", "royalblue"),
       col.dot=c("black", "grey95", "white", "royalblue"),
       axes = FALSE
) # make some room at the bottom
text(x=3 , y=5300, "Pine", col="black", font=2)
text(x=10 , y=5300, "Eucalyptus", col="black", font=2)
text(x=17.5 , y=5300, "Native", col="black", font=2)

  
Native_coeff_plot <- ggplot(rd_results %>% filter(outcome == "Native"),
                            aes(x = bandwidth, color = notes, ymin = estimate - 1.96*se, ymax = estimate + 1.96*se))+
  geom_point(position = position_dodge(width=0.9), aes(y = estimate))+
  geom_errorbar(position = position_dodge(width=0.9), width = 05)+
  theme_minimal()
Native_coeff_plot

my_covs<- cbind(
  analysis_df$years_since_contest
  , analysis_df$Trees_2000
  # , analysis_df$Development_2000
  # , analysis_df$Grassland_2000
  # , analysis_df$Water_2000
  # , analysis_df$Crop_2000
)

asdf <- rdplot(y = analysis_df$Native, x = analysis_df$score_centered, c = 0
               , covs = my_covs
)

asdf$rdplot +
  labs(x = "Centered score", y = "Native")+
  xlim(-6, 8)+
  ylim(0, 40000)

