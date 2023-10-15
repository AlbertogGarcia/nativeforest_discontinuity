library(tidyverse)
library(ggplot2)
library(rdrobust)
library(rddensity)

clean_data_dir <- here::here("data_prep", "clean")

fig_dir <- here::here("score_figs")
results_dir <- here::here("score_results")

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


score_discontinuity_df <- readRDS(paste0(clean_data_dir, "/score_discontinuity_data.rds"))

test_density <- rddensity(score_discontinuity_df$score_centered, c = 0)
summary(test_density)

test_density_pval <- test_density$test$p_jk

plot_density_test <- rdplotdensity(rdd = test_density,
                                   X = score_discontinuity_df$score_centered,
                                   type = "both")

plot_density_test$Estplot + 
  annotate(geom="text", x=-10, y=0.0425, label = paste0("p-value: ", round(test_density_pval, digits = 3)))+xlab("Centered score")

ggsave(filename = paste0(fig_dir, "/density_test_plot.png"), width = 6, height = 5, units='in')

ggplot(score_discontinuity_df, aes(x = score_centered, y = Awarded, color = Awarded)) +
  # Make points small and semi-transparent since there are lots of them
  geom_point(position = position_jitter(width = 0, height = 0.15, seed = 1234)) +
  # Add vertical line
  geom_vline(xintercept = 0) +
  # Add labels
  labs(x = "Centered score", y = "Awarded") +
  # Turn off the color legend, since it's redundant
  guides(color = FALSE)+
  theme_minimal()

ggsave(filename = paste0(fig_dir, "/sharp_discontinuity.png"), width = 7, height = 5, units='in')



analysis_df <- score_discontinuity_df %>%
  filter(received_bonus == 1 | Awarded == FALSE)





plot_bw = 5

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
  ggsave(filename = paste0(fig_dir, "/Native_condmean.png"), width = 7, height = 5, units='in')
  
  
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
    labs(x = "Centered score", y = "Eucalyptus")+
    theme_minimal()+
    xlim(-plot_bw, plot_bw)
  Eucalyptus
  ggsave(filename = paste0(fig_dir, "/Eucalyptus_condmean.png"), width = 7, height = 5, units='in')
  
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
    labs(x = "Centered score", y = "Pine")+
    theme_minimal()+
    xlim(-plot_bw, plot_bw)
  Pine
  ggsave(filename = paste0(fig_dir, "/Pine_condmean.png"), width = 7, height = 5, units='in')
  
  Plantation <- ggplot() +
    geom_point(data = conditionalMean_quantile, aes(x = score_centered, y = Plantation), size = 2.5, alpha = 1, color = "black") +
    geom_point(data = analysis_df %>% filter(between(Plantation, min(conditionalMean_quantile$Plantation), max(conditionalMean_quantile$Plantation))), aes(x = score_centered, y = Pine), size = .75, shape = 21, alpha = .5, color = palette$dark) +
    # Add a line based on a linear model
    geom_smooth(data = filter(analysis_df, score_centered <= 0), aes(x = score_centered, y = Plantation, color = Awarded), se = se_show, alpha = 0.2) +
    geom_smooth(data = filter(analysis_df, score_centered > 0), aes(x = score_centered, y = Plantation, color = Awarded), se = se_show, alpha = 0.2) +
    # Add a line based on conditional mean 
    geom_smooth(data = filter(analysis_df, score_centered <= 0), aes(x = score_centered, y = Plantation, color = Awarded), method = "lm", linetype = "dashed", se = F) +
    geom_smooth(data = filter(analysis_df, score_centered > 0), aes(x = score_centered, y = Plantation, color = Awarded), method = "lm", linetype = "dashed", se = F) +
    geom_vline(xintercept = 0) +
    labs(x = "Centered score", y = "Plantation")+
    theme_minimal()+
    xlim(-plot_bw, plot_bw)
  Plantation
  ggsave(filename = paste0(fig_dir, "/Plantation_condmean.png"), width = 7, height = 5, units='in')
  
  
main_outcomes <- c("Native", "Plantation", "Pine", "Eucalyptus", "Forest"
                   )

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
    "Obs" = sum(rd$N),
    "eff_obs" = sum(rd$N_h),
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
    "Obs" = sum(rd2$N),
    "eff_obs" = sum(rd2$N_h),
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
spec_chart_outcomes <- c("Native", "Plantation")

spec_results <- rd_results %>%
  group_by(outcome)%>%
  filter(outcome %in% spec_chart_outcomes)%>%
  arrange(match(outcome, spec_chart_outcomes)
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
  select(estimate, se, everything(), - pval, - bandwidth, - Obs, - eff_obs
         )%>%
  select(-outcome)%>%
  as.data.frame()


Labels = list("Method:" = method_order,
              "Bandwidth:" = bw_order)


png(filename = paste0(fig_dir, "/schart_main.png"), width = 6, height = 5, units='in', res = 400)
par(oma=c(1,0,1,1))

schart(rd_spec_results, 
       labels = Labels,
       index.est = 1, index.se=2, col.est = c("black", "royalblue"), col.est2=c("grey80","grey80"),
       ci= c(.9, .95),
       n = 6,
       highlight = preferred_rows, 
       bg.dot=c("black", "grey95", "white", "royalblue"),
       col.dot=c("black", "grey95", "white", "royalblue"),
       axes = FALSE
) # make some room at the bottom
text(x=3 , y=6100, "Native", col="black", font=2)
text(x=10 , y=6100, "Plantation", col="black", font=2)

# Close the png file
dev.off()
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
  
  rd_bw = rd$bws[1]
  
  cov_results <- data.frame(
    "outcome" = o,
    rd$coef,
    rd$se,
    rd$pv,
    "Obs" = sum(rd$N),
    "eff_obs" = sum(rd$N_h),
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
  
  rd_bw = rd$bws[1]
  
  placebo_results <- data.frame(
    "outcome" = o,
    rd$coef,
    rd$se,
    rd$pv,
    "Obs" = sum(rd$N),
    "bandwidth" = rd_bw,
    "eff_obs" = sum(rd$N_h),
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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(janitor)
paper_outcomes_main <- c("Native", "Plantation", "Pine", "Eucalyptus")

paper_results_main <- preferred_results %>%
  filter(outcome %in% paper_outcomes_main)%>%
  arrange(match(outcome, paper_outcomes_main))%>%
  mutate(
    stars = ifelse(between(abs(estimate/se), 1.645, 1.96), "*",
                   ifelse(between(abs(estimate/se), 1.96, 2.58), "**",
                          ifelse(abs(estimate/se) >= 2.58, "***", "")
                   )
    )
  )%>%
  mutate_at(vars(estimate, se, bandwidth), ~ round(., digits = 3))%>%
  mutate(se = paste0("(", se, ")"),
         estimate = paste0(estimate, stars))%>%
  select(outcome, estimate, se, bandwidth, eff_obs)%>%
  t() %>%
  row_to_names(row_number = 1) %>%
  as.data.frame()
row.names(paper_results_main) <- c("LATE", " ", "Optimal bandwidth", "Effective # obs.")

kbl(paper_results_main,
    #format = "latex",
    booktabs = T,
   # caption = "",
   # col.names = c("Canopy", "Loss (Acres/year)", "Gain (Acres/year)"),
    align = c("l", "c", "c", "c"),
    label = "results-score-main"
)%>%
  kableExtra::row_spec(2, hline_after = TRUE)%>%
  add_header_above(c(" " = 1, "Outcome" = 4))%>%
  footnote(general = "* p<0.1, ** p<0.05, *** p<0.01")%>%
  kable_styling(latex_options = c("hold_position"))#%>% 
#  kableExtra::save_kable(paste0(results_dir, "/results_score_main.tex"))


cov_outcomes <- c("Superficie", "Monto Solicitado", "area_ha", "Water"
                  , "Plantation_2001", "Forest_2001" 
)
cov_names <- c("Subsidy area (ha)", "Subsidy amount (UTM)", "Property area (ha)", "Water"
                  , "Plantation forest (2001)", "Native forest (2001)" 
)


paper_results_cov <- preferred_cov_results %>%
  filter(outcome %in% cov_outcomes)%>%
  arrange(match(outcome, cov_outcomes))%>%
  mutate(
    stars = ifelse(between(abs(estimate/se), 1.645, 1.96), "*",
                   ifelse(between(abs(estimate/se), 1.96, 2.58), "**",
                          ifelse(abs(estimate/se) >= 2.58, "***", "")
                   )
    )
  )%>%
  mutate(lower = round(estimate - 1.96*se, digits = 3),
         upper = round(estimate + 1.96*se, digits = 3),
         ci = paste0("[", lower, ", ", upper, "]"))%>%
  mutate_at(vars(estimate, se, bandwidth), ~ round(., digits = 3))%>%
  mutate(estimate = paste0(estimate, stars),
         outcome = c(cov_names, outcome)[match(outcome, c(cov_outcomes, outcome))])%>%
  select(outcome, estimate, pval, ci, bandwidth, eff_obs)

kbl(paper_results_cov,
    #  format = "latex",
    booktabs = T,
    row.names = FALSE,
    # caption = "",
     col.names = c("Variable", "RD estimate", "p-value", "Conf. Int.", "MSE-optimal BW", "Eff. # Obs."),
    align = c("l", "c", "c", "c", "c", "c"),
    label = "results-score-covs"
)%>%
  kableExtra::row_spec(2, hline_after = TRUE)%>%
 # add_header_above(c(" " = 1, "Outcome" = 6))%>%
  footnote(general = "* p<0.1, ** p<0.05, *** p<0.01")%>%
  kable_styling(latex_options = c("hold_position"))#%>% 
 # kableExtra::save_kable(paste0(results_dir, "/results_score_covs.tex"))


paper_results_placebo <- preferred_placebo_results %>%
  filter(outcome %in% paper_outcomes_main)%>%
  arrange(match(outcome, paper_outcomes_main))%>%
  mutate(
    stars = ifelse(between(abs(estimate/se), 1.645, 1.96), "*",
                   ifelse(between(abs(estimate/se), 1.96, 2.58), "**",
                          ifelse(abs(estimate/se) >= 2.58, "***", "")
                   )
    )
  )%>%
  mutate(lower = round(estimate - 1.96*se, digits = 3),
          upper = round(estimate + 1.96*se, digits = 3),
          ci = paste0("[", lower, ", ", upper, "]"))%>%
  mutate_at(vars(estimate, se, bandwidth), ~ round(., digits = 3))%>%
  mutate(estimate = paste0(estimate, stars),
         outcome = c(cov_names, outcome)[match(outcome, c(cov_outcomes, outcome))])%>%
  select(outcome, estimate, pval, ci, bandwidth, eff_obs)
