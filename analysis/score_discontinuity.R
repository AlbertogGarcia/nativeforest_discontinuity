library(tidyverse)
library(ggplot2)
library(janitor)
library(kableExtra)
library(rdrobust)
library(rddensity)

clean_data_dir <- here::here("data_prep", "clean")

fig_dir <- here::here("figs")
results_dir <- here::here("results")

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
  guides(color = FALSE, scale = "none")+
  theme_minimal()

ggsave(filename = paste0(fig_dir, "/sharp_discontinuity.png"), width = 7, height = 5, units='in')



bonus_dta <- score_discontinuity_df %>%
  filter(received_bonus == 1 | Awarded == FALSE)%>%
  mutate(data_group = "Received bonus")

neverbonus_dta <- score_discontinuity_df %>%
  filter(received_bonus == 0 | Awarded == FALSE) %>%
  mutate(data_group = "Never received bonus")

all_dta <- score_discontinuity_df %>%
  mutate(data_group = "All enrollees")

plot_bw = 6

conditionalMean_bin <- bonus_dta %>%
  filter(between(score_centered, -plot_bw, plot_bw))%>%
  mutate(bin = cut(score_centered, breaks = plot_bw*2)
         ) %>% 
  group_by(bin) %>% 
  summarise(score_centered = mean(score_centered, na.rm = T), 
            Native = mean(Native, na.rm = T), 
            Plantation = mean(Plantation, na.rm = T),
            Forest = mean(Forest, na.rm = T),
            Pine = mean(Pine, na.rm = T),
            Eucalyptus = mean(Eucalyptus, na.rm = T))

se_show = T

Forest <- ggplot() +
  geom_point(data = conditionalMean_bin, aes(x = score_centered, y = Forest), size = 2.5, alpha = 0.7, color = "black") +
  geom_point(data = bonus_dta %>% filter(between(Forest, min(conditionalMean_bin$Forest), max(conditionalMean_bin$Forest))), aes(x = score_centered, y = Forest), size = .75, shape = 21, alpha = .5, color = palette$dark) +
  # Add a line based on a linear model
  geom_smooth(data = filter(bonus_dta, score_centered <= 0), aes(x = score_centered, y = Forest, color = Awarded), se = se_show, alpha = 0.2) +
  geom_smooth(data = filter(bonus_dta, score_centered > 0), aes(x = score_centered, y = Forest, color = Awarded), se = se_show, alpha = 0.2) +
  # Add a line based on conditional mean 
  geom_smooth(data = filter(bonus_dta, score_centered <= 0), aes(x = score_centered, y = Forest, color = Awarded), method = "lm", linetype = "dashed", se = F) +
  geom_smooth(data = filter(bonus_dta, score_centered > 0), aes(x = score_centered, y = Forest, color = Awarded), method = "lm", linetype = "dashed", se = F) +
  geom_vline(xintercept = 0) +
  labs(x = "Centered score", y = "Overall forest")+
  theme_minimal()+
  scale_color_manual(values = c(palette$blue, palette$red), guide="none")+
  xlim(-plot_bw, plot_bw)
Forest

Native <- ggplot() +
  geom_point(data = conditionalMean_bin, aes(x = score_centered, y = Native), size = 2.5, alpha = 0.7, color = "black") +
  geom_point(data = bonus_dta %>% filter(between(Native, min(conditionalMean_bin$Native), max(conditionalMean_bin$Native))), aes(x = score_centered, y = Native), size = .75, shape = 21, alpha = .5, color = palette$dark) +
  # Add a line based on a linear model
  geom_smooth(data = filter(bonus_dta, score_centered <= 0), aes(x = score_centered, y = Native, color = Awarded), se = se_show, alpha = 0.2) +
  geom_smooth(data = filter(bonus_dta, score_centered > 0), aes(x = score_centered, y = Native, color = Awarded), se = se_show, alpha = 0.2) +
  # Add a line based on conditional mean 
  geom_smooth(data = filter(bonus_dta, score_centered <= 0), aes(x = score_centered, y = Native, color = Awarded), method = "lm", linetype = "dashed", se = F) +
  geom_smooth(data = filter(bonus_dta, score_centered > 0), aes(x = score_centered, y = Native, color = Awarded), method = "lm", linetype = "dashed", se = F) +
  geom_vline(xintercept = 0) +
  labs(x = "Centered score", y = "Native")+
  theme_minimal()+
  scale_color_manual(values = c(palette$blue, palette$red), guide="none")+
  xlim(-plot_bw, plot_bw)
Native
  ggsave(filename = paste0(fig_dir, "/Native_condmean.png"), width = 7, height = 5, units='in')
  
Eucalyptus <- ggplot() +
  geom_point(data = conditionalMean_bin, aes(x = score_centered, y = Eucalyptus), size = 2.5, alpha = 0.7, color = "black") +
  geom_point(data = bonus_dta %>% filter(between(Eucalyptus, min(conditionalMean_bin$Eucalyptus), max(conditionalMean_bin$Eucalyptus))), aes(x = score_centered, y = Eucalyptus), size = .75, shape = 21, alpha = .5, color = palette$dark) +
    # Add a line based on a linear model
  geom_smooth(data = filter(bonus_dta, score_centered <= 0), aes(x = score_centered, y = Eucalyptus, color = Awarded), se = se_show, alpha = 0.2) +
  geom_smooth(data = filter(bonus_dta, score_centered > 0), aes(x = score_centered, y = Eucalyptus, color = Awarded), se = se_show, alpha = 0.2) +
    # Add a line based on conditional mean 
  geom_smooth(data = filter(bonus_dta, score_centered <= 0), aes(x = score_centered, y = Eucalyptus, color = Awarded), method = "lm", linetype = "dashed", se = F) +
  geom_smooth(data = filter(bonus_dta, score_centered > 0), aes(x = score_centered, y = Eucalyptus, color = Awarded), method = "lm", linetype = "dashed", se = F) +
  geom_vline(xintercept = 0) +
  labs(x = "Centered score", y = "Eucalyptus")+
  theme_minimal()+
  scale_color_manual(values = c(palette$blue, palette$red), guide="none")+
  xlim(-plot_bw, plot_bw)
Eucalyptus
ggsave(filename = paste0(fig_dir, "/Eucalyptus_condmean.png"), width = 7, height = 5, units='in')
  
Pine <- ggplot() +
  geom_point(data = conditionalMean_bin, aes(x = score_centered, y = Pine), size = 2.5, alpha = 0.7, color = "black") +
  geom_point(data = bonus_dta %>% filter(between(Pine, min(conditionalMean_bin$Pine), max(conditionalMean_bin$Pine))), aes(x = score_centered, y = Pine), size = .75, shape = 21, alpha = .5, color = palette$dark) +
  # Add a line based on a linear model
  geom_smooth(data = filter(bonus_dta, score_centered <= 0), aes(x = score_centered, y = Pine, color = Awarded), se = se_show, alpha = 0.2) +
  geom_smooth(data = filter(bonus_dta, score_centered > 0), aes(x = score_centered, y = Pine, color = Awarded), se = se_show, alpha = 0.2) +
  # Add a line based on conditional mean 
  geom_smooth(data = filter(bonus_dta, score_centered <= 0), aes(x = score_centered, y = Pine, color = Awarded), method = "lm", linetype = "dashed", se = F) +
  geom_smooth(data = filter(bonus_dta, score_centered > 0), aes(x = score_centered, y = Pine, color = Awarded), method = "lm", linetype = "dashed", se = F) +
  geom_vline(xintercept = 0) +
  labs(x = "Centered score", y = "Pine")+
  theme_minimal()+
  scale_color_manual(values = c(palette$blue, palette$red), guide="none")+
  xlim(-plot_bw, plot_bw)
Pine
ggsave(filename = paste0(fig_dir, "/Pine_condmean.png"), width = 7, height = 5, units='in')
  
  
  
  
main_outcomes <- c("Native", "Eucalyptus", "Pine", "Plantation", "Forest")

bw_options <- c("mserd") # MSE-optimal bandwidth

my_kernel = "triangular"

preferred_bw = "MSE-optimal"
preferred_method = "bias-corrected"

results <- data.frame()
for(dta in list(bonus_dta, neverbonus_dta, all_dta)){
  
  for(o in main_outcomes){
    
    
    this_df <- dta %>%
      rename(out = o)%>%
      drop_na(score_centered)
    
    this_data_group = this_df$data_group[1]
    
    my_covs<- cbind(
      this_df$years_since_contest
      , this_df$Trees_2000
      , this_df$Development_2000
      , this_df$Grassland_2000
      , this_df$Plantation_2001
      , this_df$Water_2000
      , this_df$Crop_2000
    )
    
    for(b in bw_options){
      
      
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      #### RDD w/ MSE optimal BW
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      rd <- rdrobust(y = this_df$out, x = this_df$score_centered, c = 0
                     , covs = my_covs
                     , bwselect = b
                     , kernel = my_kernel
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
        "bandwidth_method" = ifelse(b == "mserd", "MSE-optimal", b),
        "method" = c("conventional", "bias-corrected", "robust"),
        "data_group" = this_data_group
      )%>%
        rbind(results)
      
      
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      #### RDD w/ 2 x MSE optimal BW
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      rd2 <- rdrobust(y = this_df$out, x = this_df$score_centered, c = 0
                      , covs = my_covs
                      , h = rd_bw*2
                      , kernel = my_kernel
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
        "bandwidth_method" = paste0("2 x ", ifelse(b == "mserd", "MSE-optimal", b)),
        "method" = c("conventional", "bias-corrected", "robust"),
        "data_group" = this_data_group
      )%>%
        rbind(results)
      
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      #### Fuzzy RDD
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      if(this_data_group == "All enrollees"){
        
        this_df <- this_df %>%
          mutate(received_bonus = replace_na(received_bonus, 0))
        
        rd_fuzzy <- rdrobust(y = this_df$out, x = this_df$score_centered, c = 0
                             , covs = my_covs
                             , bwselect = b
                             , fuzzy = this_df$received_bonus
                             , kernel = my_kernel
        )
        rd_bw_fuzzy = rd_bw
        
        results <- data.frame(
          "outcome" = o,
          rd_fuzzy$coef,
          rd_fuzzy$se,
          rd_fuzzy$pv,
          "Obs" = sum(rd_fuzzy$N),
          "eff_obs" = sum(rd_fuzzy$N_h),
          "bandwidth" = rd_bw_fuzzy,
          "bandwidth_method" = ifelse(b == "mserd", "MSE-optimal", b),
          "method" = c("conventional", "bias-corrected", "robust"),
          "data_group" = "fuzzy"
        )%>%
          rbind(results)
        
      }
      
      
      
      
      
      
    }
    
  }
  
}


rd_results <- results %>%
  rename(estimate = 2,
         se = 3,
         pval = 4)

preferred_results <- rd_results %>%
  filter(bandwidth_method == preferred_bw,
         method == preferred_method
         )


source("analysis/schart.R")

table(rd_results$outcome)

bw_order <- c("MSE-optimal", "2 x MSE-optimal")
method_order <- c("conventional", "bias-corrected",  "robust")
spec_chart_order <- c("Native", "Plantation"
                      ,"Eucalyptus", "Pine"
                      )

spec_results <- rd_results %>%
  filter(data_group == "Received bonus")%>%
  group_by(outcome)%>%
  filter(outcome %in% spec_chart_order)%>%
  filter(bandwidth_method %in% bw_order)%>%
  arrange(match(outcome, spec_chart_order)
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
  select(estimate, se, everything(), - pval, - bandwidth, - Obs, - eff_obs, - data_group
         )%>%
  as.data.frame()


Labels = list("Method:" = method_order,
              "Bandwidth:" = bw_order)


png(filename = paste0(fig_dir, "/schart_main.png"), width = 7, height = 5.5, units='in', res = 400)
par(oma=c(1,0,1,1))

schart(rd_spec_results %>% select(-outcome)
       , 
       labels = Labels,
       index.est = 1, index.se=2, col.est = c("black", "royalblue"), col.est2=c("grey80","grey80"),
       ci= c(.9, .95),
       n = 6,
       highlight = preferred_rows, 
       bg.dot=c("black", "grey95", "white", "royalblue"),
       col.dot=c("black", "grey95", "white", "royalblue"),
       axes = FALSE
) # make some room at the bottom
text(x=3.5 , y=6100, "Native", col="black", font=2)
text(x=10.5 , y=6100, "Plantation", col="black", font=2)
text(x=17.5 , y=6100, "Eucalyptus", col="black", font=2)
text(x=24.5 , y=6100, "Pine", col="black", font=2)

# Close the png file
dev.off()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Spec chart for all groups for preferred specification
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pref_chart_outcomes <- c("Native", "Plantation")

pref_chart_groups <- unique(preferred_results$data_group)

Labels = list("Treatment group:" = pref_chart_groups)

preferred_spec_results <- preferred_results %>%
  group_by(outcome)%>%
  filter(outcome %in% pref_chart_outcomes)%>%
  arrange(match(outcome, pref_chart_outcomes),
          match(outcome, pref_chart_groups))%>%
  ungroup %>%
  mutate(true = TRUE
  )%>%
  pivot_wider(names_from = data_group, values_from = true, values_fill = FALSE)%>%
  select(estimate, se, everything(), - pval, - bandwidth, - Obs, - eff_obs
         , - bandwidth_method, - method
  )%>%
  as.data.frame()


png(filename = paste0(fig_dir, "/schart_preferred.png"), width = 6, height = 5, units='in', res = 400)
par(oma=c(1,0,1,1))

schart(preferred_spec_results %>% select(-outcome)
       , 
       labels = Labels,
       index.est = 1, index.se=2, col.est = c("black", "royalblue"), col.est2=c("grey80","grey80"),
       ci= c(.9, .95),
       n = 4,
       bg.dot=c("black", "grey95", "white", "royalblue"),
       col.dot=c("black", "grey95", "white", "royalblue"),
       axes = FALSE
) # make some room at the bottom
text(x=2.5 , y=5500, "Native", col="black", font=2)
text(x=7 , y=5500, "Plantation", col="black", font=2)

# Close the png file
dev.off()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Parametric RDD, interact years since contest with treatment
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Native_bw <- rdbwselect(y = bonus_dta$Native, x = bonus_dta$score_centered, c = 0
                        , covs = cbind(
                           this_df$Trees_2000
                          , this_df$Development_2000
                          , this_df$Grassland_2000
                          , this_df$Plantation_2001
                          , this_df$Water_2000
                          , this_df$Crop_2000
                        ))$bws[1]

Native_bonus_dta <- bonus_dta %>%
  mutate(Award = ifelse(Awarded*1 == 1 , 1, 0),
         Intensity = Superficie/area_ha)%>%
  filter(between(score_centered, -Native_bw, Native_bw))

RDD_parametric <- lm(Native ~ score_centered + Award + years_since_contest
                     +Trees_2000+Development_2000+Grassland_2000+Plantation_2001+Water_2000+Crop_2000
                     , data = Native_bonus_dta
                     )
summary(RDD_parametric)

RDD_parametric_contestyear <- lm(Native ~ score_centered + Award*years_since_contest
                     +Trees_2000+Development_2000+Grassland_2000+Plantation_2001+Water_2000+Crop_2000
                     , data = Native_bonus_dta)

summary(RDD_parametric_contestyear)

library(margins)
#cplot(RDD_parametric_contestyear, x = "years_since_contest", dx = "Award", what = "effect")
cdat <- cplot(RDD_parametric_contestyear, x = "years_since_contest", dx = "Award", what = "effect")

ggplot(cdat, aes(x = xvals, y = yvals))+
  geom_line(color = palette$red, linewidth = 2)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)+
  theme_minimal() +
  ylab("Effect of Award on Native Forest") + xlab("Years since contest")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Covariate continuity and pre-treatment plantation/native spec chart
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cov_outcomes <- c("Superficie", "Monto Solicitado", "area_ha", "Water"
                        , "Plantation_2001", "Forest_2001" 
                  , "Grassland/Ag", "Orchard/Ag", "Bare", "Shrubland", "Urban"
                        )

cov_results <- data.frame()
for(o in cov_outcomes){
  
  
  this_df <- bonus_dta %>%
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
    "bandwidth_method" = "MSE-optimal",
    "method" = c("conventional", "bias-corrected", "robust")
  )%>%
    rbind(cov_results)
}

cov_results <- cov_results %>%
  rename(estimate = 2,
         se = 3,
         pval = 4)

preferred_cov_results <- cov_results %>%
  filter(method == preferred_method)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

paper_outcomes_main <- c("Native", "Plantation", "Eucalyptus", "Pine")

untreat_mean <- score_discontinuity_df %>% 
  filter(Awarded == 0)%>% 
  select(paper_outcomes_main) %>% 
  summarize_at(vars(paper_outcomes_main), ~round(mean(.), digits = 2))

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
  rbind(untreat_mean)%>%
  as.data.frame()

row.names(paper_results_main) <- c("LATE", " ", "Optimal bandwidth", "Effective # obs.", "Dep. var. mean")

kbl(paper_results_main,
    #format = "latex",
    booktabs = T,
   # caption = "",
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