library(tidyverse)  # ggplot(), %>%, mutate(), and friends
library(broom)  # Convert models to data frames
library(rdrobust)  # For robust nonparametric regression discontinuity
library(estimatr)  # Run 2SLS models in one step with iv_robust()
library(modelsummary)  # Create side-by-side regression tables
library(kableExtra)  # Fancy table formatting
library(ggplot2)
library(rddensity)
library(bunchr)

clean_data_dir <- here::here("data_prep", "clean")

discontinuity_all <- readRDS(paste0(clean_data_dir, "/discontinuity_all.rds"))
#################################################################################################
### table shows compliance across the threshold for all properties
#################################################################################################
discontinuity_all %>%
  group_by(rptpro_tipo_concurso, propertysize_reported <= size_cutoff) %>% 
  summarize(count = n()) %>% 
  group_by(rptpro_tipo_concurso) %>% 
  mutate(prop = count / sum(count))

regions_200 <- c(5,6,7,8,9, 10,14)
regions_500 <- c(1, 2, 3, 4, 15)
regions_800 <- c(11, 12)

discontinuity_all200 <- subset(discontinuity_all, rptpro_numero_region %in% regions_200)
discontinuity_all800 <- subset(discontinuity_all, rptpro_numero_region %in% regions_800)

reported_200 <- subset(discontinuity_all, between(propertysize_reported, 198, 200))%>%
  select(rptpro_id, rptpro_ano, propertysize_reported, rptpro_tipo_presenta, rptpro_tipo_concurso)


#################################################################################################
### Density tests show that there is manipulation at the 200 hectare threshold
#################################################################################################

test_density <- rddensity(discontinuity_all200$propertysize_reported, c = 200)
summary(test_density)
rdplotdensity(rdd = test_density, 
                                   X = discontinuity_all200$propertysize_reported,
                                   type = "both",
              #title = "",
              xlabel = "reported property size",
              ylabel = "density")  # This adds both points and lines


count_with_bins200 <- discontinuity_all200 %>%
  mutate(size_binned = cut(propertysize_reported, breaks = seq(0, 500, 5)),
         smallholder = ifelse(rptpro_tipo_concurso == "Otros Interesados", FALSE, TRUE)) %>% 
  # Group by each of the new bins and tutoring status
  group_by(size_binned) %>% 
  # Count how many people are in each test bin + used/didn't use tutoring
  summarize(n = n())%>%
  mutate(bin_end = as.numeric(size_binned)*5)%>%
  drop_na(size_binned)%>%
  filter(between(bin_end, 125, 275))

ggplot(count_with_bins200, aes(x = bin_end, y = n)) +
  geom_point(fill = "grey90") +
  geom_line() +
  geom_vline(xintercept = 202.5)+
  labs(x = "reported property size", y = "Proportion of properties designated as smallholders")+
  theme_minimal()

ggplot(data = discontinuity_all200) +
  geom_histogram(aes(propertysize_reported), binwidth = 1, alpha = .7 ,fill = "#E69F00", color = "white", size = 1, boundary = 0)+
  geom_vline(xintercept = 200, linetype = "dashed", size = 1.25)+
  #scale_fill_manual(values=c("grey20", "grey60")) + 
  theme_minimal()+
  xlim(190, 210)

#################################################################################################
### Showing that smallholder probability changes across thresholds
#################################################################################################

discontinuity_with_bins <- discontinuity_all %>% 
  mutate(size_binned = cut(size_centered, breaks = seq(-250, 250, 25)),
         smallholder = ifelse(rptpro_tipo_concurso == "Otros Interesados", FALSE, TRUE)) %>% 
  # Group by each of the new bins and tutoring status
  group_by(size_binned, smallholder) %>% 
  # Count how many people are in each test bin + used/didn't use tutoring
  summarize(n = n()) %>% 
  # Make this summarized data wider so that there's a column for tutoring and no tutoring
  pivot_wider(names_from = "smallholder", values_from = "n", values_fill = 0) %>% 
  rename(small_yes = `TRUE`, small_no = `FALSE`) %>% 
  # Find the probability of tutoring in each bin by taking 
  # the count of yes / count of yes + count of no
  mutate(prob_smallholder = small_yes / (small_yes + small_no),
         bin_end = as.numeric(size_binned)*25 -250 - 12.5,
         below_cutoff = bin_end <= 0)%>%
  drop_na(size_binned)

# Plot this puppy
ggplot(discontinuity_with_bins, aes(x = bin_end, y = prob_smallholder, fill = below_cutoff)) +
  geom_col(color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed")+
  labs(x = "centered property size", y = "Proportion of properties designated as smallholders")+
  theme_minimal()

## We see something similar for the 800 hectare regions

discontinuity_with_bins200 <- discontinuity_all200 %>% 
  mutate(size_binned = cut(propertysize_reported, breaks = seq(0, 500, 25)),
         smallholder = ifelse(rptpro_tipo_concurso == "Otros Interesados", FALSE, TRUE)) %>% 
  # Group by each of the new bins and tutoring status
  group_by(size_binned, smallholder) %>% 
  # Count how many people are in each test bin + used/didn't use tutoring
  summarize(n = n()) %>% 
  # Make this summarized data wider so that there's a column for tutoring and no tutoring
  pivot_wider(names_from = "smallholder", values_from = "n", values_fill = 0) %>% 
  rename(small_yes = `TRUE`, small_no = `FALSE`) %>% 
  # Find the probability of tutoring in each bin by taking 
  # the count of yes / count of yes + count of no
  mutate(prob_smallholder = small_yes / (small_yes + small_no),
         bin_end = as.numeric(size_binned)*25 -12.5,
         below_cutoff = bin_end <= 200)%>%
  drop_na(size_binned)

# Plot this puppy
ggplot(discontinuity_with_bins200, aes(x = bin_end, y = prob_smallholder, fill = below_cutoff)) +
  geom_col() +
  geom_vline(xintercept = 200, linetype = "dashed")+
  labs(x = "reported property size", y = "Proportion of properties designated as smallholders")+
  theme_minimal()


#################################################################################################
### characteristics jump at heaps
#################################################################################################
#heaps <- c(10, 20, 50, 100, 150, 250)
heaps <- c(seq(from = 10, to = 50, by = 10), 70, 100, 150, 200, 300, 400, 500)

# outcomes:
# received_bonus
# rptpro_monto_total
# rptpro_superficie
# rptpro_puntaje
# timber


analysis_df <- discontinuity_all %>%
  mutate(regeneration = (regeneracion ) > 0,
         planting = (`siembra-directa`
                     + plantacion + `plantacion-suplementaria`
                     + enriquecimiento
         ) > 0,
         digital = ifelse(rptpro_tipo_presentacion == "Digital", 1, 0),
         reforestation = ((regeneration + planting) > 0)*1,
         cutting = (`corta-liberacion` + `corta-mejoramiento` + `corta-recuperacion` +`corta-regeneracion` + `corta-selectiva` + `corta-sanitaria`) > 0,
         timber = ifelse(rptpro_objetivo_manejo == "PRODUCCION MADERERA", 1, 0),
         ecological_recovery = ifelse(rptpro_objetivo_manejo != "PRODUCCION MADERERA" & rptpro_objetivo_manejo != "PRODUCCION NO MADERERA", 1, 0),
         nontimber = ifelse(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA", 1, 0),
         extensionist_consultant = ifelse(rptpro_tipo_presenta == "Extensionista" | rptpro_tipo_presenta == "Consultor", 1, 0)
  )


heaps_results <- data.frame()
for(i in heaps){
  
bw_side = ifelse(i < 100, 9.99, 
                 ifelse(i >= 200, 99.99, 49.99)
                 )  
  
df_heap <- analysis_df %>%
  mutate(R_Z = propertysize_reported - i,
         heap = ifelse(propertysize_reported == i, 1, 0)) %>%
  filter(between(propertysize_reported, i - bw_side, i + bw_side))
reg <- as.data.frame(summary(lm(extensionist_consultant ~ heap + R_Z, data = df_heap))$coefficients) %>%
  rownames_to_column()%>%
  filter(rowname == "heap")

heaps_results <- data.frame("heap_point" = i, "coeff" = reg$Estimate, "se" = reg$`Std. Error`, "p_value" = reg$`Pr(>|t|)`)%>%
  rbind(heaps_results)

}

ggplot(data = heaps_results, aes(x = heap_point, y = coeff ))+
  geom_point(color = "blue")+
  geom_errorbar(aes(ymin = coeff - 1.96*se, ymax = coeff + 1.96*se), width = .5)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_x_continuous(breaks = heaps)+
  ylab("estimate") + xlab("heap point")+
  ggtitle("used extensionist or consultant")+
  theme_minimal()

ggsave(path = "figs", filename = "heap_covars_extensionista2.png", width = 7, height = 5)

#################################################################################################
### Implementing fuzzy rdd
#################################################################################################

# Now we have a new column named below_cutoff that we’ll use as an instrument. Most of the time this will be the same as the contest column, since most people are compliers. But some people didn’t comply,

library(estimatr)

#################################################################################################
### parametric fuzzy rdd w/ received_bonus outcome
#################################################################################################

analysis_df <- discontinuity_all200 %>%
  mutate(regeneration = (regeneracion ) > 0,
         planting = (`siembra-directa`
                     + plantacion + `plantacion-suplementaria`
                      + enriquecimiento
                          ) > 0,
         reforest = (regeneration + planting) > 0,
         cutting = (`corta-liberacion` + `corta-mejoramiento` + `corta-recuperacion` +`corta-regeneracion` + `corta-selectiva` + `corta-sanitaria`) > 0,
         timber = ifelse(rptpro_objetivo_manejo == "PRODUCCION MADERERA", 1, 0),
         ecological_recovery = ifelse(rptpro_objetivo_manejo != "PRODUCCION MADERERA" & rptpro_objetivo_manejo != "PRODUCCION NO MADERERA", 1, 0),
         nontimber = ifelse(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA", 1, 0),
         extensionist_consultant = ifelse(rptpro_tipo_presenta == "Extensionista" | rptpro_tipo_presenta == "Consultor", 1, 0)
  )

bw_list = c(120, 80, 60)
heaps <- c(seq(from = 10, to = 50, by = 10), 70, 100, 150, 200)#, 300, 400, 500, 600, 700, 800)

outcomes <- c("proportion_bonusarea"
,"rptpro_monto_total"
,"timber"
,"received_bonus"
)

rdd_results <- data.frame()

for(outcome in outcomes){

for(k in bw_list){
    
  donut = 3
  
    
    donut_df <- analysis_df %>%
      mutate(drop = ifelse(between(size_centered, -donut, 0), 1, 0)
             )%>%
      filter(drop != 1)
      
      
    bw = k 
    
    #proportion_bonusarea
    #rptpro_monto_total
    #timber
    #received_bonus
    
    form = as.formula(paste0(outcome, " ~ size_centered  + `Contest type` | size_centered  + below_cutoff"))

    rdd <- iv_robust(
      form ,
     # fixed_effects = ~ rptpre_region ,
      #diagnostics = TRUE,
      data = filter(donut_df, size_centered >= -bw - donut & size_centered <= bw )
    )
    
    rdd
    
    rdd_results <- data.frame(
      "outcome" = rdd$outcome, "coeff" = rdd$coefficients["`Contest type`Smallholder"], "se" = rdd$std.error["`Contest type`Smallholder"], "p.val" = rdd$p.value["`Contest type`Smallholder"],
      "bw" = k, "donut" = donut
    )%>%
      rbind(rdd_results)
    
}
}

library(rio)
# export(rdd_results, "rdresults_main.rds")


  