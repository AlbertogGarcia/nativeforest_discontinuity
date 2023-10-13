library(tidyverse)
library(ggplot2)
library(rdrobust)

clean_data_dir <- here::here("data_prep", "clean")

select <- dplyr::select

regions_200 <- c(5,6,7,8,9, 10,14)
regions_500 <- c(1, 2, 3, 4, 15)
regions_800 <- c(11, 12)

property_df <- readRDS(paste0(clean_data_dir, "/property_df.rds"))%>%
  filter(!duplicated(paste0(pmax(rptpro_id, rptpre_id, rptpro_ano, rptpro_puntaje, rptpre_comuna), pmin(rptpro_id, rptpre_id, rptpro_ano, rptpro_puntaje, rptpre_comuna))))%>%
  mutate(propertysize_reported = rptpre_superficie_predial,
         size_cutoff = ifelse(
  rptpro_numero_region %in% regions_200, 200,
  ifelse(rptpro_numero_region %in% regions_800, 800, 500)
  ),
size_centered = propertysize_reported - size_cutoff,
below_cutoff = propertysize_reported <= size_cutoff
)

landcover_enrolled <- readRDS(paste0(clean_data_dir, 
                                     "/landcover_enrolled.rds"
))%>%
  mutate(Plantation = Pine + Eucalyptus,
         Forest = Plantation + Native)

landcover_cols <- c("Forest", "Plantation", "Urban", "Water", "Grassland/Ag", "Pine", "Native", "Bare", "Shrubland", "Eucalyptus","Orchard/Ag" , "pixels_count")

enrolled_props <- landcover_enrolled %>%
  inner_join(property_df, by = c("rptpro_id", "rptpre_id", "rptpro_ano"))%>%
  rowid_to_column(var = "my_ID")%>%
  rename(psize_reported = rptpre_superficie_predial,
         region = rptpre_region,
         ano_concurso = rptpro_ano,
         Comuna = rptpre_comuna,
         objectivo_manejo = rptpro_objetivo_manejo)%>%
  select(size_centered, size_cutoff, below_cutoff, propertysize_reported, polyarea, my_ID, region, `Contest type`, objectivo_manejo, ano_concurso, Comuna, received_bonus, submitted_management_plan, 
         landcover_cols, matches("2000"))


ggplot(enrolled_props, aes(x = size_centered, y = Native, color = `Contest type`)) +
  geom_point(size = 1, alpha = 0.5) +
  # Add a line based on a linear model for the people scoring less than 70
  geom_smooth(data = filter(enrolled_props, size_centered <= 0), method = "lm") +
  # Add a line based on a linear model for the people scoring 70 or more
  geom_smooth(data = filter(enrolled_props, size_centered > 0), method = "lm") +
  geom_vline(xintercept = 0) +
  labs(x = "Property size", y = "Native", color = "Contest")+
  xlim(-100, 100)+ylim(0, 20000)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Fuzzy RDD using reported property size
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
analysis_df <- enrolled_props %>%
  filter(size_cutoff == 200)%>%
  filter(submitted_management_plan == 1)

outcomes <- c("Forest", "Eucalyptus", "Pine", "Native"
)


results <- data.frame()
for(o in outcomes){
  
  this_df <- analysis_df %>%
    rename(out = o)%>%
    mutate(smallholder = ifelse(`Contest type` == "Smallholder", 1, 0))
  
  
  my_covs<- cbind(
    this_df$Development_2000
    , this_df$Grassland_2000
    , this_df$Water_2000
    , this_df$Crop_2000
    , this_df$Trees_2000
  )
  
  rd <- rdrobust(y = this_df$out, x = this_df$size_centered, c = 0
                 , covs = my_covs
                 , fuzzy = this_df$smallholder
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
  
  
  rd2 <- rdrobust(y = this_df$out, x = this_df$size_centered, c = 0
                  , covs = my_covs
                  , h = rd_bw*2
                  , fuzzy = this_df$smallholder
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

spec_chart_outcomes <- c("Plantation", "Native")
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
text(x=3 , y=5300, "Plantation", col="black", font=2)
text(x=10 , y=5300, "Native", col="black", font=2)

