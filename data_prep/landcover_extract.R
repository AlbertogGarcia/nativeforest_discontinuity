library(tidyverse)
library(terra)
library(sf)
library(rio)

my_data_dir <- here::here("remote")

clean_data_dir <- here::here("data_prep", "clean")

landcover_2021_dir <- "C:\\Users\\AG2964\\Dropbox (YSE)\\chile_landcover"
#landcover_2021_dir <- here::here("chile_landcover")

select <- dplyr::select

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Read in 2021 landcover raster
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
landcover_2021_rast <- terra::rast(paste0(landcover_2021_dir, "/2021_chile_mosaic-001.tif"))

landcover_2021_class_number <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
landcover_2021_class_name <- c("Urban", "Water", "Grassland/Ag", "Pine", "Eucalyptus", "Native", "Bare", "Shrubland", "Orchard/Ag") 

graesser_2000_rast <- terra::rast(paste0(my_data_dir, "/data/graesser_lc/AnnualMosaics/Chile_p1and2_2000.tif"))

graesser_class_number <- c(1, 111, 124, 131, 138, 144, 152, 176, 195)
graesser_class_name <- c("Crop", "Water", "Development", "Bare", "Plantations", "Trees", "Shrubs", "Grassland", "Wetland") 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Extract landcover for rejected properties
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_landcover <- function(sf.obj, terra_rast, class_number, class_name, append_col, id_cols, return_pix = TRUE){
  
  # Convert sf.obj to spatvector and extract land cover
  temp <- terra::extract(
    terra_rast,
    terra::vect(
      sf.obj %>% st_transform(terra::crs(terra_rast))
    )
    ,
    na.rm=TRUE,
    weights = T
  )
  
  old_colname <- colnames(temp %>% select(2))
  
  append_term = ifelse(append_col == "", "", paste0("_", append_col))
  
  temp2 <- temp %>%
    rename(class = old_colname)%>%
    group_by(ID, class)%>%
    summarize(class_coverage = sum(weight, na.rm = T)) %>% #get total number of pixels from each class
    mutate(class = c(class_name, class)[match(class, c(class_number, class))],
           class = paste0(class, append_term)# paste year onto name of land cover class
           )%>% 
    pivot_wider(names_from = class, values_from = class_coverage)%>% 
    ungroup()
  
  if(return_pix == TRUE){
  temp2$pixels_count = rowSums(dplyr::select(temp2, -ID), na.rm = T)
  }
  
  return <- temp2%>%
    right_join(
      sf.obj %>% rowid_to_column("ID") %>% st_drop_geometry() %>% dplyr::select(ID, id_cols)
      , by = "ID")%>%
    replace(is.na(.), 0)
  
  return(return)
  

}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
enrolled_matched_rol <- read_sf(paste0(clean_data_dir, "/enrolled_match_rol.shp"))


lee_enrolled_rol <- extract_landcover(enrolled_matched_rol, 
                                            landcover_2021_rast, landcover_2021_class_number, landcover_2021_class_name, 
                                      append_col = "",
                                            id_cols = c("rptpro_id", "rptpre_id", "ROL", "rptpro_ano"))

graesser_enrolled_rol <- extract_landcover(enrolled_matched_rol, 
                                           graesser_2000_rast, graesser_class_number, graesser_class_name, 
                                            2000,
                                            id_cols = c("rptpro_id", "rptpre_id", "ROL", "rptpro_ano"), return_pix = FALSE)

extracted_enrolled_rol <- lee_enrolled_rol %>%
  left_join(graesser_enrolled_rol, by = c("ID", "rptpro_id", "rptpre_id", "ROL", "rptpro_ano"))

export(extracted_enrolled_rol, paste0(clean_data_dir, "/landcover_enrolled_rol.rds"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
enrolled_matched_main <- read_sf(paste0(clean_data_dir, "/enrolled_match_main.shp"))


lee_enrolled <- extract_landcover(enrolled_matched_main, 
                                      landcover_2021_rast, landcover_2021_class_number, landcover_2021_class_name, 
                                  append_col = "",
                                      id_cols = c("rptpro_id", "rptpre_id", "rptpro_ano", "polyarea"))

graesser_enrolled <- extract_landcover(enrolled_matched_main, 
                                           graesser_2000_rast, graesser_class_number, graesser_class_name, 
                                           2000,
                                           id_cols = c("rptpro_id", "rptpre_id", "rptpro_ano", "polyarea"), return_pix = FALSE)

extracted_enrolled <- lee_enrolled %>%
  left_join(graesser_enrolled, by = c("ID", "rptpro_id", "rptpre_id", "rptpro_ano", "polyarea"))

export(extracted_enrolled, paste0(clean_data_dir, "/landcover_enrolled.rds"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
no_asignados_matched <- read_sf(paste0(clean_data_dir, "/no_asignados_match_rol.shp"))


lee_rejected <- extract_landcover(no_asignados_matched %>% rename(ano_concurso = `AñCncrs`), 
                                      landcover_2021_rast, landcover_2021_class_number, landcover_2021_class_name, 
                                  append_col = "",
                                      id_cols = c("Postlcn", "ROL", "Comuna", "ano_concurso"))

graesser_rejected <- extract_landcover(no_asignados_matched %>% rename(ano_concurso = `AñCncrs`), 
                                           graesser_2000_rast, graesser_class_number, graesser_class_name, 
                                           2000,
                                           id_cols = c("Postlcn", "ROL", "Comuna", "ano_concurso"), return_pix = FALSE)

extracted_rejected <- lee_rejected %>%
  left_join(graesser_rejected, by = c("ID", "Postlcn", "ROL", "Comuna", "ano_concurso"))

export(extracted_rejected, paste0(clean_data_dir, "/landcover_rejected.rds"))
