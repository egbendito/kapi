# R script for EiA version of"carob"

## ISSUES
# 1. DOI and much of the metadata is missing
# 2. License is missing (CC-BY)?
# 3. Many valuable variables that need to be integrated still...
# 4. ...

carob_script <- function(path) {
   
   "
	SOME DESCRIPTION GOES HERE...

"
   
   uri <- "doi:Cambodia-DSRC-Validation"
   group <- "eia"
   
   dset <- data.frame(
      # Need to fill-in metadata...
      # carobiner::read_metadata(uri, path, group, major=2, minor=0),
      uri = carobiner::simple_uri(uri),
      dataset_id = uri,
      data_institutions = "CGIAR - IRRI",
      authors = "Rica Flor",
      title = "DSRC Cambodia Use Case Validations 2022-2023",
      description = "Data for the use case validaton of DSRC recommendations for Cambodia 2022-2023 seasons",
      group = group,
      license = 'Some license here...',
      carob_contributor = 'Eduardo Garcia Bendito',
      data_citation = '...',
      project = 'Excellence in Agronomy - DSRC Cambodia Validation',
      data_type = "on-farm experiment", # or, e.g. "on-farm experiment", "survey", "compilation"
      carob_date="2024-07-31"
   )
   
   # Manually build path (this can be automated...)
   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("/home/jovyan/carob-eia/data/raw/eia/Cambodia-DSRC-Validation/", full.names = T))
   
   # Retrieve relevant file
   f1 <- ff[basename(ff) == "EiA_farmerdiary_2022-23DS_rawdata_anonymized.O.xlsx"]
   f2 <- ff[basename(ff) == "EiA_farmerdiary_2022WS_rawdata_anonymized.O.xlsx"]
   
   # Read relevant file
   r1 <- carobiner::read.excel(f1, sheet = "2022-23DS", skip = 1)
   r2 <- carobiner::read.excel(f2, sheet = "2022WS")
   
   # Build initial DF ... Start from here
   d1 <- data.frame(
     trial_id = rep(1:(nrow(r1)/2), each = 2),
     treatment = ifelse(r1$experiment == "demotrial", "Direct Seeding and Best Agronomic Practices", "Farmers Practices"),
     on_farm = TRUE,
     country = "Cambodia",
     adm1 = r1$province,
     adm2 = r1$district,
     location = r1$commune, 
     site = r1$village,
     crop = "rice",
     variety = r1$variety,
     plot_area = r1$area_ha*10000, # We are converting to m2
     seed_source = r1$seed_source,
     # land_prep_method	= paste(ifelse(r1$tillage_no > 0, "conventional", ""),)
     season = "dry",
     planting_date = as.character(as.Date(r1$date_sowing)),
     fertilizer_type = gsub(";$", "", paste0(ifelse(!is.na(r1$fert_basal_urea_kg), "urea;", ""),
                                             ifelse(!is.na(r1$fert_basal_ammoniumsulphate_kg), "DAS;", ""),
                                             ifelse(!is.na(r1$fert_basal_dap_kg), "DAP;", ""),
                                             ifelse(!is.na(r1$fert_basal_gypsum_kg), "gypsum;", ""),
                                             ifelse(!is.na(r1$fert_basal_tsp_kg), "TSP;", ""),
                                             ifelse(!is.na(r1$fert_basal_potash_kg), "KCl;", ""),
                                             ifelse(!is.na(r1$fert_basal_sulphur_kg), "SOP;", ""))),
     N_fertilizer = ifelse(!is.na(r1$fert_basal_urea_kg), r1$fert_basal_urea_kg*0.46, 0) + 
       ifelse(!is.na(r1$fert_basal_ammoniumsulphate_kg), r1$fert_basal_ammoniumsulphate_kg*0.21, 0) +
       ifelse(!is.na(r1$fert_basal_dap_kg), r1$fert_basal_dap_kg*0.18, 0),
     P_fertilizer = ifelse(!is.na(r1$fert_basal_dap_kg), r1$fert_basal_dap_kg*0.21, 0) + 
       ifelse(!is.na(r1$fert_basal_tsp_kg), r1$fert_basal_tsp_kg*0.1923, 0),
     K_fertilizer = ifelse(!is.na(r1$fert_basal_potash_kg), r1$fert_basal_potash_kg*0.498, 0),
     yield = r1$crop_cut_kgha)
   
   d2 <- data.frame(
     trial_id = r2$hhid,
     treatment = ifelse(r2$treatment == "mDSR+BAP", "Direct Seeding and Best Agronomic Practices", "Farmers Practices"),
     on_farm = TRUE,
     country = "Cambodia",
     adm1 = r2$province,
     adm2 = r2$district,
     location = r2$commune,
     site = r2$village,
     crop = "rice",
     variety = r2$variety,
     variety_code = r2$variety_other,
     plot_area = r2$area_ha*10000,
     seed_source = r2$seed_source,
     # land_prep_method	= paste(ifelse(r2$tillage_no > 0, "conventional", ""),)
     season = "wet",
     planting_date = as.character(as.Date(r2$date_sowing)),
     fertilizer_type = gsub(";$", "", paste0(ifelse(r2$fert_basal_urea_yn == "yes", "urea;", ""),
                                             ifelse(r2$fert_basal_ammoniumsulphate_yn == "yes", "DAS;", ""),
                                             ifelse(r2$fert_basal_boron_yn == "yes", "H3BO3;", ""),
                                             ifelse(r2$fert_basal_dap_yn == "yes", "DAP;", ""),
                                             ifelse(r2$fert_basal_gypsum_yn == "yes", "gypsum;", ""),
                                             ifelse(r2$fert_basal_tsp_yn == "yes", "TSP;", ""),
                                             ifelse(r2$fert_basal_potash_yn == "yes", "KCl;", ""),
                                             ifelse(r2$fert_basal_npk_yn == "yes", "NPK;", ""),
                                             ifelse(r2$fert_basal_sulphur_yn == "yes", "SOP;", ""))),
     N_fertilizer = ifelse(r2$fert_basal_urea_yn == "yes", r2$fert_basal_urea_kg*0.46, 0) + 
       ifelse(r2$fert_basal_ammoniumsulphate_yn == "yes", r2$fert_basal_ammoniumsulphate_kg*0.21, 0) +
       ifelse(r2$fert_basal_dap_yn == "yes", r2$fert_basal_dap_kg*0.18, 0) +
       ifelse(r2$fert_basal_npk_yn == "yes", r2$fert_basal_npk_kg*0.18, 0), # Using standard NPK composition 18:8:4
     P_fertilizer = ifelse(r2$fert_basal_dap_yn == "yes", r2$fert_basal_dap_kg*0.21, 0) + 
       ifelse(r2$fert_basal_npk_yn == "yes", r2$fert_basal_npk_kg*0.08, 0) + # Using standard NPK composition 18:8:4
       ifelse(r2$fert_basal_tsp_yn == "yes", r2$fert_basal_tsp_kg*0.1923, 0),
     K_fertilizer = ifelse(r2$fert_basal_potash_yn == "yes", r2$fert_basal_potash_kg*0.498, 0) +
       ifelse(r2$fert_basal_npk_yn == "yes", r2$fert_basal_npk_kg*0.04, 0), # Using standard NPK composition 18:8:4
     yield = r2$crop_cut_kgha)
   
   d <- carobiner::bindr(d1, d2)
   
   d$adm1[grep("thom", d$adm1)] <- "Kampong Thom"
   d$adm1[grep("battambang", d$adm1)] <- "Battambang"
   d$adm1[grep("veng", d$adm1)] <- "Prey Veng"
   d$adm1[grep("takeo", d$adm1)] <- "Takeo"

   carobiner::write_files(dset, d, path=path)
}
