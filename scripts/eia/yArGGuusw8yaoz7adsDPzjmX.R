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
   
  uri <- "yArGGuusw8yaoz7adsDPzjmX"
  group <- "eia"
  
   meta <- data.frame(
      # Need to fill-in metadata...
      # carobiner::read_metadata(uri, path, group, major=2, minor=0),
      uri = uri,
      dataset_id = uri,
      data_institute = "IRRI",
      authors = "Rica Flor",
      title = "DSRC Cambodia Use Case Validations 2022-2023",
      description = "Data for the use case validaton of DSRC recommendations for Cambodia 2022-2023 seasons",
      group = group,
      license = 'Some license here...',
      carob_contributor = 'Eduardo Garcia Bendito',
      data_citation = '...',
      project = 'Excellence in Agronomy',
      usecase_code= "USC012",
      usecase_name = "SEA-DSRC-Cambodia",
      activity = "validation",
      treatment_vars = "treatment; season",
      response_vars = "yield",
      data_type = "on-farm experiment", 
      carob_date="2024-07-31"
   )
   
   # Manually build path (this can be automated...)
   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("~/carob-eia/data/raw/eia/Cambodia-DSRC-Validation/", full.names = T))
   
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
     is_survey = FALSE,
     irrigated = FALSE,
     country = "Cambodia",
     adm1 = r1$province,
     adm2 = r1$district,
     location = r1$commune,
     longitude = NA,
     latitude = NA,
     geo_from_source = FALSE,
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
     yield_part = "seed",
     yield = r1$crop_cut_kgha
     )
   
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
     yield_part = "seed",
     yield = r2$crop_cut_kgha)
   
   d <- carobiner::bindr(d1, d2)
   
   d$adm1[grep("thom", d$adm1)] <- "Kampong Thom"
   d$adm1[grep("battambang", d$adm1)] <- "Battambang"
   d$adm1[grep("veng", d$adm1)] <- "Prey Veng"
   d$adm1[grep("takeo", d$adm1)] <- "Takeo"
   
   d$longitude[d$location == "anlongrun"] <- 102.96
   d$latitude[d$location == "anlongrun"] <- 13.158
   d$longitude[d$location == "preykuy"] <- 104.915
   d$latitude[d$location == "preykuy"] <- 12.70
   d$longitude[d$location == "chroab"] <- 104.92
   d$latitude[d$location == "chroab"] <- 12.601
   d$longitude[d$location == "tboungkrapeu"] <- 105.612
   d$latitude[d$location == "tboungkrapeu"] <- 12.721
   d$longitude[d$location == "theay"] <- 105.38
   d$latitude[d$location == "theay"] <- 11.365
   d$longitude[d$location == "babaong"] <- 105.31
   d$latitude[d$location == "babaong"] <- 11.392
   d$longitude[d$location == "bankam"] <- 104.945
   d$latitude[d$location == "bankam"] <- 11.129
   d$longitude[d$location == "cheungkuon"] <- 104.819
   d$latitude[d$location == "cheungkuon"] <- 11.128
   d$longitude[d$location == "KompongPreang"] <- 103.338
   d$latitude[d$location == "KompongPreang"] <- 12.963
   d$longitude[d$location == "RangKeSey"] <- 103.246
   d$latitude[d$location == "RangKeSey"] <- 12.968
   d$longitude[d$location == "Preykandiang"] <- 105.34
   d$latitude[d$location == "Preykandiang"] <- 11.419
   d$longitude[d$location == "Commune"] <- 105.3
   d$latitude[d$location == "Commune"] <- 11.321
   d$longitude[d$site == "Svayteap"] <- 105.88
   d$latitude[d$site == "Svayteap"] <- 11.092
   d$longitude[d$adm1 == "Takeo"] <- 104.677
   d$latitude[d$adm1 == "Takeo"] <- 10.949
   d$longitude[d$site == "SLa"] <- 104.81
   d$latitude[d$site == "SLa"] <- 11.164
   d$longitude[d$location == "Osaray"] <- 104.55
   d$latitude[d$location == "Osaray"] <- 10.912
   d$longitude[d$site == "La'ak"] <- 104.603
   d$latitude[d$site == "La'ak"] <- 11.336
   d$longitude[d$site == "Tropang Pring"] <- 105.942
   d$latitude[d$site == "Tropang Pring"] <- 12.077
   d$longitude[grep("Sangke", d$adm2, ignore.case = T)] <- 103.23
   d$latitude[grep("Sangke", d$adm2, ignore.case = T)] <- 13.08
   d$longitude[grep("KonTie 1", d$adm2, ignore.case = T)] <- 102.88
   d$latitude[grep("KonTie 1", d$adm2, ignore.case = T)] <- 12.951
   
   carobiner::write_files(meta, d, path=path)
}
