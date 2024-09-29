# R script for EiA version of"carob"

## ISSUES
# 1. DOI and much of the metadata is missing
# 2. Data reads are still unstable and user needs to have access
# 3. License is missing (CC-BY)?
# 4. Many valuable variables that need to be integrated still...
# 5. ...

carob_script <- function(path) {
   
   "
	SOME DESCRIPTION GOES HERE...

"
   
   uri <- "R8EQa4bVPWLKYBuG7m9Bjsh0"
   group <- "eia"
   
   dset <- data.frame(
      # Need to fill-in metadata...
      # carobiner::read_metadata(uri, path, group, major=2, minor=0),
      uri = carobiner::simple_uri(uri),
      dataset_id = uri,
      publication= NA,
      authors ="Mary Jane; John Doe",
      data_institute ="IITA",
      title = NA,
      group = group,
      license = 'Some license here...',
      carob_contributor = 'Cedric Ngakou',
      usecase_code= "USC009",
      usecase_name = 'GH-CerLeg-GAIP',
      activity = 'addon',
      treatment_vars= "none",
      response_vars= "none",
      project = 'Excellence in Agronomy ',
      data_type = "survey",
      carob_date="2024-09-28",
      notes= "yield for crop cashew and common bean are very low"
   )
   
   # Manually build path (this can be automated...)
   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("/home/jovyan/carob-eia/data/raw/eia/Ghana-GAIP-AddOn/", full.names = T))
   
   # Retrieve relevant file
   f <- ff[basename(ff) == "EiA_GAIP_Ghana_Addon_2024_beingcleaned.xlsx"]
   # Read relevant file
   
   ## Process geo data and some crop managment practices 
   r <- carobiner::read.excel(f,sheet = "EiA_AddOn_Full_Survey_GAIP")[-c(1),]
   names(r) <- gsub("_index", "index", names(r))
   d <- data.frame(
      country = r$country,
      on_farm = FALSE,
      is_survey = TRUE,
      adm1=r$admin_1,
      adm2=r$admin_2,
      location=r$village,
      longitude=as.numeric(r$longitude),
      latitude=as.numeric(r$latitude),
      currency=r$local_currency,
      trial_id =  ifelse(!is.na(r$barcodehousehold), r$barcodehousehold, r$barcodehousehold_1),
      #technology_use= r$use_case_technology, ## Not a carob variable
      fertilizer_amount= as.numeric(r$fertiliser_amount),
      fertilizer_type= r$fertiliser_type,
      irrigation_dates= r$Irrigation_months, 
      irrigation_method= r$irrigation_technique,
      irregation_source= r$Irrigation_source,
      irrigated= ifelse(grepl("No", r$land_irrigated), FALSE,
                        ifelse(grepl("Yes", r$land_irrigated), TRUE, NA)), 
      land_prep_method=r$tillage_power,
      index=r$index,
      fertilizer_constraint= r$constraint_fertilizers ## Not a carob variable
   )
   
   ## Adding farmer gender 
   rr <- carobiner::read.excel(f,sheet = "hh_members_details_repeat")[-c(1),]
   names(rr) <- gsub("_index", "index", names(rr))
   rr <- rr[, c("person_gender", "index")]
   colnames(rr) <- c("farmer_gender", "index") 
   d <- merge(d, rr, by= "index", all.x= TRUE) 
   
   ### Processing plot size data 
   r1 <- carobiner::read.excel(f,sheet = "hh_plots_repeat")[-c(1),]
   names(r1) <- gsub("_index", "index", names(r1))
   d1 <- data.frame(
      plot_area= ifelse(grepl("acres", r1$unitland), as.numeric(r1$plot_size)*4047,
                        ifelse(grepl("other", r1$unitland), (as.numeric(r1$plot_size)*4047)/2, as.numeric(r1$plot_size))),
      index= r1$index
   )
   
   d <- merge(d, d1, by="index", all.x=TRUE)
   
   ### Processing yield data
   
   r2 <- carobiner::read.excel(f,sheet = "crop_repeat")[-c(1),]
   names(r2) <- gsub("_index", "index", names(r2))
   d2 <- data.frame(
      crop=  tolower(r2$crop_label),
      season=r2$season_grown,
      yield= ifelse(grepl("sacks_100kg", r2$crop_yield_units), as.numeric(r2$crop_yield)*100,
                    ifelse(grepl("sacks_50kg", r2$crop_yield_units), as.numeric(r2$crop_yield)*50,
                     ifelse(grepl("tonnes", r2$crop_yield_units), as.numeric(r2$crop_yield)*1000,
                     ifelse(grepl("other", r2$crop_yield_units), as.numeric(r2$crop_yield)*120,
                     ifelse(grepl("oxcart_250_to_500_kg", r2$crop_yield_units), as.numeric(r2$crop_yield)*250, as.numeric(r2$crop_yield)))))) ,
      crop_system= r2$crop_intercrop, ## Not a carob variable
      crop_price= as.numeric(r2$crop_sold_price),
      crop_residue_type= r2$crop_residue_use,
      crop_residue_used= ifelse(is.na(r2$crop_residue_use), FALSE, TRUE),
      index=r2$index
      
   ) 
   
   d <- merge(d, d2, by="index", all.x=TRUE)
   
   ## Adding variety, disease, pest and previous crop 
   r3 <- carobiner::read.excel(f,sheet = "plot_information_repeat")[-c(1),]
   names(r3) <- gsub("_index", "index", names(r3))
   d3 <- data.frame(
      house_distance= as.numeric(r3$distance_min),
      variety= r3$variety,
      previous_crop= tolower(r3$previous_crop),
      intercrops= r3$inter_crop,
      planting_date= as.character(as.Date(as.numeric(r3$planting_date), origin= "1899-12-30")),
      #pest_species= r3$pest_name,
      #diseases= r3$disease_name,
      index= r3$index
      
   )
   
   d <- merge(d, d3, by="index", all.x=TRUE)
   
   ### Adding organic matter 
   r4 <- carobiner::read.excel(f,sheet = "organic_inputs_details_repeat")[-c(1),]
   names(r4) <- gsub("_index", "index", names(r4))
   d4 <- data.frame(
      OM_amount= as.numeric(r4$organic_input_amount),
      OM_type= ifelse(grepl("compost", r4$organic_id), "compost",
                      ifelse(grepl("manure", r4$organic_id), "farmyard manure",
                             ifelse(grepl("plantBiomass", r4$organic_id), "unknown", "none"))),
      OM_price= r4$organic_input_cost,
      index= r4$index
   )
   d4$OM_used <- ifelse(grepl("none", d4$OM_type), FALSE, TRUE)
   
   d <- merge(d, d4, by="index", all.x=TRUE)
   
   r5 <- carobiner::read.excel(f,sheet = "inorganic_inputs_details_repeat")[-c(1),]
   names(r5) <- gsub("_index", "index", names(r5))
   d5 <- data.frame(
      fertilizer_type= r5$inorganic_ids,
      fertilizer_amount= r5$inorganic_inputs_amount,
      fertilizer_price= r5$inorganic_inputs_costs, ## Total fertilizer cost 
      index= r5$index
      
   )  
   
   d <- merge(d, d5, by=c("index", "fertilizer_type", "fertilizer_amount"), all.x=TRUE)
   
   d$yield <- (d$yield/d$plot_area)*10000 #  kg/ha
   d$index <- NULL
   
   ## Fixing fertilizer type 
   p <- carobiner::fix_name(d$fertilizer_type)
   p <- gsub(" ","; ",p)
   p <- gsub("11|15|17|19|20|23_3S|25|20_3S|23", "", p)
   p <- gsub("other","none", p)
   p <- gsub(NA, "none", p)
   p <- gsub("ammonium_sulphate","DAS", p)
   p <- gsub("NPK; urea; NPK","NPK; urea", p)
   p <- gsub("NPK; NPK; DAS NPK","NPK; DAS", p)
   p <- gsub("NPK; NPK; NPK; DAS","NPK; DAS", p)
   p <- gsub("NPK; NPK; DAS; NPK","NPK; DAS", p)
   p <- gsub("urea; NPK; none" ,"NPK; urea", p)
   p <- gsub("urea; NPK; NPK; DAS" ,"NPK; urea; DAS", p)
   p <- gsub("NPK; NPK; potassium_sulphate" ,"NPK; SOP", p)
   p <- gsub("potassium_sulphate" ,"SOP", p)
   d$fertilizer_type <- p
   
   ### Fixing crop 
   d$crop <- gsub("groundnuts", "groundnut", d$crop)
   d$crop <- gsub("bush beans", "common bean", d$crop)
   d$crop <- gsub("soybeans", "soybean", d$crop)
   d$crop <- gsub("cashew nuts", "cashew", d$crop)
   d$intercrops <- gsub("groundnuts", "groundnut", d$intercrops)
   d$intercrops <- gsub("no_answer", "none", d$intercrops)
   
   d$previous_crop <- gsub("groundnuts", "groundnut", d$previous_crop)
   d$previous_crop <- ifelse(grepl("bambara_groundnut", d$previous_crop), "bambara groundnut",
                             ifelse(grepl("no_answer|1|2|3", d$previous_crop), "none", d$previous_crop))
   ### 
   d$land_prep_method[grepl("2wheel_tractor|4wheel_tractor|mechanical|animal|manual", d$land_prep_method)] <- "conventional"
   d$land_prep_method[d$land_prep_method== "other"] <- "unknown"
   d$irrigation_method[grepl("bucket|hose|can", d$irrigation_method)] <- "furrow"
   
   
   ## Fixing longitude and latitude
   
   geo <- data.frame(
      location= c("Dangi", "Gbal", "Pieng", "Buo", "Fatchu", "Puzene","Kuroboi","Jitong","Gwollu","Wortuule","jitong"),
      lat= c(10.8091, 10.93808, 10.732335, 10.9683, 10.95958, 10.99051, 10.61009, 10.97781, 10.980324, 10.69137, 10.97781),
      lon= c(-2.04531, -2.332172, -1.96258, -2.505060, -2.3940022, -2.742745, -1.92689, -2.13930, -2.216948, -0.85402, -1.92689)
      
   ) 
   
   d <- merge(d, geo, by="location", all.x=TRUE)
   
   d$latitude[which(d$location %in% geo$location)] <- d$lat[which(d$location %in% geo$location)]
   d$longitude[which(d$location %in% geo$location)] <- d$lon[which(d$location %in% geo$location)]
   
   d$lat <- d$lon <- NULL
   
   ### Fixing Irrigation date
   
   d$irrigation_dates <- gsub(" ", "; ", d$irrigation_dates)
   d$irrigation_dates <- gsub("jan", "2024-01", d$irrigation_dates)
   d$irrigation_dates <- gsub("feb", "2024-02", d$irrigation_dates)
   d$irrigation_dates <- gsub("mar", "2023-03", d$irrigation_dates)
   d$irrigation_dates <- gsub("apr", "2023-04", d$irrigation_dates)
   d$irrigation_dates <- gsub("may", "2023-05", d$irrigation_dates)
   d$irrigation_dates <- gsub("jun", "2023-06", d$irrigation_dates)
   d$irrigation_dates <- gsub("jul", "2023-07", d$irrigation_dates)
   d$irrigation_dates <- gsub("sep", "2023-09", d$irrigation_dates)
   d$irrigation_dates <- gsub("oct", "2023-10", d$irrigation_dates)
   d$irrigation_dates <- gsub("nov", "2023-11", d$irrigation_dates)
   d$irrigation_dates <- gsub("dec", "2023-12", d$irrigation_dates)
   
   
   carobiner::write_files(dset, d, path=path)
}

#carob_script(path)
